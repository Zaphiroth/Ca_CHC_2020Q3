# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Projection
# programmer:   Zhe Liu
# Date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
# universe PCHC
pchc.universe <- bind_rows(imp.total, pchc.universe1, pchc.universe2) %>% 
  group_by(pchc) %>% 
  summarise(province = first(province),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            est = first(na.omit(est))) %>% 
  ungroup() %>% 
  filter(!is.na(province), !is.na(city), !is.na(district), !is.na(est)) %>% 
  mutate(flag_sample = if_else(pchc %in% unique(raw.total$pchc), 1, 0))

# segment
seg <- read.xlsx("02_Inputs/seg_ca.xlsx") %>% 
  mutate(city_seg = if_else(city %in% c("上海", "广州"), 
                            stri_paste(city, district), city)) %>% 
  select(province, city_seg, seg)

# argument
argument <- read.xlsx("02_Inputs/七城市自变量_clean.xlsx") %>% 
  filter(`地级市` %in% kTargetCity) %>% 
  select(province = `省`, city = `地级市`, pchc = PCHC_Code, Est_DrugIncome_RMB) %>% 
  left_join(pchc.universe[, c("pchc", "district")], by = "pchc") %>% 
  mutate(city_seg = if_else(city %in% c("上海", "广州"), stri_paste(city, district), city),
         panel = if_else(pchc %in% unique(imp.total$pchc), 1, 0),
         panel = if_else(is.na(Est_DrugIncome_RMB), 0, panel),
         panel_all = panel) %>% 
  left_join(seg, by = c("province", "city_seg")) %>% 
  mutate(seg = if_else(is.na(seg) & city == "上海", 1, seg),
         seg = if_else(is.na(seg) & city == "广州", 6, seg),
         market = "CA")

# outlier
outlier <- read.xlsx("02_Inputs/outlier_arti.xlsx")


##---- Projection ----
# projection data
proj.raw <- imp.total %>% 
  group_by(year, quarter, province, city, district, pchc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(market = "CA", 
         city_seg = if_else(city %in% c("上海", "广州"), stri_paste(city, district), city)) %>% 
  left_join(seg, by = c("province", "city_seg")) %>% 
  mutate(seg = if_else(is.na(seg) & city == "上海", 1, seg), 
         seg = if_else(is.na(seg) & city == "广州", 6, seg), 
         seg = if_else(is.na(seg), 1, seg))

proj.data <- proj.raw %>% 
  group_by(quarter, pchc, market, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(pchc))

# proj.price <- proj.raw %>% 
#   group_by(quarter, packid) %>% 
#   summarise(price = sum(sales, na.rm = TRUE) / sum(units, na.rm = TRUE)) %>% 
#   ungroup()

# proj.market <- proj.raw %>% 
#   group_by(year, pchc, market) %>% 
#   summarise(panel_sales = sum(sales, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   right_join(argument, by = "pchc") %>% 
#   filter(!(pchc %in% outlier$PCHC))

# model data
model.outlier1 <- read.xlsx("02_Inputs/model1/outlier.xlsx")
model.factor1 <- read.xlsx("02_Inputs/model1/factor.xlsx")

model.outlier2 <- read.xlsx("02_Inputs/model2/outlier.xlsx")
model.factor2 <- read.xlsx("02_Inputs/model2/factor.xlsx")

model.outlier <- bind_rows(model.outlier1, model.outlier2) %>% 
  select(pchc = mapping, market = mkt) %>% 
  mutate(flag = 1)

model.factor <- bind_rows(model.factor1, model.factor2) %>% 
  select(market = mkt, seg, factor)

# projection
universe.set <- merge(distinct(argument, pchc), distinct(proj.data, quarter)) %>% 
  merge(distinct(proj.raw, market, packid)) %>% 
  left_join(argument, by = c("pchc", "market")) %>% 
  left_join(model.outlier, by = c("pchc", "market")) %>% 
  mutate(panel = if_else(pchc %in% outlier$PCHC, 0, panel),
         panel = if_else(!is.na(flag), 0, panel))

universe.sales <- universe.set %>% 
  filter(panel == 1) %>% 
  left_join(proj.data, by = c("quarter", "pchc", "market", "packid")) %>% 
  mutate(sales = if_else(is.na(sales), 0, sales))

proj.parm <- data.table(universe.sales)[, 
                                        {
                                          ux <- mean(Est_DrugIncome_RMB)
                                          uy <- mean(sales)
                                          slope <- uy / ux
                                          intercept = 0
                                          predict_sales = slope*Est_DrugIncome_RMB
                                          spearman_cor = cor(sales, predict_sales, method = 'spearman')
                                          list(slope = slope, intercept = intercept, spearman_cor = spearman_cor)
                                        }
                                        , by = list(quarter, packid ,market, seg)
                                        ]

universe.sales.total <- universe.set %>% 
  left_join(proj.data, by = c("quarter", "pchc", "market", "packid")) %>% 
  mutate(sales = if_else(is.na(sales), 0, sales))

# QC
sum(universe.sales.total$sales, na.rm = TRUE) <= sum(proj.data$sales, na.rm = TRUE)

# result
proj.total <- universe.sales.total %>% 
  left_join(proj.parm, by = c("quarter", "market", "packid", "seg")) %>% 
  mutate(predict_sales = Est_DrugIncome_RMB * slope + intercept,
         predict_sales = if_else(predict_sales < 0, 0, predict_sales)) %>% 
  left_join(model.factor, by = c("market", "seg")) %>% 
  mutate(final_sales = if_else(panel_all == 0, predict_sales * factor, sales),
         final_sales = if_else(panel_all == 0 & is.na(Est_DrugIncome_RMB) & sales != 0, 
                               sales, 
                               final_sales)) %>% 
  filter(final_sales > 0) %>% 
  mutate(year = stri_sub(quarter, 1, 4)) %>% 
  select(year, quarter, province, city, seg, pchc, market, packid, 
         sales = final_sales, panel_all)

write.xlsx(proj.total, "03_Outputs/03_WYETH_Ca_Projection.xlsx")

