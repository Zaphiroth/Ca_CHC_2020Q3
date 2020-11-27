# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Adjustment ----
## corporation
packsize <- fread("02_Inputs/cn_prod_ref_201903_1.txt", stringsAsFactors = FALSE, sep = "|") %>% 
  distinct(Pack_Id, PckSize_Desc) %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

## product name
product.name <- read.xlsx("02_Inputs/商品名_CNEN_packid.xlsx", sheet = 2) %>% 
  distinct(prodid = stri_sub(Pack_ID, 1, 5), prod_cn = `商品名_f`)

## adjustment factor
proj.adj <- proj.price %>% 
  mutate(sales = if_else(city == "上海" & panel_all == 0, sales * 0.5, sales),
         units = if_else(city == "上海" & panel_all == 0, units * 0.5, units),
         packid = stri_pad_left(packid, 7, 0), 
         prodid = stri_sub(packid, 1, 5)) %>% 
  left_join(pack.info, by = "packid") %>% 
  left_join(product.name, by = "prodid") %>% 
  left_join(packsize, by = c("packid" = "Pack_Id")) %>% 
  mutate(prod_en = stri_trim_right(stri_sub(Prd_desc, 1, -4)),
         dosage_units = units * PckSize_Desc,
         channel = "CHC",
         atc3 = stri_sub(ATC4_Code, 1, 4),
         sales = if_else(sales < 0, 0, sales),
         units = if_else(units < 0, 0, units),
         sales = if_else(units == 0, 0, sales),
         units = if_else(sales == 0, 0, units)) %>% 
  group_by(packid, channel, province, city, quarter, atc3, market, Molecule_Desc, 
           prod_en, prod_cn, Pck_Desc, Corp_Desc) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE),
            dosage_units = sum(dosage_units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(sales = case_when(
    city == "宁波" & prod_cn == "钙尔奇 D600" ~ sales * 1.5,
    TRUE ~ sales
  ),
  units = case_when(
    city == "宁波" & prod_cn == "钙尔奇 D600" ~ units * 1.5,
    TRUE ~ units
  ),
  dosage_units = case_when(
    city == "宁波" & prod_cn == "钙尔奇 D600" ~ dosage_units * 1.5,
    TRUE ~ dosage_units
  )) %>% 
  mutate(units = round(units),
         dosage_units = round(dosage_units),
         sales = round(sales, 2),
         Corp_Desc = if_else(grepl("钙尔奇", prod_cn), "WYETH", Corp_Desc)) %>% 
  filter(prod_en != "JING RENEED") %>% 
  select(Pack_ID = packid, Channel = channel, Province = province, City = city, 
         Date = quarter, ATC3 = atc3, MKT = market, Molecule_Desc = Molecule_Desc, 
         Prod_Desc_EN = prod_en, Prod_Desc_CN = prod_cn, Pck_Desc, Corp_Desc, 
         Sales = sales, Units = units, DosageUnits = dosage_units)

write.xlsx(proj.adj, '03_Outputs/05_WYETH_Ca_2020Q3.xlsx')


##---- Qoq-growth ----
ca.chpa <- read.xlsx('05_Internal_Review/Ca_CHPA_2018Q1_2020Q3.xlsx')

chpa.growth <- ca.chpa %>% 
  filter(Date %in% c('2020Q2', '2020Q3'), 
         Sales > 0) %>% 
  pivot_wider(id_cols = Pack_ID, 
              names_from = Date, 
              values_from = Sales, 
              values_fill = 0) %>% 
  mutate(qoq_growth = `2020Q3` / `2020Q2`, 
         qoq_growth = ifelse(qoq_growth > quantile(qoq_growth, 0.95), 
                             quantile(qoq_growth, 0.95), 
                             ifelse(qoq_growth < quantile(qoq_growth, 0.05), 
                                    quantile(qoq_growth, 0.05), 
                                    qoq_growth))) %>% 
  select(Pack_ID, qoq_growth)


##---- result ----
## history
history.delivery <- read.xlsx("06_Deliveries/05_WYETH_Ca_CHC_2018Q1_2020Q2_20201109.xlsx")

## growth projection
proj.growth <- history.delivery %>% 
  filter(Date == '2020Q2', !(City %in% unique(proj.adj$City))) %>% 
  left_join(chpa.growth, by = 'Pack_ID') %>% 
  mutate(Sales = Sales * qoq_growth, 
         Units = Units * qoq_growth, 
         DosageUnits = DosageUnits * qoq_growth, 
         Date = '2020Q3')

## result
wyeth.delivery <- bind_rows(history.delivery, proj.adj, proj.growth) %>% 
  filter(Prod_Desc_EN != "JING RENEED") %>% 
  arrange(Date, Province, City, Pack_ID) %>% 
  group_by(Pack_ID) %>% 
  mutate(ATC3 = first(na.omit(ATC3)), 
         Molecule_Desc = first(na.omit(Molecule_Desc)), 
         Prod_Desc_EN = first(na.omit(Prod_Desc_EN)), 
         Prod_Desc_CN = first(na.omit(Prod_Desc_CN)), 
         Pck_Desc = first(na.omit(Pck_Desc)), 
         Corp_Desc = first(na.omit(Corp_Desc))) %>% 
  ungroup() %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc_EN, Prod_Desc_CN, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits)

write.xlsx(wyeth.delivery, "03_Outputs/05_WYETH_Ca_CHC_2018Q1_2020Q3.xlsx")
