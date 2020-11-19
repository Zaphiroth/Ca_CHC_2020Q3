# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Shanghai imputation
# programmer:   Zhe Liu
# Date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
# molecule info
mole.ref <- fread("02_Inputs/cn_mol_ref_201903_1.txt", stringsAsFactors = FALSE)
mole.lkp <- fread("02_Inputs/cn_mol_lkp_201903_1.txt", stringsAsFactors = FALSE)

mole.mapping <- mole.lkp %>% 
  left_join(mole.ref, by = c("Molecule_ID" = "Molecule_Id")) %>% 
  arrange(Molecule_Desc) %>% 
  group_by(packid = stri_pad_left(Pack_ID, 7, 0)) %>% 
  summarise(Molecule_Desc = paste0(Molecule_Desc, collapse = "+")) %>% 
  ungroup()

# 2018 data
total.2018.part1 <- read.xlsx("02_Inputs/data/五城市rawdata_2018.xlsx")
total.2018.part2 <- read.xlsx("02_Inputs/data/二城市rawdata_2018.xlsx")

total.2018 <- total.2018.part2 %>% 
  mutate(Date = as.character(Date)) %>% 
  bind_rows(total.2018.part1) %>% 
  mutate(year = stri_sub(Date, 1, 4),
         quarter = ifelse(Date %in% c("201801", "201802", "201803"), "2018Q1", 
                          ifelse(Date %in% c("201804", "201805", "201806"), "2018Q2", 
                                 ifelse(Date %in% c("201807", "201808", "201809"), "2018Q3", 
                                        ifelse(Date %in% c("201810", "201811", "201812"), "2018Q4", 
                                               "2018")))),
         province = gsub("省|市", "", province),
         city = gsub("市", "", city),
         pchc = if_else(is.na(PCHC), PCHC.v2, PCHC)) %>% 
  filter(!is.na(pchc), !is.na(Prd_desc_m)) %>% 
  select(year, date = Date, quarter, province, city, district = `区县`, pchc, 
         packid = pfc_m, Prd_desc = Prd_desc_m, units, 
         sales = value) %>% 
  group_by(year, date, quarter, province, city, district, pchc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(mole.mapping, by = 'packid')


##---- Model ----
# train set
train.set <- total.2018 %>% 
  filter(pchc %in% unique(raw.total$pchc)) %>% 
  mutate(flag = 0) %>% 
  group_by(province, city, district, pchc, flag, date) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

# test set
test.set <- total.2018 %>% 
  filter(city == '上海', !(pchc %in% unique(raw.total$pchc))) %>% 
  mutate(flag = 1) %>% 
  group_by(province, city, district, pchc, flag, date) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

# model
model.train <- train.set[, 5:17]
model.test <- test.set[, 5:17]

knn.model <- kknn(flag ~ ., train = model.train, test = model.test, k = 3, scale = TRUE)

# model weightage
model.indice <- as.data.frame(knn.model$C) %>% 
  lapply(function(x) {
    train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  bind_cols(test.set[, c("province", "city", "pchc")]) %>% 
  pivot_longer(cols = -c(province, city, pchc), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

model.weight <- as.data.frame(knn.model$D) %>% 
  lapply(function(x) {
    1 / (x+1)
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>% 
  bind_cols(test.set[, c("province", "city", "pchc")]) %>% 
  select(-weight_sum) %>% 
  pivot_longer(cols = -c(province, city, pchc), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')

# model growth
model.growth <- raw.total %>% 
  left_join(mole.mapping, by = 'packid') %>% 
  bind_rows(total.2018) %>% 
  filter(quarter %in% c("2018Q3", "2020Q3"), 
         pchc %in% train.set$pchc) %>% 
  group_by(knn_pchc = pchc, Molecule_Desc, year, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(model.indice, by = "knn_pchc") %>% 
  left_join(model.weight, by = c("province", "city", "pchc", "knn_level")) %>% 
  group_by(pchc, Molecule_Desc, year, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(pchc, Molecule_Desc), 
              names_from = quarter, 
              values_from = sales, 
              values_fill = 0) %>% 
  mutate(growth_q3 = `2020Q3` / `2018Q3`,
         growth_q3 = ifelse(is.na(growth_q3) | growth_q3 < 0.1 | growth_q3 > 10, 1, growth_q3)) %>% 
  select(pchc, Molecule_Desc, `2018Q3` = growth_q3) %>% 
  pivot_longer(cols = `2018Q3`, 
               names_to = 'quarter', 
               values_to = 'growth')

# model sales
imp.sh <- total.2018 %>% 
  filter(pchc %in% test.set$pchc, 
         quarter %in% c("2018Q3")) %>% 
  left_join(model.growth, by = c("pchc", "Molecule_Desc", "quarter")) %>% 
  mutate(growth = if_else(is.na(growth), 0, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units = units * growth,
         sales = sales * growth,
         year = "2020",
         date = gsub("2018", "2020", date),
         quarter = gsub("2018", "2020", quarter)) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)


##---- Bind ----
imp.total <- bind_rows(raw.total, imp.sh) %>% 
  filter(units > 0, sales > 0)

write.xlsx(imp.total, "03_Outputs/02_WYETH_Ca_Imputation.xlsx")

