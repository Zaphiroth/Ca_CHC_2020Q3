# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Adjustment ----
# corporation
packsize <- fread("02_Inputs/cn_prod_ref_201903_1.txt", stringsAsFactors = FALSE, sep = "|") %>% 
  distinct(Pack_Id, PckSize_Desc) %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

# product name
product.name <- read.xlsx("02_Inputs/商品名_CNEN_packid.xlsx", sheet = 2) %>% 
  distinct(packid = Pack_ID, prod_cn = `商品名_f`)

# adjustment factor
proj.adj <- proj.price %>% 
  mutate(sales = if_else(city == "上海" & panel_all == 0, sales * 0.5, sales),
         units = if_else(city == "上海" & panel_all == 0, units * 0.5, units),
         packid = stri_pad_left(packid, 7, 0)) %>% 
  left_join(pack.info, by = "packid") %>% 
  left_join(product.name, by = "packid") %>% 
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

write.xlsx(proj.adj, '03_Outputs/05_WYETH_Ca_Summary.xlsx')


##---- result ----
history.delivery <- read.xlsx("06_Deliveries/05_WYETH_Ca_CHC_2018Q1_2020Q2_20201109.xlsx")

wyeth.delivery <- bind_rows(proj.adj, history.delivery) %>% 
  filter(Prod_Desc_EN != "JING RENEED") %>% 
  # filter(City == "广州", Prod_Desc_CN == "钙尔奇 D600") %>% 
  # setDT() %>% 
  # dcast(Pack_ID + Channel + Province + City + ATC3 + MKT + Molecule_Desc + Prod_Desc_EN + 
  #         Prod_Desc_CN + Pck_Desc + Corp_Desc ~ Date, value.var = c("Sales", "Units", "DosageUnits")) %>% 
  # mutate(Date = "2019Q4",
  #        Sales = round(Sales_2018Q4 * Sales_2019Q3 / Sales_2018Q3, 2),
  #        Units = round(Units_2018Q4 * Sales_2019Q3 / Sales_2018Q3),
  #        DosageUnits = round(DosageUnits_2018Q4 * Sales_2019Q3 / Sales_2018Q3)) %>% 
  arrange(Date, Province, City, Pack_ID) %>% 
  group_by(Pack_ID) %>% 
  mutate(ATC3 = first(ATC3), 
         Molecule_Desc = first(Molecule_Desc), 
         Prod_Desc_EN = first(Prod_Desc_EN), 
         Prod_Desc_CN = first(Prod_Desc_CN), 
         Pck_Desc = first(Pck_Desc), 
         Corp_Desc = first(Corp_Desc)) %>% 
  ungroup() %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc_EN, Prod_Desc_CN, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits)

write.xlsx(wyeth.delivery, "03_Outputs/05_WYETH_Ca_CHC_2018Q1_2020Q3.xlsx")
