# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Projection rate ----
raw.sum <- raw.total %>% 
  group_by(province, city, quarter, packid) %>% 
  summarise(units_raw = sum(units, na.rm = TRUE), 
            sales_raw = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

imp.sum <- imp.total %>% 
  group_by(province, city, quarter, packid) %>% 
  summarise(units_imp = sum(units, na.rm = TRUE), 
            sales_imp = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

proj.sum <- proj.price %>% 
  group_by(province, city, quarter, packid) %>% 
  summarise(units_proj = sum(units, na.rm = TRUE), 
            sales_proj = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

proj.rate <- raw.sum %>% 
  full_join(imp.sum, by = c('province', 'city', 'quarter', 'packid')) %>% 
  full_join(proj.sum, by = c('province', 'city', 'quarter', 'packid')) %>% 
  mutate(sales_imp_rate = sales_imp / sales_raw, 
         sales_proj_rate = sales_proj / sales_imp) %>% 
  left_join(pack.info, by = "packid")

write.xlsx(proj.rate, '05_Internal_Review/Projection_Rate.xlsx')


##---- Price check ----
price.check <- wyeth.delivery %>% 
  mutate(price = round(Sales / Units)) %>% 
  distinct(City, Pack_ID, Date, price) %>% 
  pivot_wider(id_cols = c(City, Pack_ID), 
              names_from = Date, 
              values_from = price)

write.xlsx(price.check, '05_Internal_Review/Price_Check.xlsx')


##---- Check SOP -----
# CHPA
chpa.format <- read.xlsx('05_Internal_Review/ims_chpa_to20Q3_format.xlsx')

ca.chpa <- chpa.format %>% 
  pivot_longer(cols = c(ends_with('UNIT'), ends_with('RENMINBI')), 
               names_to = 'quarter', 
               values_to = 'value') %>% 
  separate(quarter, c('quarter', 'measure'), sep = '_') %>% 
  pivot_wider(id_cols = c(Pack_ID, ATC3_Code, Molecule_Desc, Prd_desc, 
                          Pck_Desc, Corp_Desc, quarter), 
              names_from = measure, 
              values_from = value) %>% 
  filter(Pack_ID %in% market.def, 
         stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  mutate(MKT = 'CA', 
         Prd_desc = trimws(stri_sub(Prd_desc, 1, -4))) %>% 
  select(Pack_ID, Date = quarter, ATC3 = ATC3_Code, MKT, Molecule_Desc, 
         Prod_Desc_EN = Prd_desc, Pck_Desc, Corp_Desc, Units = UNIT, 
         Sales = RENMINBI)

write.xlsx(ca.chpa, '05_Internal_Review/Ca_CHPA_2018Q1_2020Q3.xlsx')

# Update
chpa.info <- ca.chpa %>% 
  distinct(Pack_ID, ATC3, Molecule_Desc, Prod_Desc_EN, Pck_Desc, Corp_Desc)

delivery.update <- wyeth.delivery %>% 
  distinct(Pack_ID, Channel, Province, City, Date, MKT, Prod_Desc_CN, 
           Sales, Units, DosageUnits) %>% 
  left_join(chpa.info, by = 'Pack_ID') %>% 
  filter(!is.na(ATC3))

write.xlsx(delivery.update, '05_Internal_Review/delivery_updated.xlsx')


