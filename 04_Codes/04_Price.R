# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Price
# programmer:   Zhe Liu
# Date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Price ----
# origin price
price.origin <- raw.total %>% 
  bind_rows(imp.sh) %>% 
  group_by(packid, quarter, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(-sales, -units)

# mean price by city year
price.city <- raw.total %>% 
  group_by(packid, year, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_city = sales / units) %>% 
  select(-sales, -units)

# mean price by province quarter
price.province <- raw.total %>% 
  group_by(packid, quarter, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_prov = sales / units) %>% 
  select(-sales, -units)

# mean price by province year
price.year <- raw.total %>% 
  group_by(packid, year, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_year = sales / units) %>% 
  select(-sales, -units)

# mean price by pack quarter
price.pack <- raw.total %>% 
  group_by(packid, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack = sales / units) %>% 
  select(-sales, -units)

# mean price by pack year
price.pack.year <- raw.total %>% 
  group_by(packid, year) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack_year = sales / units) %>% 
  select(-sales, -units)


##---- result ----
proj.price <- proj.total %>% 
  left_join(price.origin, by = c("province", "city", "quarter", "packid")) %>% 
  left_join(price.city, by = c("province", "city", "year", "packid")) %>% 
  left_join(price.province, by = c("province", "quarter", "packid")) %>% 
  left_join(price.year, by = c("province", "year", "packid")) %>% 
  left_join(price.pack, by = c("quarter", "packid")) %>% 
  left_join(price.pack.year, by = c('year', 'packid')) %>% 
  mutate(price = if_else(is.na(price), price_city, price), 
         price = if_else(is.na(price), price_prov, price), 
         price = if_else(is.na(price), price_year, price), 
         price = if_else(is.na(price), price_pack, price), 
         price = if_else(is.na(price), price_pack_year, price)) %>% 
  mutate(units = sales / price) %>% 
  # filter(!is.na(price), sales > 0) %>% 
  select(year, quarter, province, city, seg, pchc, market, packid, price, 
         units, sales, panel_all)

write.xlsx(proj.price, "03_Outputs/04_WYETH_Ca_Price.xlsx")

