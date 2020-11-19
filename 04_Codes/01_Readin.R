# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Readin raw data
# programmer:   Zhe Liu
# Date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
# PCHC code
pchc.universe.raw <- read.xlsx("02_Inputs/Universe_PCHCCode_20201118.xlsx", sheet = "PCHC")

pchc.universe1 <- pchc.universe.raw %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code), 
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup()

pchc.universe2 <- pchc.universe.raw %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code), 
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup()

pchc.mapping <- bind_rows(pchc.universe1, pchc.universe2) %>% 
  distinct(province, city, district, hospital, pchc)

# pack info
ims.prod <- fread("02_Inputs/cn_prod_ref_201903_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0")) %>% 
  select(Pack_Id, NFC123_Code)

ims.raw <- read.xlsx("02_Inputs/ims_chpa_to19Q4.xlsx")

pack.info <- ims.raw[, 1:21] %>% 
  distinct() %>% 
  filter(!is.na(Pack_Id)) %>% 
  left_join(ims.prod, by = "Pack_Id") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, MNF_TYPE, MnfType_Desc, 
         Mnf_Desc, ATC4_Code, NFC123_Code, Prd_desc, Pck_Desc, 
         Molecule_Desc)

# SKU
sku.info <- read.xlsx('02_Inputs/CA_ahbjjssdzj181920Q1Q2_sku_packid_price_ims_chk_new.xlsx') %>% 
  mutate(Prd_desc_ZB = if_else(is.na(Prd_desc_ZB), '无', Prd_desc_ZB))

# market definition
market.def <- read_xlsx("02_Inputs/钙尔奇 招标数据缺失产品汇总.xlsx", sheet = "市场定义同分子下的packid")
market.def <- sort(c(unique(market.def$Pack_ID), "0242704"))

# target city
kTargetCity <- c("北京", "广州", "杭州", "南京", "宁波", "上海", "苏州")


##---- Raw data ----
raw.ahbjjs <- read.csv('02_Inputs/data/noAZ_EPI_ahbjjs_2020Q3_packid_moleinfo.csv')

raw.total <- raw.ahbjjs %>% 
  filter(Project == 'CA') %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc4 = ATC4_Code, 
           nfc = NFC123_Code, 
           product = Prd_desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = Volume, 
           sales = Value) %>% 
  left_join(pchc.mapping, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc), packid %in% market.def, city %in% kTargetCity, 
         units > 0, sales > 0) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)), 
         city = first(na.omit(city)), 
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, atc4, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units)

write.xlsx(raw.total, '03_Outputs/01_WYETH_Ca_Raw.xlsx')


