library(readr)
library(dplyr)
covid <- read_csv("Data/COVID/COVID2020forModel.csv")
svi <- read_csv("Data/COVID/SVI.csv")
uhvi <- read_csv("Data/COVID/UHVI.csv")
#  asthma, COPD, diabetes, cancer, smoking, coronary heart disease, hypertension, stroke, kidney disease, and old age (>= 65 years)
uhvi <- uhvi %>% mutate(Geo_FIPS = as.character(TractFIPS)) %>% 
    select(Geo_FIPS, CASTHMA_CrudePrev,COPD_CrudePrev,DIABETES_CrudePrev,
                CANCER_CrudePrev, CSMOKING_CrudePrev, CHD_CrudePrev, 
                BPHIGH_CrudePrev,STROKE_CrudePrev,
                KIDNEY_CrudePrev,COREM_CrudePrev, COREW_CrudePrev) %>% 
    mutate_at(vars(contains("CrudePrev")), funs(ntile(., 100))) %>% 
    mutate(SumUHVI = rowSums(select(.,CASTHMA_CrudePrev:COREW_CrudePrev), na.rm = TRUE)/11) %>% 
    select(Geo_FIPS, SumUHVI)

svi <- svi %>% mutate(Geo_FIPS = as.character(FIPS)) %>% 
    select(Geo_FIPS, RPL_THEMES) %>% 
    mutate(RPL_THEMES = RPL_THEMES * 100) %>% 
    rename(SVI = RPL_THEMES)
nyct <- sf::st_read("shapes/BaseLayers/nyct2010/")
nyct<- nyct %>% sf::st_drop_geometry() %>% select(BoroCT2010, Geo_FIPS)

covid <- covid %>% mutate(Geo_FIPS = as.character(Geo_FIPS)) %>%
    left_join(svi) %>% 
    left_join(uhvi) %>% 
    left_join(nyct)

write_csv(covid, "Data/COVID/COVID2020forModel.csv")
