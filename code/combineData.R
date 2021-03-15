library(readr)
library(dplyr)

acsCTNames <- read_csv("Data/PopulationDemographics/ACS2018_censusTract.csv") %>% 
    janitor::clean_names() %>% 
    select(56:163) %>% names()
acsCTNames <- c("fips", acsCTNames)

acsCT <- read_csv("Data/PopulationDemographics/ACS2018_censusTract.csv", skip = 1) %>% 
    rename(fips = Geo_FIPS) %>% 
    select(!contains("Geo_"))

names(acsCT) <- acsCTNames

nyct <- sf::st_read("shapes/BaseLayers/nyct2010/")

nyct <- nyct %>% mutate(Geo_COUNTY  = case_when(BoroName == 'Bronx' ~ '005',
                                                BoroName == 'Brooklyn' ~ '047',
                                                BoroName == 'Manhattan' ~ '061',
                                                BoroName == 'Queens' ~ '081',
                                                BoroName == 'Staten Island' ~ '085',
                                                TRUE ~ 'Other'),
                        Geo_FIPS = paste0("36", Geo_COUNTY, CT2010),
                        Borough  = case_when(BoroName == 'Bronx' ~ 'BX',
                                             BoroName == 'Brooklyn' ~ 'BK',
                                             BoroName == 'Manhattan' ~ 'MN',
                                             BoroName == 'Queens' ~ 'QN',
                                             BoroName == 'Staten Island' ~ 'SI',
                                             TRUE ~ 'Other'),
                        CTLabel = as.character(CTLabel))

## Health
healthCT <- acsCT %>% select(fips, contains("health_insurance")) %>% 
    mutate(fips = as.character(fips)) %>% 
    rename(Geo_FIPS = fips)

## Demographics
demoCT <- acsCT %>% select(fips, contains("total_population"), contains("household_size"), contains("25_years"))%>% 
    mutate(fips = as.character(fips)) %>% 
    rename(Geo_FIPS = fips)

## Economic
economicCT <- acsCT %>% select(fips, contains("16_years"), 
                               contains("commute"), contains("median_gross_rent"), 
                               contains("housing_units"), contains("income"))%>% 
    mutate(fips = as.character(fips)) %>% 
    rename(Geo_FIPS = fips)

## Critcal Infrastucture
ciCT <- read_csv("Data/CriticalInfrastructure/combinedCriticalInfra.csv") %>% 
    mutate(BoroCT2010 = as.character(BoroCT2010)) %>% 
    select(-c(CTLabel, BoroCode, BoroName, CT2010, CDEligibil))


## COVID Data
# https://github.com/nychealth/coronavirus-data
covid <- read_csv("Data/COVID/nycCTCovid.csv") %>% 
    rename(Geo_FIPS = nyct2010_Geo_FIPS) %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS))

##JOIN

nyctJoin <- nyct %>% left_join(healthCT) %>% 
    left_join(demoCT) %>% 
    left_join(economicCT) %>% 
    left_join(ciCT) %>% 
    left_join(covid) %>% 
    sf::st_drop_geometry()

#write_csv(nyctJoin, "Data/COVID/COVID2020.csv")
