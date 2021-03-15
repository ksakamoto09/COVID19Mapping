library(readr)

covid <- read_csv("dasymetry/COVIDJOIN.csv")

library(dplyr)

covid

zipPop <- covid %>% group_by(MODZCTA) %>% 
    summarize(zipPop = sum(PropPop, na.rm = TRUE))

zipPop %>% View

covidJoin <- covid %>% left_join(zipPop) %>% 
    mutate(zipprop = PropPop/zipPop,
           deaths = zipprop * COVID_DEATH_COUNT,
           cases = zipprop * COVID_CASE_COUNT)

nycCTCOVID <- covidJoin %>% group_by(nyct2010_Geo_FIPS) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE),
              cases = sum(cases, na.rm = TRUE))
