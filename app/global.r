library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'Data')
shapeDir <- file.path(root, 'shapes')

nyct <- st_read(file.path(shapeDir, 'nyct2010'), stringsAsFactors = FALSE)
nyct <- nyct %>% mutate(Geo_COUNTY  = case_when(BoroName == 'Bronx' ~ '005',
                                                BoroName == 'Brooklyn' ~ '047',
                                                BoroName == 'Manhattan' ~ '061',
                                                BoroName == 'Queens' ~ '081',
                                                BoroName == 'Staten Island' ~ '085',
                                                TRUE ~ 'Other'))

## Critical Infrastructure Data
supermarkets <- st_read(file.path(shapeDir, "nyc_supermarkets"))
supermarketsBB <- supermarkets %>% group_by(BoroName) %>% count() %>% st_drop_geometry() %>% ungroup()

Link <- read_csv(file.path(dataDir,"CriticalInfrastructure" ,"LinkNYC_Locations.csv")) %>% 
    st_as_sf( coords = c("Longitude","Latitude"))

## Economic Vulnerability Data
# https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr&table=B08303
travelTime <- read_csv(file.path(dataDir, 'EconomicVulnerability', 'ACS20185yr_AvgTravel.csv')) %>% 
    select(Geo_COUNTY, Geo_TRACT, starts_with('SE')) %>% 
    rename(TotPop = SE_A00001_001,
           AvgCommute = SE_A09003_001)



nyct <- nyct %>% left_join(travelTime, by = c('Geo_COUNTY' = 'Geo_COUNTY', 'CT2010' = 'Geo_TRACT'))
palCommute <- colorQuantile(n = 6,
                     palette = "Reds",
                     domain = nyct$AvgCommute)

st_transform(nyct, crs = 4326)
