library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'Data')
shapeDir <- file.path(root, 'shapes')


#### Critical Infrastructure Data

# supermarkets <- st_read(file.path(shapeDir, "nyc_supermarkets"))
# supermarketsBB <- supermarkets %>% group_by(BoroName) %>% count() %>% st_drop_geometry() %>% ungroup()
# 
# pharmacies <- st_read(file.path(shapeDir, "nycPharmacies")) %>% st_transform(4326)
# 
# nycFoodPantry <- st_read(file.path(shapeDir, 'nycFoodPantry')) 
# 
# subway <- readr::read_csv(file.path(dataDir,"CriticalInfrastructure" ,"NYC_Subway_stations_locations.csv"))
# newGeom = st_as_sfc(subway$the_geom)
# subway = st_set_geometry(subway, newGeom) %>% st_set_crs(4326)
# subway$the_geom=NULL
# 
# precincts <- readr::read_csv(file.path(dataDir,"CriticalInfrastructure" ,"NYPD_precinct_locations.csv"))
# newGeom = st_as_sfc(precincts$the_geom)
# precincts = st_set_geometry(precincts, newGeom) %>% st_set_crs(4326)
# precincts$the_geom=NULL

# Link <- read_csv(file.path(dataDir,"CriticalInfrastructure" ,"LinkNYC_Locations.csv")) %>% 
#     st_as_sf( coords = c("Longitude","Latitude"))
blocks <- st_read(file.path(shapeDir, "BaseLayers", "PLUTO_Dissolve"),stringsAsFactors = FALSE)  %>% 
    mutate(Geo_COUNTY  = case_when(BoroName == 'Bronx' ~ '005',
                                   BoroName == 'Brooklyn' ~ '047',
                                   BoroName == 'Manhattan' ~ '061',
                                   BoroName == 'Queens' ~ '081',
                                   BoroName == 'Staten Island' ~ '085',
                                   TRUE ~ 'Other'),
           Geo_FIPS = paste0("36", Geo_COUNTY, CT2010_y),
           Shape_Area = st_area(.) %>% as.numeric(),
           acre = Shape_Area * 2.2957e-5) %>%  
               st_centroid()

dist <- read_csv(file.path(dataDir, "CriticalInfrastructure", "CIDist.csv")) %>% 
    rename(CT2010_y = CT2010.y)

CIDist <- blocks %>% left_join(dist) %>% st_transform(4326)

marketBreaks <- BAMMtools::getJenksBreaks(CIDist$mrktDst, k = 5)
marketPal <- colorBin(palette = "Reds", bins = marketBreaks,
                      domain = CIDist$mrktDst)

subwayBreaks <- BAMMtools::getJenksBreaks(CIDist$sbwyDst, k = 5)
subwayPal <- colorBin(palette = "Reds", bins = subwayBreaks,
                      domain = CIDist$sbwyDst)

hospBreaks <- BAMMtools::getJenksBreaks(CIDist$hosptlD, k = 5)
hospPal <- colorBin(palette = "Reds", bins = hospBreaks,
                      domain = CIDist$hosptlD)

pharmBreaks <- BAMMtools::getJenksBreaks(CIDist$phrmDst, k = 5)
pharmPal <- colorBin(palette = "Reds", bins = pharmBreaks,
                      domain = CIDist$phrmDst)
pantryBreaks <- BAMMtools::getJenksBreaks(CIDist$pntryDs, k = 5)
pantryPal <- colorBin(palette = "Reds", bins = pantryBreaks,
                     domain = CIDist$pntryDs)

## Economic Vulnerability Data
# https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr&table=B08303
travelTime <- read_csv(file.path(dataDir, 'EconomicVulnerability', 'ACS20185yr_AvgTravel.csv')) %>% 
    select(Geo_FIPS, starts_with('SE')) %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS)) %>% 
    rename(TotPop = SE_A00001_001,
           AvgCommute = SE_A09003_001) %>% 
    select(Geo_FIPS, AvgCommute)
employment <- read_csv(file.path(dataDir, 'EconomicVulnerability', 
                                     'ACS2018_5yr_employment_schoolDropout_HHIncome.csv')) %>% 
    select(Geo_FIPS, starts_with('SE')) %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS)) %>% 
    rename(civPop16_19 =  SE_A12003_001,
           dropOutSchool = SE_A12003_002,
           civPopOver16 = SE_A17005_001,
           unemployed = SE_A17005_003,
           medHHIncome = SE_A14006_001,
           avgHHIncome = SE_A14008_001) %>% 
    select(!starts_with('SE'))

NAICS <- readxl::read_xlsx(file.path(dataDir, 'EconomicVulnerability', 
                                     'workersIndustryNAICS.xlsx')) %>% 
    rename(TotJobs = `Total Number of Jobs`,
           retail = `NAICS Sector 44-45 (Retail Trade)`,
           trans = `NAICS Sector 48-49 (Transportation and Warehousing)`,
           ed = `NAICS Sector 61 (Educational Services)`,
           health = `NAICS Sector 62 (Health Care and Social Assistance)`,
           construction = `NAICS Sector 23 (Construction)`,
           food = `NAICS Sector 72 (Accommodation and Food Services)`) %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS)) %>% 
    select(Geo_FIPS, TotJobs, retail, trans, ed, health, construction, food)

nyct_economic <- blocks %>% 
    left_join(travelTime, by = c('Geo_FIPS' = 'Geo_FIPS')) %>% 
    left_join(employment,  by = c('Geo_FIPS' = 'Geo_FIPS')) %>% 
    left_join(NAICS,  by = c('Geo_FIPS' = 'Geo_FIPS')) %>% 
    mutate(unempDensity = (unemployed*resPercent)/acre,
           dropOutDensity = (dropOutSchool*resPercent)/acre,
           TotJobs = (TotJobs*resPercent)/acre,
           retailDensity = (retail*resPercent)/acre,
           transDensity = (trans*resPercent)/acre,
           edDensity = (ed*resPercent)/acre,
           heatlhDensity= (health*resPercent)/acre,
           constructDensity = (construction*resPercent)/acre,
           foodDensity = (food*resPercent)/acre) %>%
    st_transform(crs = 4326)

commuteBreaks <- BAMMtools::getJenksBreaks(nyct_economic$AvgCommute, k = 5)
palCommute <- colorBin( palette = "Reds",bins = commuteBreaks,
                    domain =nyct_economic$AvgCommute )
incomeBreaks <- BAMMtools::getJenksBreaks(nyct_economic$medHHIncome, k = 5)
palMedHHIncome <- colorBin( palette = "Reds",bins = incomeBreaks,
                        domain =nyct_economic$medHHIncome )
dropOutBreaks <- BAMMtools::getJenksBreaks(nyct_economic$dropOutDensity, k = 5)
palDropOut <- colorBin( palette = "Reds",bins = dropOutBreaks,
                        domain =nyct_economic$dropOutDensity )

unempBreaks <- BAMMtools::getJenksBreaks(nyct_economic$unempDensity, k = 5)
palUnemp <- colorBin( palette = "Reds",bins = unempBreaks,
                        domain =nyct_economic$unempDensity )

retailBreaks <- BAMMtools::getJenksBreaks(nyct_economic$retailDensity, k = 5)
palRetail <- colorBin( palette = "Reds",bins = retailBreaks,
                      domain =nyct_economic$retailDensity )

transBreaks <- BAMMtools::getJenksBreaks(nyct_economic$transDensity, k = 5)
palTrans <- colorBin( palette = "Reds",bins = transBreaks,
                       domain =nyct_economic$transDensity )
edBreaks <- BAMMtools::getJenksBreaks(nyct_economic$edDensity, k = 5)
palEdu <- colorBin( palette = "Reds",bins = edBreaks,
                      domain =nyct_economic$edDensity )
healthBreaks <- BAMMtools::getJenksBreaks(nyct_economic$heatlhDensity, k = 5)
palHealth <- colorBin( palette = "Reds",bins = healthBreaks,
                    domain =nyct_economic$heatlhDensity )
constructBreaks <- BAMMtools::getJenksBreaks(nyct_economic$constructDensity, k = 5)
palConstruct <- colorBin( palette = "Reds",bins = constructBreaks,
                       domain =nyct_economic$constructDensity )
foodBreaks <- BAMMtools::getJenksBreaks(nyct_economic$foodDensity, k = 5)
palFood <- colorBin( palette = "Reds",bins = foodBreaks,
                          domain =nyct_economic$constructDensity )


## Population Demographics
race <- read_csv(file.path(dataDir, 'PopulationDemographics', 
                                 'race_NYC_censustract.csv')) %>% 
    select(Geo_FIPS, starts_with('SE')) %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS)) %>% 
    rename(TotPop = SE_A03001_001,
          whitePop = SE_A03001_002,
          blackPop = SE_A03001_003,
          asianPop = SE_A03001_005,
          otherPop = SE_A03001_007)
    # mutate(whitePercent = if_else(TotPop==0, NA_real_, 100*(whitePop/TotPop)),
    #        blackPercent = if_else(TotPop==0, NA_real_, 100*(blackPop/TotPop)),
    #        asianPercent = if_else(TotPop==0, NA_real_, 100*(asianPop/TotPop)),
    #        otherPercent = if_else(TotPop==0, NA_real_, 100*(otherPop/TotPop)))
    

nyct_demogrpahics <- blocks %>% 
    left_join(race, by = c('Geo_FIPS' = 'Geo_FIPS')) %>% 
    st_transform(crs = 4326)
nyct_demogrpahics <- nyct_demogrpahics %>% 
    mutate(popNumber = TotPop*resPercent,
       blackNumber = blackPop*resPercent,
       whiteNumber = whitePop*resPercent,
       asianNumber = asianPop*resPercent,
       otherNumber = otherPop*resPercent,
       popDensity = popNumber /acre,
       blackDensity = blackNumber/ acre,
       whiteDensity = whiteNumber/acre,
       asianDensity = asianNumber/acre,
       otherDensity = otherNumber/acre) 

popBreaks <- BAMMtools::getJenksBreaks(nyct_demogrpahics$popDensity, k = 5)
palPop <- colorBin( palette = "Blues",bins = popBreaks,
                    domain = nyct_demogrpahics$popDensity )

blackBreaks <- BAMMtools::getJenksBreaks(nyct_demogrpahics$blackDensity, k = 5)
palBlack <- colorBin(palette = "Blues", bins = blackBreaks,
                        domain = nyct_demogrpahics$blackDensity)

whiteBreaks <- BAMMtools::getJenksBreaks(nyct_demogrpahics$whiteDensity, k = 5)
palWhite <- colorBin(palette = "Blues", bins = whiteBreaks,
                        domain = nyct_demogrpahics$whiteDensity)

AsianBreaks <- BAMMtools::getJenksBreaks(nyct_demogrpahics$asianDensity, k = 5)
palAsian <- colorBin(palette = "Blues", bins = AsianBreaks,
                        domain = nyct_demogrpahics$asianDensity)

otherBreaks <- BAMMtools::getJenksBreaks(nyct_demogrpahics$otherDensity, k = 5)
palOther <- colorBin(palette = "Blues", bins = otherBreaks,
                     domain = nyct_demogrpahics$otherDensity)

