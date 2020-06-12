pluto <- st_read("shapes/BaseLayers/MapPLUTO")

blockpluto <- pluto %>% 
    filter(!is.na(CT2010)) %>% 
    st_make_valid() %>% 
    group_by(Borough, Block, CT2010) %>% 
    summarize(resArea = sum(ResArea, na.rm = TRUE),
        geometry = st_union(geometry))

leaflet(data = blockpluto %>% filter(Borough == "MN") %>% st_transform(4326),
        options = leafletOptions(zoomControl = TRUE)) %>% 
    setView(lat = 40.7445698, lng = -73.9375989, zoom = 12) %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    addPolygons()


nyct <- st_read("shapes/BaseLayers/nyct2010/")
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


blockpluto <- blockpluto %>% 
    ungroup() %>%
    mutate(Borough = as.character(Borough),
           CT2010 = as.character(CT2010))

blockplutoCast <- blockpluto %>% st_cast()

#blockplutoCT <-  st_join(x = blockplutoCast, y = nyct, join = st_intersects)
blockplutoCast <- blockplutoCast %>% left_join(nyct %>% select(-CT2010) %>% 
                                                   st_drop_geometry(), by = c("CT2010" = "CTLabel",
                                                                 "Borough" = "Borough"))

ctResArea <- blockplutoCast %>% st_drop_geometry() %>% 
    filter(!is.na(Geo_FIPS)) %>% 
    group_by(Geo_FIPS) %>% 
    summarize(resAreaTot = sum(resArea))

blockplutoCTArea <- blockplutoCast %>% left_join(ctResArea)


blockplutoCTArea <- blockplutoCTArea %>% 
    mutate(resPercent = resArea/resAreaTot)

CIDist <- st_read(file.path(shapeDir, "CriticalInfrastructure", "CIDist")) %>% 
    mutate(Geo_COUNTY  = case_when(BoroNam == 'Bronx' ~ '005',
                                   BoroNam == 'Brooklyn' ~ '047',
                                   BoroNam == 'Manhattan' ~ '061',
                                   BoroNam == 'Queens' ~ '081',
                                   BoroNam == 'Staten Island' ~ '085',
                                   TRUE ~ 'Other'),
           Geo_FIPS = paste0("36", Geo_COUNTY, CT2010)) %>% 
    select(mrktDst, sbwyDst, hosptlD, phrmDst, pntryDs, AreBlck, ArBlckP, Geo_FIPS, Borough, Block)

blockplutoCTArea <- blockplutoCTArea %>% filter(!is.na(BoroCode)) %>% 
    left_join(CIDist %>% st_drop_geometry() %>% select(-c(Geo_FIPS, AreBlck, ArBlckP))) %>% 
    distinct(.keep_all = TRUE)

nyctdf  <- read_csv("Data/PopulationDemographics/race_NYC_censustract.csv") %>%
    select(Geo_FIPS, starts_with('SE')) %>%
    mutate(Geo_FIPS = as.character(Geo_FIPS)) %>%
    rename(TotPop = SE_A03001_001,
           whitePop = SE_A03001_002,
           blackPop = SE_A03001_003,
           asianPop = SE_A03001_005,
           otherPop = SE_A03001_007) %>%
    mutate(whitePercent = if_else(TotPop==0, NA_real_, 100*(whitePop/TotPop)),
           blackPercent = if_else(TotPop==0, NA_real_, 100*(blackPop/TotPop)),
           asianPercent = if_else(TotPop==0, NA_real_, 100*(asianPop/TotPop)),
           otherPercent = if_else(TotPop==0, NA_real_, 100*(otherPop/TotPop)))

nyctJoin <- left_join(nyct, nyctdf) %>% mutate(blackDensity = blackPop/Shape_Area)
blockplutoCTArea <- blockplutoCTArea %>% left_join(nyctdf)

blockplutoCTArea <- blockplutoCTArea %>% 
    mutate(popNumber = TotPop*resPercent,
           blackNumber = blackPop*resPercent,
           Shape_Area = st_area(.),
           popDensity = popNumber /Shape_Area,
           blackDensity = blackNumber/ Shape_Area,
           marketDist = supermarketDist) 

popBreaks <- BAMMtools::getJenksBreaks(blockplutoCTArea$popDensity, k = 5)

palPop <- colorBin( palette = "Reds",bins = popBreaks,
                            domain = blockplutoCTArea$popDensity )

blackBreaks <- BAMMtools::getJenksBreaks(blockplutoCTArea$blackDensity, k = 5)
palPopBlack <- colorBin(palette = "Reds", bins = blackBreaks,
                        domain = blockplutoCTArea$blackDensity)

leaflet(data = blockplutoCTArea %>% filter(Borough =="BK", !is.nan(popDensity)) %>% st_transform(4326),
        options = leafletOptions(zoomControl = TRUE)) %>% 
    setView(lat = 40.7445698, lng = -73.9375989, zoom = 12) %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    addPolygons(color = ~palPopBlack(blackDensity),
                label = ~blackDensity,
                stroke = .4,
                group = "Black Pop")  %>%
    addPolygons(stroke = .4,
                color = ~palPop(popDensity),
                label = ~popDensity,
                group = "Tot Pop") %>%
    addLayersControl(
        baseGroups = c("Black Pop", "Tot Pop"),
        options = layersControlOptions(collapsed = FALSE))

#st_write(blockplutoCTArea, "shapes/nycBlocks.shp", append=FALSE)
blackBreaksCT <- BAMMtools::getJenksBreaks(nyctJoin$blackDensity, k = 5)
palPopBlackCT <- colorBin(palette = "Reds", bins = blackBreaks,
                        domain = nyctJoin$blackDensity)
leaflet(options = leafletOptions(zoomControl = TRUE)) %>% 
    setView(lat = 40.7445698, lng = -73.9375989, zoom = 12) %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    addPolygons(data = blockplutoCTArea %>% filter(Borough =="BK", !is.nan(popDensity)) %>% st_transform(4326),
                color = ~palPopBlack(blackDensity),
                label = ~blackDensity,
                stroke = .4,
                group = "Black Density Block")  %>%
    addPolygons(data = nyctJoin %>% filter(BoroName == "Brooklyn") %>% st_transform(4326),
                stroke = .4,
                color = ~palPopBlackCT(blackDensity),
                label = ~blackDensity,
                group = "Black Density Tract") %>%
    addLayersControl(baseGroups = c("Black Density Block", "Black Density Tract"),
        options = layersControlOptions(collapsed = FALSE))

marketBreaks <- BAMMtools::getJenksBreaks(blockplutoCTArea$marketDist, k = 5)
palMarket <- colorBin(palette = "Reds", bins = marketBreaks,
                          domain = blockplutoCTArea$marketDist)
blockplutoCTArea <- blockplutoCTArea %>% 
    mutate(subwayDist = subwayDist)
subwayBreaks <- BAMMtools::getJenksBreaks(blockplutoCTArea$subwayDist, k = 5)
palSubway <- colorBin(palette = "Reds", bins = subwayBreaks,
                      domain = blockplutoCTArea$subwayDist)
leaflet( options = leafletOptions(zoomControl = TRUE)) %>% 
    setView(lat = 40.7445698, lng = -73.9375989, zoom = 12) %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    addPolygons(data = blockplutoCTArea %>% filter(Borough =="QN", !is.nan(popDensity)) %>% st_transform(4326),
                color = ~palMarket(marketDist),
                label = ~marketDist,
                stroke = .4,
                group = "Market Distance") %>%
    addPolygons(data = blockplutoCTArea %>% filter(BoroName == "Queens") %>% st_transform(4326),
                stroke = .4,
                color = ~palSubway(subwayDist),
                label = ~subwayDist,
                group = "Subway Distance") %>%
    addLayersControl(baseGroups = c("Market Distance", "Subway Distance"),
                     options = layersControlOptions(collapsed = FALSE))
