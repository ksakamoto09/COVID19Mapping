# Load libraries
library(jsonlite)
library(utils)
library(readr)
library(dplyr)
library(leaflet)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'Data')
shapeDir <- file.path(root, 'shapes')

# google places api call
plcUrl <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

# Attach API key
key <- Sys.getenv("GOOGLEPLACES_KEY") #add you own key


# landmark types "https://developers.google.com/places/supported_types"
type = "grocery_or_supermarket"
# search radius
radius = 2640 # roughly 500 meters

## grid of lat long for analysis
nybb <- read_sf(file.path(shapeDir,"nybb"))

nycgrid <- st_make_grid(nybb, cellsize =  5280)

nyccentroids <- st_centroid(nycgrid) %>% st_transform(4326)

coords <- st_coordinates(nyccentroids) %>% tibble::as_tibble()

coordList <- split(coords,f = seq(nrow(coords)))

## automating loop for google places

placeQuery <- function(coordsPair){
    query <- paste0("location=", coordsPair$Y, ",", coordsPair$X, "&radius=", radius, "&type=", type)
    strurl <- as.character(paste(plcUrl ,query,"&key=",key,sep=""))
    rd <- fromJSON(URLencode(strurl))
    
    if(rd$status == "OK"){
        lat <- rd$results$geometry$location$lat
        long <- rd$results$geometry$location$lng
        id <- rd$results$id
        id_place <- rd$results$place_id
        name <- rd$results$name
        type <-  purrr::map(rd$results$types, ~paste(.x, collapse = ", ")) %>% unlist
        outpuDF <- tibble::tibble(lat, long, id, id_place, name, type)
    }
}

#placesDF <- coordList %>% purrr::map_df(~placeQuery(.x))
placesDF <- read_csv(file.path(dataDir, 'placesDF.csv'))
# we want to keep unique rows
cleanDF <- placesDF %>% distinct()
cleanSF <- st_as_sf(cleanDF, coords = c("long", "lat"), crs = 4326)

cleanSF <- cleanSF %>% st_transform(2263)
intersectSF <- st_intersection(cleanSF, nybb)
intersectSF <- intersectSF %>% st_transform(4326)


# if you want to map it out on leaflet
pal <- colorQuantile(n = 6,
    palette = "Reds",
    domain = test$area)

leaflet(intersectSF, options = leafletOptions(zoomControl = TRUE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircleMarkers(data = intersectSF,
               label = ~name,
             #   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
             #                                         iconCreateFunction =
             #                                             JS("
             #                                                function(cluster) {
             #                                                return new L.DivIcon({
             #                                                html: '<div style=\"background-color:gray;opacity: 0.8; color:white;font-weight: bold;\"><span>' + cluster.getChildCount() + '</div><span>',
             #                                                className: 'marker-cluster'
             #                                                });
             #                                                }")
             #                                         ),
             labelOptions = labelOptions(noHide = FALSE,
                                         textOnly = TRUE,textsize = "15px")) %>% 
    addPolygons(data = test, color = ~pal(area))


box <- st_bbox(nybb)

v <- cleanSF%>% 
    st_geometry() %>%
    st_union() %>%
    st_voronoi(do.call(c, .)) %>%
    st_collection_extract()


test <- st_sf(geom = v)

test <- test %>% st_intersection(nybb)

test <- test %>% mutate(area = st_area(test) %>% as.numeric())

library(ggplot2)
test %>% ggplot(aes(x = area)) + geom_histogram()

test <- test %>% st_transform(4326)

leaflet(intersectSF, options = leafletOptions(zoomControl = TRUE)) %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    addPolygons(data = test, fillColor = ~pal(area), weight = 1,
                color = ~pal(area)) %>% 
    addCircleMarkers(data = intersectSF, 
                     stroke = .5, radius = 3,color = "white",
                     label = ~name)
