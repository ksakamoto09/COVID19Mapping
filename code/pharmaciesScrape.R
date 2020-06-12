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
plcUrl <- "https://maps.googleapis.com/maps/api/place/textsearch/json?query="

# Attach API key
key <- Sys.getenv("GOOGLEPLACES_KEY") #add you own key

type = "Pharmacy"
# search radius
radius = 500 # roughly 500 meters

## grid of lat long for analysis
nybb <- read_sf(file.path(shapeDir,"nybb"))
nycgrid <- st_make_grid(nybb, cellsize =  5280)
nyccentroids <- st_centroid(nycgrid) %>% st_transform(4326)
coords <- st_coordinates(nyccentroids) %>% tibble::as_tibble()
coordList <- split(coords,f = seq(nrow(coords)))

placeQuery <- function(coordsPair){
    query <- paste0("&location=", coordsPair$Y, ",", coordsPair$X,"&radius=", radius)
    strurl <- as.character(paste(plcUrl ,type,query, "&key=",key,sep=""))
    rd <- fromJSON(URLencode(strurl))
    
    if(rd$status == "OK"){
        lat <- rd$results$geometry$location$lat
        long <- rd$results$geometry$location$lng
        id <- rd$results$formatted_address
        name <- rd$results$name
        type <-  purrr::map(rd$results$types, ~paste(.x, collapse = ", ")) %>% unlist
        outpuDF <- tibble::tibble(lat, long, id, name, type)
    }
}

#placesDF <- coordList %>% purrr::map_df(~placeQuery(.x))
cleanDF <- placesDF %>% distinct()
cleanSF <- st_as_sf(cleanDF, coords = c("long", "lat"), crs = 4326)

cleanSF <- cleanSF %>% st_transform(2263)
intersectSF <- st_intersection(cleanSF, nybb)
intersectSF <- intersectSF %>% st_transform(4326)
#write_sf(intersectSF, "shapes/nycPharmacies.shp")
#write_csv(intersectSF %>% st_drop_geometry(), "Data/CriticalInfrastructure/pharmaciesNYC.csv")


query <- paste0("500", "@40.754795,-73.9882624")
strurl <- as.character(paste(plcUrl ,type, plcUrl2,query, "&key=",key,sep=""))
rd <- fromJSON(URLencode(strurl))
