library(readr)
library(dplyr)
library(jsonlite)
library(utils)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'Data')

foodpantry <- read_csv(file.path(dataDir,'CriticalInfrastructure', 'NYCFoodPantriesOrig.csv')) 

# NEWMAN MEMORIAL UNITED METHODIST CHURCH COMMU = 257 Macon St
cleanDF <- foodpantry %>% 
    filter(DIS != 'DIS',
           !stringr::str_detect(DISTADD, "\\s-\\s"),
           !stringr::str_detect(DISTADD, "[!@#$%^&*()]")) 

    distinct(DISTADD, .keep_all = TRUE)

toClean <- foodpantry %>% setdiff(cleanDF)
toClean %>% View()

#write_csv(toClean, "Data/CriticalInfrastructure/foodPantryClean.csv")
## Manually Clean

toCleanNew <- read_csv(file.path(dataDir,'CriticalInfrastructure', 'foodPantryClean.csv')) %>% 
    mutate(DBIOSRTOZI = as.character(DBIOSRTOZI))

foodPantryClean <- cleanDF %>% bind_rows(toCleanNew) %>% 
    distinct(DISTADD, .keep_all = TRUE)

#write_csv(foodPantryClean, file.path(dataDir, 'CriticalInfrastructure', 'nycfoodPantry.csv'))
#1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=YOUR_API_KEY
geocodeURL <- "https://maps.googleapis.com/maps/api/geocode/json?address="
key <- Sys.getenv("GOOGLEPLACES_KEY") #add you own key

foodPantryClean <- foodPantryClean %>% rowwise() %>% 
    mutate(
        Address = paste(DISTADD %>% stringr::str_trim() %>% stringr::str_replace_all(" ", "+"), 
                        DBIOSRTOZI, DIS, sep = ",")
    ) 

Addresses <- foodPantryClean %>% pull(Address)

geocodeFunc <- function(Address){
    strurl <- as.character(paste(geocodeURL, Address,"&key=",key,sep=""))
    rd <- fromJSON(URLencode(strurl))
    
    if(rd$status == "OK"){
        lat <- rd$results$geometry$location$lat
        long <- rd$results$geometry$location$lng
        id_place <- rd$results$place_id
        formatted_address <- rd$results$formatted_address
        outpuDF <- tibble::tibble(lat, long, id_place, formatted_address, Address)
    }
}

#pantryGeocode <- split(Addresses,f = seq(nrow(foodPantryClean)))  %>% purrr::map_df(~geocodeFunc(.x))
#write_csv(pantryGeocode, file.path(dataDir, 'CriticalInfrastructure', 'pantryGeocode.csv'))
foodPantrysf <- foodPantryClean %>% left_join(pantryGeocode) %>% 
    filter(!is.na(lat)) %>% st_as_sf(coords = c("long", "lat"), crs = 4362)

#sf::write_sf(foodPantrysf, "shapes/nycFoodPantry.shp")
