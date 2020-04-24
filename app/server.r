library(shiny)
library(leaflet)

server <- function(input, output) {

## map for Critical Infrastructure
    output$ciMap <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = TRUE)) %>% 
            setView(lat = 40.7445698, lng = -73.9375989, zoom = 12) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            addCircleMarkers(data = supermarkets, 
                             stroke = .5, radius = 3,color = "white",
                             label = ~name,
                             group = "Grocery Stores") %>% 
            addCircleMarkers(data = Link, 
                             stroke = .5, radius = 3,color = "red",
                             label = ~`Street Address`,
                             group = "Link Kiosks") %>% 
        addLayersControl(
            overlayGroups = c("Grocery Stores", "Link Kiosks"),
            options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    output$groceryB <- renderPlot(
        ggplot(supermarketsBB, aes(x= BoroName, y = n, fill = BoroName)) + 
            geom_bar(stat = "identity", show.legend = FALSE) +
            xlab("Borough") + 
            ylab("Count") + 
            theme_dark()+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    )

## Map for Economic Vulnerbility
    output$evMap <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = TRUE)) %>% 
            setView(lat = 40.7445698, lng = -73.9375989, zoom = 12) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            addPolygons(data = st_transform(nyct, crs = 4326), 
                             stroke = .5,
                        color = ~palCommute(AvgCommute),
                             label = ~AvgCommute,
                             group = "Link Kiosks")
    })
    
}