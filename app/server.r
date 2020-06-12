library(shiny)
library(leaflet)

server <- function(input, output) {

## map for Critical Infrastructure
    output$ciMap <- renderLeaflet({
        leaflet(data = CIDist, options = leafletOptions(zoomControl = TRUE)) %>% 
            setView(lat = 40.7445698, lng = -73.9375989, zoom = 11) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            addCircleMarkers( stroke = .5, radius = .75,
                              color = ~marketPal(mrktDst),
                             label = ~mrktDst,
                             group = "Grocery Stores") %>% 
            addCircleMarkers(stroke = .5, radius = .75, color = ~subwayPal(sbwyDst),
                             label = ~sbwyDst,
                             group = "Subway Entrances") %>% 
            addCircleMarkers(stroke = .5, radius = .75, color = ~hospPal(hosptlD),
                             label = ~hosptlD,
                             group = "Emergency Hospitals") %>% 
        addCircleMarkers(stroke = .5, radius = .75, color = ~pharmPal(phrmDst),
                         label = ~phrmDst,
                         group = "Pharmacies") %>%
            addCircleMarkers(stroke = .5, radius = .75, color = ~pantryPal(pntryDs),
                             label = ~pntryDs,
                             group = "Food Pantries") %>%
        addLayersControl(
            baseGroups = c("Grocery Stores", "Subway Entrances", "Emergency Hospitals", 
                              "Pharmacies","Food Pantries"),
            options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    # output$groceryB <- renderPlot(
    #     ggplot(supermarketsBB, aes(x= BoroName, y = n, fill = BoroName)) + 
    #         geom_bar(stat = "identity", show.legend = FALSE) +
    #         xlab("Borough") + 
    #         ylab("Count") + 
    #         theme_dark()+
    #         theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #     
    # )

## Map for Economic Vulnerbility
    output$evMap <- renderLeaflet({
        leaflet(data = nyct_economic, options = leafletOptions(zoomControl = TRUE)) %>% 
            setView(lat = 40.7445698, lng = -73.9375989, zoom = 11) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palCommute(AvgCommute),
                             label = ~AvgCommute,
                             group = "Average Commute") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palMedHHIncome(medHHIncome),
                        label = ~medHHIncome,
                        group = "Median Household Income") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palUnemp(unempDensity),
                        label = ~unempDensity,
                        group = "Unemployment Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palDropOut(dropOutDensity),
                        label = ~dropOutDensity,
                        group = "High School Drop Out Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palRetail(retailDensity),
                        label = ~retailDensity,
                        group = "Retail Trade Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palTrans(transDensity),
                        label = ~transDensity,
                        group = "Transportation and Warehousing Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palEdu(edDensity),
                        label = ~edDensity,
                        group = "Educational Services Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palHealth(heatlhDensity),
                        label = ~heatlhDensity,
                        group = "Health Care and Social Assistance Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palFood(foodDensity),
                        label = ~foodDensity,
                        group = "Accommodation and Food Services Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palConstruct(constructDensity),
                        label = ~constructDensity,
                        group = "Construction Trade Density") %>%
            addLayersControl(
                baseGroups = c("Average Commute", "Median Household Income",
                               "Unemployment Density", "High School Drop Out Density",
                               "Retail Trade Density", "Transportation and Warehousing Density",
                               "Educational Services Density", "Health Care and Social Assistance Density",
                               "Accommodation and Food Services Density", "Construction Trade Density"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    ## Contronl Ecoomic Vulnerability Legend
    observeEvent(input$evMap_groups,{
        evMap <- leafletProxy("evMap", data = nyct_economic)
        evMap %>% clearControls()
        if (input$evMap_groups == "Average Commute") {
            evMap %>% addLegend(pal = palCommute, values = ~nyct_economic$AvgCommute, 
                                opacity = 1, position="topright", title = "Average Commute")
        }
        else if (input$evMap_groups == "Median Household Income") {
            evMap %>% addLegend(pal = palMedHHIncome, values = ~nyct_economic$medHHIncome, 
                                opacity = 1, position="topright", title = "Median Household Income")
        }
        else if (input$evMap_groups == "Unemployment Density") {
            evMap %>% addLegend(pal = palUnemp, values = ~nyct_economic$unempDensity, 
                                opacity = 1, position="topright", title = "Unemployment Density")
        }
        else if (input$evMap_groups == "High School Drop Out Density") {
            evMap %>% addLegend(pal = palDropOut, values = ~nyct_economic$dropOutDensity, 
                                opacity = 1, position="topright", title = "High School Drop Out Density")
        }
        else if (input$evMap_groups == "Retail Trade Density" ) {
            evMap %>% addLegend(pal = palRetail, values = ~nyct_economic$retailDensity, 
                                opacity = 1, position="topright", title = "Retail Trade Density" )
        }
        else if (input$evMap_groups == "Transportation and Warehousing Density" ) {
            evMap %>% addLegend(pal = palTrans, values = ~nyct_economic$transDensity, 
                                opacity = 1, position="topright", title = "Transportation and Warehousing Density" )
        }
        else if (input$evMap_groups == "Educational Services Density") {
            evMap %>% addLegend(pal = palEdu, values = ~nyct_economic$edDensity, 
                                opacity = 1, position="topright", title = "Educational Services Density")
        }
        else if (input$evMap_groups == "Health Care and Social Assistance Density") {
            evMap %>% addLegend(pal = palHealth, values = ~nyct_economic$heatlhDensity, 
                                opacity = 1, position="topright", title = "Health Care and Social Assistance Density")
        }
        else if (input$evMap_groups == "Accommodation and Food Services Density") {
            evMap %>% addLegend(pal = palFood, values = ~nyct_economic$foodDensity, 
                                opacity = 1, position="topright", title = "Accommodation and Food Services Density")
        }
        else if (input$evMap_groups == "Construction Trade Density") {
            evMap %>% addLegend(pal = palConstruct, values = ~nyct_economic$constructPercent, 
                                opacity = 1, position="topright", title = "Construction Trade Density")
        }
    })
    
    ## Map for Pop Demographcis
    output$demoMap <- renderLeaflet({
        leaflet(data = nyct_demogrpahics, options = leafletOptions(zoomControl = TRUE)) %>% 
            setView(lat = 40.7445698, lng = -73.9375989, zoom = 11) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            addCircleMarkers( stroke = .5, radius = .75,
                              color = ~palPop(popDensity),
                              label = ~popDensity,
                              group = "Population Density") %>% 
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palBlack(blackDensity),
                        label = ~blackDensity,
                        group = "Black Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palWhite(whiteDensity),
                        label = ~whiteDensity,
                        group = "White Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palAsian(asianDensity),
                        label = ~asianDensity,
                        group = "Asian Density") %>%
            addCircleMarkers( stroke = .5, radius = .75,
                        color = ~palOther(otherDensity),
                        label = ~otherDensity,
                        group = "Other Race Density") %>% 
            addLayersControl(
                baseGroups = c("Population Density", "Black Density", "White Density",
                               "Asian Density", "Other Race Density"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    ## Contronl Pop Demographics Legend
    observeEvent(input$demoMap_groups,{
        demoMap <- leafletProxy("demoMap", data = nyct_demogrpahics)
        demoMap %>% clearControls()
        if (input$demoMap_groups == "Black Density") {
            demoMap %>% addLegend(pal = palBlack, values = ~nyct_demogrpahics$blackDensity, 
                                opacity = 1, position="topright", title = "Black Density")
        }
        else if (input$demoMap_groups == "White Density") {
            demoMap %>% addLegend(pal = palWhite, values = ~nyct_demogrpahics$whitePercent, 
                                opacity = 1, position="topright", title = "White Density")
        }
        else if (input$demoMap_groups == "Asian Density") {
            demoMap %>% addLegend(pal = palAsian, values = ~nyct_demogrpahics$asianPercent, 
                                opacity = 1, position="topright", title = "Asian Density")
        }
        else if (input$demoMap_groups == "Other Race Density") {
            demoMap %>% addLegend(pal = palOther, values = ~nyct_demogrpahics$otherPercent, 
                                opacity = 1, position="topright", title = "Other Race Density")
        }
        else if (input$demoMap_groups == "Population Density") {
            demoMap %>% addLegend(pal = palPop, values = ~nyct_demogrpahics$popDensity, 
                                  opacity = 1, position="topright", title = "Population Density")
        }
    })
    
}