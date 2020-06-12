library(shiny)

ui <-  bootstrapPage(    
    tags$style(type = "text/css", ".leaflet {height: calc(100vh - 80px) !important;}"),
    navbarPage(theme = shinythemes::shinytheme("slate"), collapsible = TRUE,
                 "COVID-19 Mapping",
                 tabPanel("Critical Infrastructure",
                              leafletOutput("ciMap"),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            h4("Grocery Stores by Borough", align = "center"),
                                            plotOutput('groceryB', height = 250))
                          ),
                 tabPanel("Economic Vulnerability",
                          leafletOutput("evMap")),
                 tabPanel("Health Vulnerability"),
               tabPanel("Population Demographics",
                        leafletOutput("demoMap")),
                 tabPanel("Resiliency"),
                 tabPanel("About")
)
)