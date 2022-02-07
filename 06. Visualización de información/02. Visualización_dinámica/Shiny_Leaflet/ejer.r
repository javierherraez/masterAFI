library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = 'Probability distributions',
                  titleWidth = 250),
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem('Salarios Nacional', tabName = 'nacional'),
                     menuItem('Salarios Povincias', tabName = 'provincias')
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'provincias',
              fluidRow(
                column( 4, 
                        fluidRow(sidebarPanel(checkboxGroupInput("sex", "Sexo:",
                                                  c("Hombre" = "norm",
                                                    "Mujeres" = "unif"))
                                )
                        ),
                        fluidRow(sidebarPanel(checkboxGroupInput("contract", "Tipo Contrato:",
                                                  c("Indefinido" = "undefined",
                                                    "Temporal" = "temp"))
                                )
                        )
                ),
                column(8,leafletOutput(outputId = 'mapa'))
              )
      )
    )
  )
)

salarios <- read.csv("Shiny_Leaflet/datos_ejercicios/ees2014.tsv", sep = "\t", encoding="UTF-8")
geojson <- readLines('Shiny_Leaflet/datos_ejercicios/provinciasSimplified.json', warn = FALSE) %>%
      paste(collapse = '\n') %>%
      fromJSON(simplifyVector = FALSE)

server <- function(input, output){
output$mapa <- renderLeaflet({
    
    geojson$style = list(
      weight = 1,
      color = "#555555",
      opacity = 1,
      fillOpacity = 0.5
    )
    
    properties <- lapply(geojson$features, function(feat){ feat$properties })
  
    leaflet() %>% setView(lng = -3.68, lat = 40.43, zoom = 4) %>%
                  addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
                  addGeoJSON(geojson)  %>% 
                  addLegend("bottomright", pal = pal, values = variable,
                            title = input$variable, opacity = 1)
  })
}

shinyApp(ui = ui, server = server)
