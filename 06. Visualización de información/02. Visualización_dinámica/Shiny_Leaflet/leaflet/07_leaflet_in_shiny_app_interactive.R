library(shiny)
library(leaflet)
library(jsonlite)

ui <- fluidPage(
  selectInput(
    inputId = 'variable',
    label = 'Elija variable a representar',
    choices = c('turistas', 'longitud', 'latitud')
  ),
  
  leafletOutput(outputId = 'mapa')
)

server <- function(input, output){
  output$mapa <- renderLeaflet({
    geojson <- readLines('Shiny_Leaflet/datos_ejercicios/comunidades.json', warn = FALSE) %>%
      paste(collapse = '\n') %>%
      fromJSON(simplifyVector = FALSE)
    
    # Default styles for all features
    
    geojson$style = list(
      weight = 1,
      color = "#555555",
      opacity = 1,
      fillOpacity = 0.5
    )
    
    properties <- lapply(geojson$features, function(feat){ feat$properties })
    
    variable <- c()
    
    if(input$variable == 'turistas'){
      variable <- sapply(properties, function(prop){ prop$turistas })
    } else if(input$variable == 'longitud'){
      variable <- sapply(properties, function(prop){ prop$lon})
    } else{
      variable <- sapply(properties, function(prop){ prop$lat})
    }
    
    pal <- colorNumeric('Greens', variable)
    
    # Style for each polygon
    
    if(input$variable == 'turistas'){
      geojson$features <- lapply(geojson$features, function(feat) { 
        feat$properties$style <- list(fillColor = pal(feat$properties$turistas))
        return(feat)
      })
    } else if(input$variable == 'longitud'){
      geojson$features <- lapply(geojson$features, function(feat) { 
        feat$properties$style <- list(fillColor = pal(feat$properties$lon))
        return(feat)
      })
    } else{
      geojson$features <- lapply(geojson$features, function(feat) { 
        feat$properties$style <- list(fillColor = pal(feat$properties$lat))
        return(feat)
      })
    }
    
    leaflet() %>% setView(lng = -3.68, lat = 40.43, zoom = 4) %>%
                  addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
                  addGeoJSON(geojson)  %>% 
                  addLegend("bottomright", pal = pal, values = variable,
                            title = input$variable, opacity = 1)
  })
}

shinyApp(ui = ui, server = server)
