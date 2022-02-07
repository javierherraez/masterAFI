library(leaflet)
library(jsonlite)

geojson <- readLines('Shiny_Leaflet/datos_ejercicios/comunidades.json', warn = FALSE) %>%
  paste(collapse = '\n') %>%
  fromJSON(simplifyVector = FALSE)


############################################

# Default styles for all features

geojson$style = list(
  weight = 1,
  color = "#555555",
  opacity = 1,
  fillOpacity = 0.5
)

properties <- lapply(geojson$features, function(feat){ feat$properties })
turistas <- sapply(properties, function(prop){ prop$turistas })

pal <- colorNumeric('Greens', turistas)

# Style for each polygon

geojson$features <- lapply(geojson$features, function(feat) { 
    feat$properties$style <- list(fillColor = pal(feat$properties$turistas))
    return(feat)
  })

leaflet() %>% setView(lng = -3.68, lat = 40.43, zoom = 5) %>%
              addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
              addGeoJSON(geojson)  %>% 
              addLegend("bottomright", pal = pal, values = turistas,
                                            title = "Número de turistas",
                                            opacity = 1)
