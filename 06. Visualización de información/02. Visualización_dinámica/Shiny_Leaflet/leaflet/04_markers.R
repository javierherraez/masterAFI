library(leaflet)

data(quakes)
str(quakes)

# Weird things about coordinates: http://stackoverflow.com/questions/19879746/why-are-datasetquakes-longtitude-values-above-180

# A marker at each point

leaflet(data = quakes) %>% addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
                           addMarkers(~long, ~lat, popup = ~as.character(stations))
              
  
# Choose different markers

leaflet(data = quakes) %>% addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
                           addMarkers(~long, ~lat, clusterOptions = markerClusterOptions(), popup = ~as.character(stations))
# Something a bit more complicated

pal <- colorNumeric(
  palette = "Greens",
  domain = quakes$mag
)

popup <- paste0('<b>Stations:</b> ', as.character(quakes$stations), '<br>',
                '<b>Magnitude:</b>', as.character(quakes$mag))

leaflet(data = quakes) %>% 
        addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
        addCircles(~long, ~lat, 
             popup = ~ popup,
             color = ~ pal(mag), 
             fill = T, 
             fillOpacity = 0.5, 
             weight = 0, 
             radius = 40)

