library(leaflet)

data(quakes)
str(quakes)

pal <- colorNumeric(
  palette = "Greens",
  domain = quakes$mag
)

leaflet(data = quakes) %>% addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
  addCircles(~long, ~lat, 
             popup = ~paste0('<b>Stations:</b> ', as.character(stations), '<br>',
                             '<b>Magnitude:</b>', as.character(mag)),
             color = ~pal(mag), fill = T, fillOpacity = 0.5, weight = 0, radius = 20) %>%
  addLegend("bottomright", pal = pal, values = ~mag,
            title = "Magnitude",
            opacity = 1
  )
