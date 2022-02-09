library(leaflet)
library(dplyr)

# A simple map with a marker

leaflet() %>% addTiles() %>%
              addMarkers(lng = -3.6878, lat = 40.4309,
                         popup = 'Afi Escuela de Finanzas')