library(leaflet)

# A simple map with a marker, fix center and zoom

leaflet() %>% setView(lng = -3.68, lat = 40.43, zoom = 10) %>%
  addTiles() %>%
  addMarkers(lng = -3.6878, lat = 40.4309, popup = 'Afi Escuela de Finanzas')

# A simple map with a marker, fix bounds

map <- leaflet() %>% fitBounds(lng1 = -3.69, lat1 = 40.43, lng2 = -3.68, lat2 = 40.44) %>%
  addTiles() %>%
  addMarkers(lng = -3.6878, lat = 40.4309, popup = 'Afi Escuela de Finanzas')

map

# A simple map with a marker, automatic bounds

map %>% clearBounds %>%
  addTiles() %>%
  addMarkers(lng = -3.6878, lat = 40.4309, popup = 'Afi Escuela de Finanzas')
