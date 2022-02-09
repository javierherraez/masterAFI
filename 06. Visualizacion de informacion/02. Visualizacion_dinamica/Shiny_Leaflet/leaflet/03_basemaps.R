library(leaflet)

# Basemaps: https://leaflet-extras.github.io/leaflet-providers/preview/


leaflet() %>% 
  setView(lng = -3.68, lat = 40.43, zoom = 10) %>%
  addTiles(layerId = 'https://{s}.tile.jawg.io/jawg-dark/{z}/{x}/{y}{r}.png') %>%
  addMarkers(lng = -3.6878, lat = 40.4309, popup = 'Afi Escuela de Finanzas')
  
leaflet() %>% 
  setView(lng = -3.68, lat = 40.43, zoom = 10) %>%
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}') %>%
  addMarkers(lng = -3.6878, lat = 40.4309, popup = 'Afi Escuela de Finanzas')

