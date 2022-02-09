library(leaflet)

multas <- read.csv2('multas.csv',
                    header = T, stringsAsFactors = F, fileEncoding = 'utf-8')

multas$lon <- as.numeric(multas$lon)
multas$lat <- as.numeric(multas$lat)



pal <- colorFactor(
  palette = "Set1",
  domain = multas$CAT
)

leaflet(data = multas) %>% 
              setView(lng = -3.68, lat = 40.43, zoom = 10) %>%
              addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
              addCircles(~lon, ~lat, 
                         popup = ~paste0('<b>Mes:</b> ', as.character(MES),
                                         '<br><b>Lugar:</b> ', LUGAR), 
                         radius = 50,
                         color = ~pal(CAT),
                         opacity = 0.2,
                         weight = 0) %>%
              addLegend("bottomright", pal = pal, values = ~CAT,
                        title = "Tipo de multa",
                        opacity = 1)
