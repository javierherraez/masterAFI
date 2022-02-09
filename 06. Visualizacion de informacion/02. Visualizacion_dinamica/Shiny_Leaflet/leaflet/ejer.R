library(leaflet)

multas <- read.csv("Shiny_Leaflet/datos_ejercicios/multas.csv", sep = ";", encoding="UTF-8")
head(multas)
              
pal <- colorFactor(
  palette = "Paired",
  domain = multas$CAT
)

popup <- paste0('<b>Mes:</b> ', as.character(multas$MES), '<br>',
                '<b>Lugar:</b>', as.character(multas$LUGAR))

leaflet(data = multas) %>% 
        addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
        addCircles(~lon, ~lat, 
             popup = ~ popup,
             color = ~ pal(CAT), 
             fill = T, 
             fillOpacity = 0.5, 
             weight = 0, 
             radius = 40) %>%
        addLegend("bottomright", pal = pal, values = ~CAT,
          title = "Tipo de multa",
          opacity = 1) %>%
        setView(lng = -3.68, lat = 40.43, zoom = 12.5)

