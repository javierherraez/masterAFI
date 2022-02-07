library(xts)
draw_dygraph <- function(team_filtre){
  stats_date <- nba_df
  
  if(team_filtre != "TODOS"){
    stats_date <- stats_date %>%
      filter(team_name == team_filtre)
  }
  
  stats_date <- stats_date %>%
    group_by(week = cut(date, "week")) %>% 
    summarise(points = sum(points, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
              assists = sum(assists, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
              blocks = sum(blocks, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
              rebounds = sum(rebounds, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
              steals = sum(steals, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
              turnovers = sum(turnovers, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
              n_games = n_distinct(game_id))
  
  stats_date$week <- as.Date(stats_date$week)
  
  ts_stats_date <- xts(stats_date[-1], order.by = stats_date$week)
  
  return (dygraph(ts_stats_date, width = 500, height = 500, main = "Weekly Average")%>% 
            dyRangeSelector(dateWindow = c(min(stats_date$week), max(stats_date$week))) %>% 
            dySeries("points", color="#e41a1c", label = "Puntos", strokeWidth = 3) %>%
            dySeries("assists", color="#377eb8", label = "Asistencias", strokeWidth = 3) %>%
            dySeries("blocks", color="#4daf4a", label = "Bloqueos", strokeWidth = 3) %>%
            dySeries("rebounds", color="#984ea3", label = "Rebotes", strokeWidth = 3) %>%
            dySeries("steals", color="#ff7f00", label = "Robos", strokeWidth = 3) %>%
            dySeries("turnovers", color="#ffd92f", label = "Perdidas", strokeWidth = 3) %>% 
            dySeries("n_games", strokePattern="dotted", color="#a65628", label = "Partidos", strokeWidth = 3)%>% 
            dyLegend(labelsDiv = "legenddygraph"))
}

draw_map <- function(){
  # ees %>% filter(var_sexo %in% input$sexos) %>%
  #   filter(var_tipocontrato %in% input$contratos) %>%
  #   group_by(var_provincia) %>%
  #   summarise(salario = sum(salario_bruto_anual_medio*factor_elev)/sum(factor_elev)) -> map_df
  # 
  # pal <- colorNumeric('Greens', map_df$salario)
  # geojson$features <- lapply(geojson$features, function(feat){
  #   provincia <- as.numeric(feat$properties$id)
  #   feat$properties$style <- list(fillColor = pal(map_df$salario[map_df$var_provincia == provincia]))
  #   feat$properties$popup <- paste0('<b>Provincia:</b> ', feat$properties$description,
  #                                   '<br><b>Salario medio:</b> ',
  #                                   round(map_df$salario[map_df$var_provincia == provincia]))
  #   return(feat)
  # })
  
  return (leaflet() %>% 
            # setView(lng = -3.68, lat = 40.43, zoom = 5) %>%
            addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
            addGeoJSON(geojson)# %>% 
            #addLegend("bottomright", pal = pal, values = map_df$salario,title = "Salario",opacity = 1)
            )
}