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
            dyShading(from = "1912-1-1", to = "2022-1-1", color = "#222D32") %>% 
            dyRangeSelector(dateWindow = c(min(stats_date$week), max(stats_date$week))) %>% 
            dySeries("points", color="#e41a1c", label = "Puntos", strokeWidth = 3, drawPoints = T, pointSize = 3) %>%
            dySeries("assists", color="#377eb8", label = "Asistencias", strokeWidth = 3, drawPoints = T, pointSize = 3) %>%
            dySeries("blocks", color="#4daf4a", label = "Bloqueos", strokeWidth = 3, drawPoints = T, pointSize = 3) %>%
            dySeries("rebounds", color="#984ea3", label = "Rebotes", strokeWidth = 3, drawPoints = T, pointSize = 3) %>%
            dySeries("steals", color="#ff7f00", label = "Robos", strokeWidth = 3, drawPoints = T, pointSize = 3) %>%
            dySeries("turnovers", color="#ffd92f", label = "Perdidas", strokeWidth = 3, drawPoints = T, pointSize = 3) %>% 
            dySeries("n_games", strokePattern="dotted", color="#a65628", label = "N Partidos", strokeWidth = 3)%>% 
            dyLegend(labelsDiv = "legenddygraph"))
}

draw_map <- function(positions, conferences, divisions){
  map_df <- nba_df %>% 
    filter(grepl(paste(positions, collapse="|"), player_position)) %>% 
    group_by(team_name) %>%
    summarise(salary = sum(salary, na.rm = T) / n_distinct(id))

  pal <- colorNumeric(rev(heat.colors(10)), map_df$salary)
  
  geojson$features <- lapply(geojson$features, function(feat){
    feat$properties$style <- list(fillColor = pal(map_df$salary[map_df$team_name == feat$properties$team]))
    feat$properties$popup <- paste('<b>Equipo:</b>', feat$properties$cit, feat$properties$team,
                                   '<br><b>Estadio:</b>', feat$properties$arena,
                                   '<br><b>Salario medio:</b>',
                                   format(round(map_df$salary[map_df$team_name == feat$properties$team], digits = 0), 
                                          big.mark=".", decimal.mark = ","), "$")
    return(feat)
  })
  
  geojson_draw_map <- c()
  geojson_draw_map$type <- geojson$type
  geojson_draw_map$style <- geojson$style
  geojson_draw_map$features <- c()
  for (feat in 1:length(geojson$features)){
    if ((geojson$features[[feat]]$properties$conference %in% conferences) &
        (geojson$features[[feat]]$properties$division %in% divisions)){
      geojson_draw_map$features[[length(geojson_draw_map$features) + 1]] <- geojson$features[[feat]]
    }
  }
  
  return (leaflet() %>% 
            setView(lng = -102, lat = 38, zoom = 4) %>%
            addTiles(urlTemplate = 'https://cartocdn_{s}.global.ssl.fastly.net/base-midnight/{z}/{x}/{y}.png') %>%
            addGeoJSON(geojson_draw_map) %>% 
            addCircleMarkers(
              lng = sapply(geojson_draw_map$features, function(feat) {
                return(feat$geometr$coordinates[[1]])
              }),
              lat = sapply(geojson_draw_map$features, function(feat) {
                return(feat$geometr$coordinates[[2]])
              }),
              color = sapply(geojson_draw_map$features, function(feat) {
                return(feat$properties$style$fillColor)
              })
            ) %>% 
            leaflet::addLegend("bottomright", pal = pal, values = map_df$salary, title = "Salario",opacity = 1)
    )
}

