library(xts)

draw_dygraph <- function(df, team_filtre) {
  stats_date <- df
  if (team_filtre != "TODOS") {
    stats_date <- stats_date %>%
      filter(team_name == team_filtre)
  }
  
  stats_date <- stats_date %>%
    group_by(week = cut(date, "week")) %>%
    summarise(
      points = sum(points, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
      assists = sum(assists, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
      blocks = sum(blocks, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
      rebounds = sum(rebounds, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
      steals = sum(steals, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
      turnovers = sum(turnovers, na.rm = T) / n_distinct(game_id) / ifelse(team_filtre != "TODOS", 1, 2),
      n_games = n_distinct(game_id)
    )
  
  stats_date$week <- as.Date(stats_date$week)
  
  ts_stats_date <- xts(stats_date[-1], order.by = stats_date$week)
  
  return (
    dygraph(
      ts_stats_date,
      width = 500,
      height = 500,
      main = "Weekly Average"
    ) %>%
      dyShading(from = "1912-1-1", to = "2022-1-1", color = "#222D32") %>%
      dyRangeSelector(dateWindow = c(
        min(stats_date$week), max(stats_date$week)
      )) %>%
      dySeries(
        "points",
        color = "#e41a1c",
        label = "Puntos",
        strokeWidth = 3,
        drawPoints = T,
        pointSize = 3
      ) %>%
      dySeries(
        "assists",
        color = "#377eb8",
        label = "Asistencias",
        strokeWidth = 3,
        drawPoints = T,
        pointSize = 3
      ) %>%
      dySeries(
        "blocks",
        color = "#4daf4a",
        label = "Bloqueos",
        strokeWidth = 3,
        drawPoints = T,
        pointSize = 3
      ) %>%
      dySeries(
        "rebounds",
        color = "#984ea3",
        label = "Rebotes",
        strokeWidth = 3,
        drawPoints = T,
        pointSize = 3
      ) %>%
      dySeries(
        "steals",
        color = "#ff7f00",
        label = "Robos",
        strokeWidth = 3,
        drawPoints = T,
        pointSize = 3
      ) %>%
      dySeries(
        "turnovers",
        color = "#ffd92f",
        label = "Perdidas",
        strokeWidth = 3,
        drawPoints = T,
        pointSize = 3
      ) %>%
      dySeries(
        "n_games",
        strokePattern = "dotted",
        color = "#a65628",
        label = "N Partidos",
        strokeWidth = 3
      ) %>%
      dyLegend(labelsDiv = "legenddygraph")
  )
}

draw_map <- function(df,json,positions,conferences,divisions) {
  salary_df <- df %>%
    filter(grepl(paste(positions, collapse = "|"), player_position)) %>%
    group_by(team_name) %>%
    summarise(salary = sum(salary, na.rm = T) / n_distinct(id))
  
  pal <- colorNumeric(rev(heat.colors(10)), salary_df$salary)
  
  geojson_draw_map <- c()
  geojson_draw_map$type <- json$type
  geojson_draw_map$features <- c()
  i = 1
  for (feat in json$features) {
    if ((feat$properties$conference %in% conferences) &
        (feat$properties$division %in% divisions)) {
      
      geojson_draw_map$features[[i]] <- feat
      
      geojson_draw_map$features[[i]]$properties$style <-
        list(fillColor = pal(salary_df$salary[salary_df$team_name == feat$properties$team]))
      
      geojson_draw_map$features[[i]]$properties$popup <-
        paste('<b>Equipo:</b>',feat$properties$cit,feat$properties$team,
              '<br><b>Estadio:</b>', feat$properties$arena,
              '<br><b>Salario medio:</b>', format(
                round(salary_df$salary[salary_df$team_name == feat$properties$team], digits = 0),
                big.mark = ".",
                decimal.mark = ","),"$")
      i = i + 1
    }
  }
  
  df_map <- as.data.frame(t(sapply(geojson_draw_map$features, function(feat) {
    return(
      c(long = feat$geometr$coordinates[[1]],
        lat = feat$geometr$coordinates[[2]],
        color = feat$properties$style$fillColor)
      )
    })))
  
  df_map$lat <- as.numeric(df_map$lat)
  df_map$long <- as.numeric(df_map$long)
  
  return (
    leaflet(df_map) %>%
      setView(lng = -102, lat = 38, zoom = 4) %>%
      addTiles(urlTemplate = 'https://cartocdn_{s}.global.ssl.fastly.net/base-midnight/{z}/{x}/{y}.png') %>%
      addGeoJSON(geojson_draw_map) %>%
      addCircleMarkers(
        lng = ~ long,
        lat = ~ lat,
        color = ~ color,
        opacity = 1,
        fillOpacity  = 0.5
      ) %>%
      leaflet::addLegend("bottomright", pal = pal, values = salary_df$salary, title = "Salario", opacity = 1)
  )
}


draw_barplot <- function(df, entry_stat){
  entry_stat <- "points"
  entry_var <- "team_name"
  var_sym <- rlang::sym(entry_var)
  stat_sym <- rlang::sym(entry_stat)
  
  stat_df <- nba_df %>%
    select({{entry_var}}, {{entry_stat}}) %>% 
    group_by({{var_sym}}) %>% 
    summarise("stat" := mean({{stat_sym}}, na.rm = T))
  # 
  # nba_df %>% 
  #   do(Split.position("-"))
  #   filter(player_position != "") %>% 
  #   mutate(player_position = function(player_position) unlist(strsplit(player_position, '-'))) %>% 
  #   group_by(player_position) %>% summarise("stat" = mean(points))
  return(ggplot(stat_df, aes(reorder(team_name, -stat), stat)) +
           geom_col())
}


