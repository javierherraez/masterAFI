#############################################
# José Manuel Rodríguez Madrid              #
# Datos abiertos haciendo Web Scraping      #
#############################################
library(rvest)
library(ggplot2)
library(tidyverse)

get_series_season_url <- function(series, season) {
  return(paste0('https://www.imdb.com/title/', series, '/episodes', season))
}

scrape_series_data <- function(series, verbose) {
  data <- list()
  season_param <- '?season=1'
  
  repeat {
    season_number <- str_extract(season_param, '(\\d)+')
    
    if (verbose) {
      print(paste('Descargando temporada', season_number))  
    }
    
    imdb_series_season_url <- get_series_season_url(series, season_param)
    
    imdb_series_season_web <- read_html(imdb_series_season_url, encoding = 'UTF-8')
    
    episode_number <- imdb_series_season_web %>% 
      html_nodes('div.info') %>% 
      html_nodes('[itemprop=\'episodeNumber\']') %>% 
      html_attr('content')
    
    episode_title <- imdb_series_season_web %>% 
      html_nodes('div.info') %>% 
      html_nodes('strong') %>% 
      html_text()
    
    episode_rating <- imdb_series_season_web %>% 
      html_nodes('div.info') %>% 
      html_nodes('div.ipl-rating-widget') %>% 
      html_nodes('div.ipl-rating-star.small') %>% 
      html_nodes('span.ipl-rating-star__rating') %>% 
      html_text()
    
    max_length <- max(
      length(episode_number),
      length(episode_title), 
      length(episode_rating)
    )
    
    episode_rating <- c(
      episode_rating, 
      rep(NA, max_length - length(episode_rating))
    )
    
    season <- data.frame(
      season = as.factor(season_number), 
      episode = as.factor(episode_number), 
      title = episode_title, 
      rating = as.numeric(episode_rating), 
      stringsAsFactors = F
    )
    
    data <- append(data, list(season))
    
    season_param <- imdb_series_season_web %>% 
      html_nodes('#load_next_episodes') %>% 
      html_attr('href')
    
    if (length(season_param) == 0)
      break
  }
  
  data <- do.call(rbind, data)
  
  return(data)
}

series <- 'tt1124373'

tv_series_data <- scrape_series_data(series, TRUE)

write_csv(tv_series_data, '.\\rvest_imdb\\imdb.csv' )

ggplot(tv_series_data, aes(x = season, y = rating)) +
  geom_boxplot()

ggplot(tv_series_data, aes(x = rating, y = 1, fill = season)) +
  geom_dotplot(stackgroups = TRUE, method = 'histodot', binwidth = 0.1)
