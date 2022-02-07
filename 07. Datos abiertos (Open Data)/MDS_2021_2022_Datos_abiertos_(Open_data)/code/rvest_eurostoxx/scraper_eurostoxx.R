library(rvest)
library(lubridate)
library(tidyverse)
library(httr)

process_timestamp <- function (dates) {
  dates <- str_replace_all(
    dates, 
    c('^([0-9]{1,2}/[0-9]{1,2})$' = paste(paste('\\1', 
                                                as.character(year(Sys.Date())), 
                                                sep = '/'), '23:59'), 
      '^([0-9]{1,2}:[0-9]{1,2})$' = paste(format(Sys.Date(), '%d/%m/%Y'), '\\1')
    )
  )
  dates <- dmy_hm(dates)
  dates <- replace_na(dates, Sys.time())
  
  return(dates)
}

scrape_eurostoxx_data <- function(verbose) {
  url_expansion <- 'http://www.expansion.com/mercados/cotizaciones/indices/eurostoxx_I.5E.html'
  eurostoxx_table_columns <- c('stock', 'last', 'var_perc', 'var', 'ytd', 'max', 
                               'min', 'volume', 'capitalization', 'timestamp')

  expansion_request <- GET(url_expansion)
  
  if (expansion_request$status_code != 200) {
    eurostoxx_table <- data.frame(character(), numeric(), numeric(), numeric(), 
                                  numeric(), numeric(), numeric(), numeric(), 
                                  numeric(), POSIXct())
    
    colnames(eurostoxx_table) <- eurostoxx_table_columns
    return(eurostoxx_table)
  }
  
  expansion_web <- read_html(content(expansion_request, as = 'text'), 
                             encoding = 'ISO-8859-15')

  expansion_tables <- html_nodes(expansion_web, 'table#listado_valores')
  
  eurostoxx_table <- html_table(expansion_tables[[1]], fill = TRUE, dec = ',')

  if (verbose) {
    print(head(eurostoxx_table))
  }

  eurostoxx_table <- eurostoxx_table[, 1:ncol(eurostoxx_table) - 1]
  
  colnames(eurostoxx_table) <- eurostoxx_table_columns
  
  eurostoxx_table <- eurostoxx_table %>% 
    as_tibble()
  
  if (verbose) {
    print(sapply(eurostoxx_table, class))
  }
  
  eurostoxx_table <- eurostoxx_table %>%
    mutate(
      across(
        c(last, max, min, volume, capitalization), 
        ~parse_number(.x, locale = locale(decimal_mark = ',', grouping_mark = '.'))
      )
    )
  
  eurostoxx_table <- eurostoxx_table %>%
    mutate(across(c(var_perc, ytd), function(x) x / 100))
  
  eurostoxx_table <- eurostoxx_table %>%
    mutate(timestamp = process_timestamp(timestamp))
  
  if (verbose) {
    print(sapply(eurostoxx_table, class))
  }
  return(eurostoxx_table)
}

eurostoxx_data <- scrape_eurostoxx_data(T)

write_csv(eurostoxx_data, '.\\rvest_eurostoxx\\eurostoxx.csv' )