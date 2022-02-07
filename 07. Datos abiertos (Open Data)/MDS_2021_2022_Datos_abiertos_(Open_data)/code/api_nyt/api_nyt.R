library(rjson)
library(httr)

get_nyt_most_popular_url <- function(service, time_period) {
  return(paste0('https://api.nytimes.com/svc/mostpopular/v2/', service, '/', time_period, '.json'))
}

download_nyt_most_popular <- function(service, time_period, api_key) {
  nyt_most_popular_url <- get_nyt_most_popular_url(service, time_period)
  
  nyt_most_popular_response <- GET(nyt_most_popular_url, query = list(`api-key` = api_key))
  
  if (nyt_most_popular_response$status_code != 200) {
    return(NULL)
  }
  
  return(rjson::fromJSON(content(nyt_most_popular_response, as = 'text')))
}


api_key_nyt_most_popular <- ''
time_period <- '30'

service <- 'viewed'
nyt_most_viewed <- download_nyt_most_popular(service, time_period, api_key_nyt_most_popular)

service <- 'shared'
nyt_most_shared <- download_nyt_most_popular(service, time_period, api_key_nyt_most_popular)

service <- 'emailed'
nyt_most_mailed <- download_nyt_most_popular(service, time_period, api_key_nyt_most_popular)

nyt_most_popular <- c(nyt_most_viewed$results, nyt_most_shared$results, nyt_most_mailed$results)

write(toJSON(nyt_most_popular, indent = 4), '.\\api_nyt\\news.json')
