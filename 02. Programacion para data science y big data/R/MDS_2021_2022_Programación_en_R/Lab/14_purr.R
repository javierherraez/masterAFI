###################################
#  PURRR                          #
###################################
library(purrr)
library(dplyr)
# map
nyc <- list(pop = 8405837,
            boroughs = c("Manhattan", "Bronx", "Brooklyn",
                         "Queens", "Staten Island"),
            capital = FALSE)

# lapply
lapply(nyc, class)
# Usando lapply
map(nyc, class)


cities <- c("New York", "Paris", "London", "Tokyo", 
            "Rio de Janeiro", "Cape Town")

num_chars <- c()
for (i in 1:length(cities)) {
  num_chars[i] <- nchar(cities[i])
}

num_chars

lapply(cities, nchar)
map(cities, nchar) 

unlist(map(cities, nchar)) #Con unlist, pasamos a vector


# Con funciones propias
oil_prices <- list(2.37, 2.49, 2.18, 2.22, 2.47, 2.32)
triple <- function(x) {
  x * 3
}

unlist(map(oil_prices, triple))

# Con funciones propias pasando argumentos
oil_prices <- list(2.37, 2.49, 2.18, 2.22, 2.47, 2.32)
multiply <- function(x, factor) {
  x * factor
}

unlist(map(oil_prices, function(x) multiply(x, factor = 3)))
unlist(map(oil_prices, multiply, factor = 3))
unlist(map(oil_prices, ~multiply(.x, factor = 3)))


cities <- c("New York", "Paris", "London", "Tokyo", 
            "Rio de Janeiro", "Cape Town")

vapply(cities, nchar, numeric(1))
map_dbl(cities, nchar)

first_and_last <- function(name) {
  name <- gsub(" ", "", name)
  letters <- strsplit(name, split = "")[[1]]
  c(first = min(letters), last = max(letters))
}

vapply(cities, first_and_last, character(2))
map_chr(cities, first_and_last) # Error!
map_df(cities, first_and_last)
