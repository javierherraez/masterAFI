###################################
# lapply                          #
###################################

# lapply
nyc <- list(pop = 8405837,
            boroughs = c("Manhattan", "Bronx", "Brooklyn",
                         "Queens", "Staten Island"),
            capital = FALSE)

# Usando bucle
for (info in nyc) {
  print(class(info))
}

# Usando lapply
lapply(nyc, class)


cities <- c("New York", "Paris", "London", "Tokyo", 
            "Rio de Janeiro", "Cape Town")

num_chars <- c()
for (i in 1:length(cities)) {
  num_chars[i] <- nchar(cities[i])
}

num_chars

lapply(cities, nchar) #Ojo, siempre devuelve una lista

unlist(lapply(cities, nchar)) #Con unlist, pasamos a vector


# Con funciones propias
oil_prices <- list(2.37, 2.49, 2.18, 2.22, 2.47, 2.32)
triple <- function(x) {
  x * 3
}
unlist(lapply(oil_prices, triple))

# Con funciones propias pasando argumentos
oil_prices <- list(2.37, 2.49, 2.18, 2.22, 2.47, 2.32)
multiply <- function(x, factor) {
  x * factor
}
unlist(lapply(oil_prices, multiply, factor = 3))
