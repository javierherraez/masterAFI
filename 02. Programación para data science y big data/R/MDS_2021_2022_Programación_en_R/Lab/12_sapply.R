###################################
# sapply                          #
###################################

# sapply
cities <- c("New York", "Paris", "London", "Tokyo", 
            "Rio de Janeiro", "Cape Town")

sapply(cities, nchar) # Obtenemos un vector

sapply(cities, nchar, USE.NAMES = FALSE)

first_and_last <- function(name) {
  name <- gsub(" ", "", name)
  letters <- strsplit(name, split = "")[[1]]
  c(first = min(letters), last = max(letters))
}

sapply(cities, first_and_last) # Obtenemos una matriz

# ¿Qué sucede si no puede simplificar?
unique_letters <- function(name) {
  name <- gsub(" ", "", name)
  letters <- strsplit(name, split = "")[[1]]
  unique(letters)
}

sapply(cities, unique_letters) # No puede simplificar y devuelve otra lista

lapply(cities, unique_letters)
