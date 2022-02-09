###################################
# vapply                          #
###################################

# vapply
cities <- c("New York", "Paris", "London", "Tokyo", 
            "Rio de Janeiro", "Cape Town")

vapply(cities, nchar, numeric(1))

first_and_last <- function(name) {
  name <- gsub(" ", "", name)
  letters <- strsplit(name, split = "")[[1]]
  c(first = min(letters), last = max(letters))
}

vapply(cities, first_and_last, character(2))

vapply(cities, first_and_last, character(1)) #Error, ya que estamos devolviendo un vector de longitud 2

unique_letters <- function(name) {
  name <- gsub(" ", "", name)
  letters <- strsplit(name, split = "")[[1]]
  unique(letters)
}

vapply(cities, unique_letters, character(7)) #Error, el restultado es variable
