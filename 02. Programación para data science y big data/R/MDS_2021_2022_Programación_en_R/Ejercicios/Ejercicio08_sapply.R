###################################
# sapply                          #
###################################

# La siguiente variable contiene muestras de temperatura de cada día de la semana.
temp <- list(monday = c(3, 7, 9, 6, -1),
             tuesday = c(6, 9, 12, 13, 5),
             wednesday = c(4, 8, 3, -1, -3),
             thursday = c(1, 4, 7, 2, -2),
             friday = c(5, 7, 9, 4, 2),
             saturday = c(-3, 5, 8, 9, 4),
             sunday = c(3, 6, 9, 4, 1))

str(temp)

# Utiliza lapply para encontrar la temeperatura mínima de cada día
lapply(temp, min)

# Utiliza sapply para encontrar la temeperatura mínima de cada día
sapply(temp, min)

# Utiliza lapply para encontrar la temeperatura máxima de cada día
lapply(temp, max)

# Utiliza sapply para encontrar la temeperatura máxima de cada día
sapply(temp, max)

# Crea una función extremes_avg que calcula la media entre la mínima del día y la máxima
extremes_avg <- function(x) {
  mean(c(max(x), min(x)))
}

# Aplica la nueva función utilizando sapply
sapply(temp, extremes_avg)

# Aplica la nueva función utilizando lapply
lapply(temp, extremes_avg)

# Crea una función extremes que devuelve un vector con:
#   1. La temperatura mínima
#   2. La temperatura máxima
extremes <- function(x) {
  c(min = min(x), max = max(x))
}

# Aplica la nueva función utilizando sapply
sapply(temp, extremes)

# Aplica la nueva función utilizando lapply
lapply(temp, extremes)

# Crea una función below_zero que devuelve las muestras menores que cero
below_zero <- function(x) {
  x[x < 0]
}

# Aplica la nueva función utilizando sapply y guardarla en freezing_s
freezing_s <- sapply(temp, below_zero)

# Aplica la nueva función utilizando lapply y guardarla en freezing_l
freezing_l <- lapply(temp, below_zero)

# Compara freezing_s y freezing_l con identical
identical(freezing_s, freezing_l)

# Crea una función que devuelva los siguiente: cat("The average temperature is", mean(x), "\n")
print_info <- function(x) {
  cat("The average temperature is", mean(x), "\n")
}

# Aplica la nueva función utilizando lapply
lapply(temp, print_info)

# Aplica la nueva función utilizando sapply
sapply(temp, print_info)

#¿Qué ha pasado? sapply no ha podido simplificar