###################################
# apply                           #
###################################

m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)

apply(m, 1, mean) # Por filas

apply(m, 2, mean) # Por columnas

apply(m, 1:2, function(x) x/2) # Por filas y por columnas m / 2
