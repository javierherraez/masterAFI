###################################
# Matrices                        #
###################################

# Los siguientes vectores contienen la recaudación de las tres primeras películas de Star Wars en US y fuera de US (non-US)
new_hope <- c(461, 314.4)
empire_strikes <- c(290.5, 247.9)
return_jedi <- c(309.3, 165.8)

# Crea una matriz que contenga toda la información (con tres filas)
star_wars_matrix <- matrix(c(new_hope, empire_strikes, return_jedi), byrow=TRUE, nrow=3)

# Ponles nombres a las columnas: "US" y "non-US"
colnames(star_wars_matrix) <- c("US", "non-US")

# Ponles nombres a las filas: "A New Hope", "The Empire Strikes Back" y "Return of the Jedi"
rownames(star_wars_matrix) <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")

# Si el precio de la entrada es de 5$, estima el número de espectadores de cada película
visitors <- star_wars_matrix / 5

# Como el precio de las entradas no es el mismo todos los años, creamos una matriz de precios
ticket_prices_matrix <- matrix(c(5, 5, 6, 6, 7, 7), nrow = 3, byrow = TRUE, dimnames = list(rownames(star_wars_matrix), colnames(star_wars_matrix)))

# Repite el cálculo del número de espectadores con la matriz anterior
visitors <- star_wars_matrix / ticket_prices_matrix

# Calcula el numero de espectadores medio en US
average_us_visitors <- mean(visitors[, 1])

# Calcula el numero de espectadores medio fuera de US
average_non_us_visitors <- mean(visitors[, 2])

# Calcula los totales de recaudación por película
worldwide_vector <- rowSums(star_wars_matrix)

# Añade el vector anterior con los totales por película como una nueva columna de la matriz
all_wars_matrix <- cbind(star_wars_matrix, worldwide_vector)

# Crea una nueva matriz con las recaudaciones de las siguientes tres películas de Star Wars
phantom_menace <- c(474.5, 552.5)
attack_clones <- c(310.7, 338.7)
revenge_sith <- c(380.3, 468.5)
star_wars_matrix2 <- matrix(c(phantom_menace, attack_clones, revenge_sith), byrow=TRUE, nrow=3)

# Ponles nombres a las columnas: "US" y "non-US"
colnames(star_wars_matrix2) <- c("US", "non-US")

# Ponles nombres a las filas: "The Phantom Menace", "Attack of the Clones" y "Revenge of the Sith"
rownames(star_wars_matrix2) <- c("The Phantom Menace", "Attack of the Clones", "Revenge of the Sith")

# Une en una nueva matriz la recaudación de todas las películas, las tres primeras filas corresponderán
# a las tres primeras películas y las tres siguientes a las últimas películas
all_wars_matrix <- rbind(star_wars_matrix, star_wars_matrix2)

# Calcula los totales de recaudación de todas las películas en US y fuera de US
total_revenue_vector <- colSums(all_wars_matrix)

# Calcula la media recaudada de las tres primeras películas fuera de US
non_us_all <- mean(star_wars_matrix[, 2])

# Calcula la media recaudada de las 2 primeras películas fuera de US
non_us_some <- mean(star_wars_matrix[1:2, 2])