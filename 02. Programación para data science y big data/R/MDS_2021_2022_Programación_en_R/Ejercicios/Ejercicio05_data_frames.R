###################################
# Data frames                     #
###################################

# Crea a partir de los vectores siguientes un data frame
planets <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune");
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883); 
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67);
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE);

planets_df  <- data.frame(planets, type, diameter, rotation, rings)

# Comprueba el contenido del data frame
str(planets_df)

# Selecciona la información de los primeros tres planetas (los más cercanos al sol)
closest_planets_df <- planets_df[1:3, ]

# Selecciona la información de los últimos tres planetas (los más lejanos al sol)
furthest_planets_df <- planets_df[6:8, ]

# Comprueba la selección
closest_planets_df
furthest_planets_df

# Selecciona la columna diameter de los últimos seis planetas (los más lejanos al sol)
furthest_planets_diameter <- planets_df[3:8, "diameter"]

# Selecciona sólo los planetas que tienen anillos
planets_with_rings_df <- planets_df[planets_df$rings, ]

# Selecciona los planetas que tienen un diametro inferior al de la tierra (aquellos que tienen diametro < 1, 
# puesto que la variable es relativa al diametro de la tierra)
small_planets_df  <- subset(planets_df, subset = diameter < 1)

# La función order devuelve las posiciones de un vector ordenado ascendentemente
a <- c(4, 10, 3)
order(a)
a[order(a)]

# Ordena el data frame según el diametro de los planetas ascendentemente
positions <- order(planets_df$diameter, decreasing = TRUE)
largest_first_df <- planets_df[positions, ]
largest_first_df