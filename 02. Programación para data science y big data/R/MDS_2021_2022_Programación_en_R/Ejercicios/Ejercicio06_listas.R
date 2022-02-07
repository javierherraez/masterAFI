###################################
# Listas                          #
###################################

actors <- c("Jack Nicholson", "Shelley Duvall", "Danny Lloyd", "Scatman Crothers", "Barry Nelson")
scores <- c(4.5, 4.0, 5.0)
sources <- c("IMDb1", "IMDb2", "IMDb3")
comments <- c("Best Horror Film I Have Ever Seen", "A truly brilliant and scary film from Stanley Kubrick", "A masterpiece of psychological horror")
reviews <- data.frame(scores, sources, comments)

# Crea una lista que contenga los siguientes componentes:
#   1. moviename: "The Shining"
#   2. actors: el vector de actores
#   3. reviews: el data frame de reviews
shining_list <- list(moviename = "The Shining", actors = actors, reviews = reviews)

# Selecciona el último actor del vector de actores de la lista
last_actor <- shining_list[["actors"]][5]

# Selecciona la segunda de las críticas del data frame de reviews de la lista
second_review <- shining_list[["reviews"]][2, ]

# Añade un nuevo elemento a lista:
#   1. year: 1980
shining_list_full <- c(shining_list, year = 1980)

# Comprueba el contenido de la lista empleando la función str
str(shining_list_full)