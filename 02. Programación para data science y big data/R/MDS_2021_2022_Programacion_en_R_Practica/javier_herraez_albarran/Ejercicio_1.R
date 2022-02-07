library(dplyr)
library(stringr)
library(purrr)

# 1. Lee el fichero y asígnalo a una variable.
f1_results <- read.csv("F1_results.csv",
                       sep = ";",
                       header=T,
                       stringsAsFactors = FALSE,
                       dec = ",")
# 2. ¿De qué clase es el objeto?
class(f1_results)

# 3. ¿Cómo se pueden ver el tipo de cada columna y una muestra de ejemplos?
str(f1_results)

# 4. Muestra los primeros 23 registros del dataset.
head(f1_results, 23)

# 5. Muestra los últimos 19 registros del dataset.
tail(f1_results, 19)

# 6. ¿Cuáles son las dimensiones del dataset?
dim(f1_results)

# 7. ¿Cuáles son los nombres de las variables del dataset?
names(f1_results)

# 8. Comprueba si alguna de las variables contiene Nas. Pista: ¿R siempre los detecta?
sum(is.na(f1_results))  # podemos observar el número total de Nas
summary(f1_results)     # summary nos indica el número de Nas de cada columna
lapply(lapply(f1_results, is.na), sum) # si queremos ver el nº de Nas por columna de forma más clara

# 9. Las variables positionText, status, circuit_location, circuit_country, driver_nationality,
# constructor_nationality y driver_number deberían ser categóricas. Crea un factor con
# etiquetas para dichas columnas y asígnalo a la columna correspondiente de nuevo.
# Es decir, el factor deberá tener tantas categorías como valores tiene la variable y
# cada una de esas categorías debe tener una etiqueta. Recuerda pasar los
# parámetros correctamente. Pista: cuidado con los Nas.

f1_results$positionText <- factor(f1_results$positionText, 
                                  levels = sort(unique(f1_results$positionText)), 
                                  labels = sort(unique(f1_results$positionText)))

f1_results$status <- factor(f1_results$status, 
                            levels = sort(unique(f1_results$status)), 
                            labels = sort(unique(f1_results$status)))

f1_results$circuit_location <- factor(f1_results$circuit_location, 
                                      levels = sort(unique(f1_results$circuit_location)), 
                                      labels = sort(unique(f1_results$circuit_location)))

f1_results$circuit_country <- factor(f1_results$circuit_country, 
                                     levels = sort(unique(f1_results$circuit_country)), 
                                     labels = sort(unique(f1_results$circuit_country)))

f1_results$driver_nationality <- factor(f1_results$driver_nationality, 
                                        levels = sort(unique(f1_results$driver_nationality)), 
                                        labels = sort(unique(f1_results$driver_nationality)))

f1_results$constructor_nationality <- factor(f1_results$constructor_nationality, 
                                             levels = sort(unique(f1_results$constructor_nationality)), 
                                             labels = sort(unique(f1_results$constructor_nationality)))

f1_results$driver_number <- factor(f1_results$driver_number, 
                                   levels = sort(unique(f1_results$driver_number), na.last = T), 
                                   labels = sort(unique(f1_results$driver_number)))

# 10. Crea una variable booleana, end_race, que indique si el piloto finalizó la carrera
# con la variable status como “Finished”
f1_results$end_race <- f1_results$status == 'Finished'

# 11. Calcula la media de la columna laps.
mean(f1_results$laps)

# 12. Guarda en un vector (media) la media de las columnas laps, fastestLapSpeed y
# points. Es decir, el vector “media” deberá tener 3 elementos: el primero conteniendo
# la media de la columna laps, el segundo con la media de la columna
# fastestLapSpeed y el tercero con la media de la columna points.
media <- c(mean(f1_results$laps, na.rm = T), 
           mean(f1_results$fastestLapSpeed, na.rm = T), 
           mean(f1_results$points, na.rm = T))
media

# 13. ¿Qué variables son numéricas? PISTA: utiliza sapply junto con la función is.numeric.
# Guardarlo en una variable. Cuidado con los ids…
numeric_cols <- sapply(f1_results, is.numeric)
numeric_cols
# id_columns <- str_detect(colnames(f1_results), "Id$") para detectar columnas con Ids

# 14. Utilizando la variable con el resultado anterior, selecciona aquellas columnas
# numéricas y calcula la media de aquellas en las que tenga sentido.

columns_sense <- c("grid_quali", "positionOrder", "points", "laps", "milliseconds", "fastestLap", "fastestLapSpeed")
sapply(f1_results[columns_sense], mean, na.rm = T)

# 15. Selecciona las 31 primeras filas y todas las columnas menos las tres últimas
# (emplea índices positivos tanto en filas como en columnas).
f1_results[1:31, 1:(ncol(f1_results) - 3)]

# 16. Selecciona las 31 primeras filas y todas las columnas menos las tres últimas
# (emplea índices negativos tanto en filas como en columnas).
f1_results[-(32:nrow(f1_results)), -((ncol(f1_results) - 2):ncol(f1_results))]

# 17. Obtén los cuartiles de la variable laps.
quantile(f1_results$laps)

# 18. Obtén los deciles de la variable laps.
quantile(f1_results$laps, probs = seq(0, 1, 0.1))

# 19. Obtén los estadísticos básicos de todas las variables en un solo comando.
summary(f1_results)

# 20. ¿En cuántos circuitos se han disputado carreras de Fórmula 1?
length(unique(f1_results$circuitId))

# 21. ¿Cuántas veces un piloto ha conseguido más de 6 puntos?
sum(f1_results$points > 6) #podemos usar la función sum ya que el valor True se corresponde con 1

# 22. Ordena de mayor a menor los 100 primeros elementos de la variable
# fastestLapSpeed.
sort(head(f1_results$fastestLapSpeed, 100), decreasing = T)

# 23. Ordena el dataset por la variable points de manera ascendente. Inspecciona los
# primeros resultados para comprobar que se ha ordenado como se pide.
f1_results <- f1_results[order(f1_results$points), ]
head(f1_results, 10)

# 24. Obtén los índices de los registros para los que el valor de la variable laps es
# superior a la mediana.
which(f1_results$laps > median(f1_results$laps))

# 25. ¿Cuántas veces ha disputado un piloto español una carrera?
sum(f1_results$driver_nationality == "Spanish")
length(which(f1_results$driver_nationality == "Spanish"))

# 26. ¿Qué nacionalidad es la que más participaciones tiene en una carrera de Fórmula
f1_results %>%
  group_by(driver_nationality) %>%
  summarise(n_nationality = n()) %>%
  arrange(desc(n_nationality))

#podríamos usar la función table
#table(f1_results$driver_nationality)

# 1? ¿Y las que menos?
f1_results %>%
  group_by(driver_nationality) %>%
  summarise(n_nationality = n()) %>%
  arrange(n_nationality)

# 27. ¿Qué pilotos no han finalizado una carrera?
#2 formas

f1_results %>%
  group_by(driverRef) %>%
  summarise(n = sum(status == 'Finished')) %>%
  filter(n == 0) %>%
  select(driverRef)

pilotos_con_victorias <- unique(f1_results$driverId[which(f1_results$status == 'Finished')])
unique(f1_results[!f1_results$driverId %in% pilotos_con_victorias, c("driverRef")])

# 28. Comprueba utilizando el boxplot si la variable laps tiene outliers.
boxplot(f1_results$laps, horizontal = T)

# 29. Pinta un histograma de la variable fastestLap
hist(f1_results$fastestLap)

# 30. Crea una función (get_podium) que reciba dos parámetros:
#   a. positionOrder: posición final en la carrera.
#   b. podiumPlaces: posición hasta la que se considera que el piloto ha
#      conseguido un podium. El valor por defecto es 3.
# La función debe devolver “PODIUM” si ha finalizado entre las plazas de podium y
# “NO PODIUM” en caso contrario. Ejemplifica en el código que funciona
# correctamente con varios casos.

get_podium <- function(positionOrder, podiumPlaces = 3){
  if (positionOrder <= podiumPlaces)
    return ("PODIUM")
  return ("NO PODIUM")
}
get_podium(3)
get_podium(4)
get_podium(5, 6)
get_podium(7, 6)

# 31. Mediante una llamada a una de las funciones apply aplica la función anterior a toda
# la columna postionOrder. El resultado obtenido debe ser almacenado en una nueva
# variable del data frame llamada podium.
f1_results$podium <- lapply(f1_results$positionOrder, get_podium)

# 32. Repite el ejercicio anterior usando el paquete PURRR.
f1_results$podium <- unlist(map(f1_results$positionOrder, get_podium))
                         