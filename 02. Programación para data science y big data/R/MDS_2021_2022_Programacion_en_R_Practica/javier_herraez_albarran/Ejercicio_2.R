library(dplyr)
library(stringr)
library(purrr)
library(reshape2)
library(tidyr)
library(data.table)

f1_results <- read.csv("F1_results.csv",
                       sep = ";",
                       header=T,
                       stringsAsFactors = FALSE,
                       dec = ",")
# 1. A partir de las variables driver_id y constructor_id, crea una nueva carId que sea la
# concatenación de ambas mediante un guion bajo. No borres las columnas id y title
# en el proceso.

f1_results$carId <- paste(f1_results$driverId, "_", f1_results$constructorId)

# 2. Sobreescribe el valor de las columnas race_Date por una columna de tipo fecha.
f1_results$race_Date <- as.Date(f1_results$race_Date)

#3. Crea un long dataset que contenga las siguientes variables:
# resultId: identificador único de cada carrera.
# VARIABLE: que contiene la lista de variables: grid_quali, positionOrder y points.
# VALUE: los diferentes valores.

df_long <- f1_results[c("resultId", "grid_quali", "positionOrder", "points")]
df_long_tidyr <- pivot_longer(df_long, cols = c("grid_quali", "positionOrder", "points"), names_to = "VARIABLE", values_to = "VALUE")
df_long_reshape2 <- reshape2::melt(df_long, id.vars = c("resultId"), variable.name = "VARIABLE", value.name = "VALUE" )
# 4. Realiza el proceso inverso desde el long dataset anterior para obtener el dataset
# original con las siguientes columnas:
# resultId
# result_name
# grid_quali
# points
# positionOrder
df_wide_tidyr <- pivot_wider(df_long_tidyr, names_from = VARIABLE, values_from = VALUE)
df_wide_reshape2 <- reshape2::dcast(df_long_reshape2, resultId ~ VARIABLE, value.var = "VALUE")

# 1. Calcula el número de victorias totales para cada piloto y ordena el resultado según
# esta nueva variable de forma descendente. Utiliza driverRef.
f1_results %>%
  filter(positionOrder == 1) %>% 
  group_by(driverRef) %>% 
  summarise(n_victorias = n()) %>% 
  arrange(desc(n_victorias))

f1_results.dt <- as.data.table(f1_results)

f1_results.dt[positionOrder == 1, .(Count = .N), by = driverRef][order(-Count)]
 
# 2. Calcula el número de carreras disputadas y los puntos medios obtenidos por cada
# piloto en cada circuito y ordena el resultado según esta nueva variable de forma
# descendente. Utiliza driverRef y circuitRef.
f1_results %>% 
  group_by(driverRef, circuitRef) %>% 
  summarise(n_carreras = n(),
            pts_medios = mean(points)) %>% 
  arrange(desc(pts_medios))

f1_results.dt[, 
              .(n_carreras = .N, pts_medios = mean(points)), 
              by = .(driverRef, circuitRef)][order(-pts_medios)]

# 3. Calcula el número de vueltas totales completadas en cada mes.
f1_results %>% 
  group_by(strftime(race_Date, '%m')) %>% 
  summarise(n_vueltas = sum(laps))

f1_results.dt[, 
              .(n_vueltas = sum(laps)), 
              by = .(strftime(race_Date, '%m'))]

# 4. ¿Cuál es el constructor con mayor número de puntos medios?
f1_results %>% 
  group_by(constructor_name) %>% 
  summarise(pts_medios = mean(points)) %>% 
  arrange(desc(pts_medios))

f1_results.dt[, 
              .(pts_medios = mean(points)), 
              by = .(constructor_name)][order(-pts_medios)]

# 5. Restringiéndonos al siglo XXI, ¿cuál es la nacionalidad (de pilotos) que más veces
# ha logrado finalizar entre los 5 primeros?
f1_results %>% 
  filter(as.integer(strftime(race_Date, '%Y')) > 2000, positionOrder <= 6) %>% 
  group_by(driver_nationality) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

f1_results.dt[as.integer(strftime(race_Date, '%Y')) > 2000 & positionOrder <= 6, 
              .(n = .N), 
              by = .(driver_nationality)][order(-n)]

# 6. Calcula las vueltas de la carrera más larga, más corta y la vuelta media en la que se
# obtiene la vuelta rápida de las carreras posteriores a 2014 y sólo si el piloto ha
# finalizado la carrera (que status sea Finished).

f1_results %>% 
  filter(as.integer(strftime(race_Date, '%Y')) > 2014, status == 'Finished') %>% 
  summarise(max_laps = max(laps),
            min_laps = min(laps),
            mean_fastest_lap = mean(fastestLap, na.rm = T))

f1_results.dt[as.integer(strftime(race_Date, '%Y')) > 2014 & status == 'Finished', 
              .(max_laps = max(laps),
                min_laps = min(laps),
                mean_fastest_lap = mean(fastestLap, na.rm = T)), 
              ,]

