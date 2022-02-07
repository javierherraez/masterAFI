##################################
# data.table                     #
##################################
library(data.table)

# Convierte iris en un data.table
iris.dt <- as.data.table(iris)

# Revisa su contenido
str(iris.dt)

# Para cada especie calcula la longitud media del sépalo
iris.dt[, mean(Sepal.Length), by = Species]

# Agrupando por la primera letra del nombre de cada especie calcula la longitud media del sépalo
iris.dt[, mean(Sepal.Length), by = substr(Species, 1, 1)]

# Agrupa las especies por el área del sépalo (cada 10 cm2) y cuenta cuantos hay en cada bloque
iris.dt[, .(Count = .N), by = .(Area = 10 * round(Sepal.Length * Sepal.Width / 10))]  

# Para cada especie calcula la mediana de todas las columnas. Ordena el resultado por el nombre de la especie descendentemente.
iris.dt[, .(Sepal.Length = median(Sepal.Length), 
       Sepal.Width = median(Sepal.Width), 
       Petal.Length = median(Petal.Length),
       Petal.Width = median(Petal.Width)), by = Species][order(-Species)]

# Repite el ejercicio anterior usando .SD
iris.dt[, lapply(.SD, median), by = Species][order(-Species)]

# Filtra las filas de la especie virginica
iris.dt[Species == "virginica"]

# Filtra las filas de la especie virginica y versicolor
iris.dt[Species %in% c("virginica", "versicolor")]

# Elimina el prefijo sepal del nombre de las columnas
setnames(iris.dt, gsub("^Sepal\\.", "", names(iris)))

# Elimina las dos columnas que comienzan por "Petal"
iris.dt[, grep("^Petal", names(iris)) := NULL]

# Filtra las filas para las que el área es mayor que 20
iris.dt[Width * Length > 20]

# Añade una nueva columna que indique si la fila tienen una área mayor de 25
iris.dt[, is_large := Width * Length > 25]

# Filtra aquellas filas que cumplen la condición anterior
iris[is_large == TRUE]
iris[(is_large)]