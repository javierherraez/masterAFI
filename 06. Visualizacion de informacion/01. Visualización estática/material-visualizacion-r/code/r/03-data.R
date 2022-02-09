# Estudiemos el set de datos
str(iris)
head(iris)


###############
## dplyr
###############

# Incluimos el paquete
library(dplyr)

# Filtrado
iris %>% filter(Species == "setosa")

# Indexación por columnas
iris %>% select(Species)
iris %>% select(one_of(c("Species", "Petal.Length")))
iris %>% select(Species, Petal.Length)

# Agregación por columnas
iris %>% summarise(Sepal.Length.Mean = mean(Sepal.Length))
iris %>% summarise(Petal.Width.IQR = IQR(Petal.Width))

# Adición / modificación de columnas
iris %>% mutate(Sepal.Area = Sepal.Length * Sepal.Width, Petal.Area = Petal.Length * Petal.Width)
iris %>% transmute(Sepal.Area = Sepal.Length * Sepal.Width, Petal.Area = Petal.Length * Petal.Width)

# Agrupación de elementos por clave
iris %>% group_by(Species)

# Agregación por columnas en base a grupos
iris %>% 
  group_by(Species) %>% 
  summarise(Petal.Length.Mean = mean(Petal.Length))

# Adición / modificación de columnas en base a grupos
iris %>% 
  group_by(Species) %>% 
  mutate(Petal.Length.Mean = mean(Petal.Length)) %>% 
  ungroup()


###############
## tidyr
###############

# Incluimos el paquete
library(tidyr)

# Conversión de datos wide a datos long
colnames(iris)
iris.long <- iris %>% gather(Part.Measure, Value, 1:4)
str(iris.long)
head(iris.long)

# Conversión de datos long a datos wide
iris.wide <- iris.long %>% spread(Part.Measure, Value) # ERROR, NO HAY IDENTIFICACIÓN DE OBSERVACIÓN

# Split de columnas
head(iris.long)
iris.long <- iris.long %>% separate(Part.Measure, c("Part", "Measure"), sep="\\.")
str(iris.long)
head(iris.long)

# Unión de columnas
head(iris.long)
iris.long <- iris.long %>% unite(Part.Measure, 2:3, sep=".")
str(iris.long)
head(iris.long)


