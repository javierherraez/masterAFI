# Incluimos la libreria
library(ggplot2)

# Resumen del contenido del set de datos
str(iris)

# Base: Scatter plot de Longitud vs. Anchura en sépalos
plot(iris$Sepal.Length, iris$Sepal.Width)

# ggplot2: Scatter plot de Longitud vs. Anchura en sépalos
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point()

# Base: Scatter plot de Longitud vs. Anchura en sépalos Y pétalos - MAL
plot(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Petal.Length, iris$Petal.Width)

# Base: Añadir Longitud vs. Anchura de pétalos - REGULAR
plot(iris$Sepal.Length, iris$Sepal.Width)
points(iris$Petal.Length, iris$Petal.Width)

# Base: Añadir Longitud vs. Anchura de pétalos - (casi) BIEN
plot(iris$Sepal.Length, iris$Sepal.Width)
points(iris$Petal.Length, iris$Petal.Width, col='red')

# ggplot2: Scatter plot de Longitud vs. Anchura en sépalos Y pétalos - REGULAR
ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width)) + 
  geom_point() + 
  geom_point(aes(x=Petal.Length, y = Petal.Width), col = 'red')

# Base: Con el siguiente dataframe, scatter, barras, linea, area, texto...
df <- data.frame(x = c(3, 1, 5), y = c(2, 4, 6), label=c('a', 'b', 'c'))
plot(df$x, df$y)
barplot(df$x)
plot(df$x, df$y, type="l")
# NO HAY AREA DE FORMA DIRECTA
plot(df$x, df$y, pch = as.character(df$label))

# ggplot2: Con el siguiente dataframe, scatter, barras, linea, area, texto...
chart <- ggplot(df, aes(x=x, y=y, label=label))
chart + geom_point()
chart + geom_bar(stat="identity")
chart + geom_line()
chart + geom_area()
chart + geom_text()
