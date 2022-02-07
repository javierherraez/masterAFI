# Incluimos el paquete
library(ggplot2)

# SUAVIZADO / REGRESIÓN
# ---------------------

# Volvamos al ejemplo básico
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point()

# Añadimos un suavizado mediante el uso del stat_smooth
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() + 
  stat_smooth()

# Algunos parámetros
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_point() + 
  stat_smooth(method="loess", n = 10, level = 0.99, se = T)

# Pintamos únicamente el stat
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +   
  stat_smooth()

# Modificamos el geom del stat
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +   
  stat_smooth(geom = "line")

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +   
  stat_smooth(geom = "point")

# Modificamos los aesthetics del stat
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +   
  stat_smooth(aes(fill = Species), alpha = 0.2)


# CUANTILES
# ---------

# Volvmeos al gráfico de siempre
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point()

# Añadamos una estaística de cuantiles
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +   
  stat_quantile()

# Algunos parámetros
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +     
  geom_point() + 
  stat_quantile(quantiles = c(0, 0.25, 0.50, 0.75, 1))

# Podemos usar las variables generadas por el stat
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +     
  stat_quantile(quantiles = c(0, 0.25, 0.50, 0.75, 1), aes(col = as.factor(..quantile..)))

  
# DISCRETIZACION
# --------------

# Conteos
ggplot(iris, aes(x = Species)) + 
  stat_count()

# Discretización
ggplot(iris, aes(x = Sepal.Length)) + 
  stat_bin(bins = 30)


# DISTRIBUCIONES
# --------------

# Densidad 1D
ggplot(iris, aes(x = Sepal.Length)) + 
  stat_density()

# Densidad 2D
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  stat_density2d(aes(col = ..level..))

# Boxplot
ggplot(iris, aes(x = Species, y = Sepal.Width)) + 
  stat_boxplot()














