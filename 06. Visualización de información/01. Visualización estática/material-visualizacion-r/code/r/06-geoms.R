# Incluimos el paquete
library(ggplot2)

# Estudiemos el set de datos
str(iris)
head(iris)

# Una variable continua
# ---------------------
gg <- ggplot(iris, aes(x = Petal.Length))

# Área
gg + 
  geom_area(stat = "bin", bins = 10)

# Histograma
gg + 
  geom_histogram(binwidth=1)

# Densidad
gg + 
  geom_density()

# Histograma de puntos
gg + 
  geom_dotplot()


# Dos variables continuas
# -----------------------
gg <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length))

# Scatter plot
gg + 
  geom_point()

# Scatter con ruido
gg +   
  geom_jitter()

# Linea
gg + 
  geom_line()

# Distribución de observaciones por eje (rug)
gg + 
  geom_rug()

# Smooth / regresión
gg + 
  geom_smooth(method = "loess")

# Texto
gg + 
  geom_text(aes(label = substr(Species, 1, 2)))

# Densidad / curvas de nivel
gg +   
  geom_density2d()

# Tiles 2D
gg + 
  geom_bin2d()


# Una variable discreta y una continua
# ------------------------------------
gg <- ggplot(iris, aes(x = Species, y = Sepal.Length))

# Barras
gg + 
  geom_bar(stat="identity")

# Scatter
gg + 
  geom_point()

# Scatter con ruido (barras con distribución)
gg + 
  geom_jitter(width=0.5)

# Boxplot
gg + 
  geom_boxplot()

# Violin
gg + 
  geom_violin()


# Una variable discreta y un rango
# --------------------------------
df <- data.frame(type = c("A", "B"), value = 4:5, variation = 1:2)
gg <- ggplot(df, aes(type, value, ymin = value - variation, ymax = value + variation))

# Boxplot sin bigotes
gg + 
  geom_crossbar()

# Barra de "error"
gg + 
  geom_errorbar()

# Rango por linea
gg + 
  geom_linerange()
