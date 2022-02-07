# Incluimos el paquete
library(ggplot2)

# Analizamos el set de datos
str(mtcars)
head(mtcars)

# POSICIONAMIENTO DE GRÁFICOS DE BARRAS
# -------------------------------------

# Generamos un gráfico de barras 
gg <- ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(am)))
gg + 
  geom_bar()

# Posicionamiento "uno al lado del otro"
gg + 
  geom_bar(position = "dodge")

# Modificamos el posicionamiento en X para que haya solapamiento
gg + 
  geom_bar(size = 1, position = position_dodge(width = 0.5))

# Posicionamiento "uno encima del otro"
gg + 
  geom_bar(position = "stack")

# Posicionamiento "uno encima del otro normalizado"
gg + 
  geom_bar(position = "fill")



# OVERPLOTTING EN SCATTER PLOTS
# -----------------------------

# Un ejemplo de overplotting
gg <- ggplot(mtcars, aes(x = as.factor(cyl), y = as.factor(am), col = as.factor(gear)))
gg + 
  geom_point(size = 3)

# Con geom_jitter lo solucionamos
gg +
  geom_jitter(size = 3)

# geom_jitter es un alias para
gg + 
  geom_point(size = 3, position = "jitter")

# Perdemos "la pista" de de dónde vienen los puntos. Sería necesario tener menos ruido

# Establecemos los parámetros de la función de posición
gg + 
  geom_point(size = 2, position = position_jitter(width = 0.2, height = 0.2))

