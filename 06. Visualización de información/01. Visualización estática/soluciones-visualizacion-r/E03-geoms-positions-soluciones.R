# Incluimos los paquetes necesarios
library(ggplot2)

# Pintamos la serie de las temperatura máximas y mínimas históricas
# como un gráfico de rango por línea con color "wheat"
ggplot(past, aes(x = year.day, ymin = min, ymax = max)) + 
  geom_linerange(col = "wheat")

# Pintamos la serie de las temperaturas históricas al 95% como un gráfico
# de rango por línea con color "wheat4"
ggplot(past, aes(x = year.day, ymin = mean_inf, ymax = mean_sup)) + 
  geom_linerange(col = "wheat4")

# Pintamos la serie de las temperaturas del presente año como un gráfico de línea
# de grosor 0.3
ggplot(present, aes(x = year.day, y = temp)) + 
  geom_line(size = 0.3)

# Pintamos la serie de las temperaturas superiores a la histórica como un gráfico de puntos
# de tamaño 1 y color firebrick3
ggplot(present.highs, aes(x = year.day, y = temp)) + 
  geom_point(col = "firebrick3", size = 1)

# Pintamos la serie de las temperaturas inferiores a la histórica como un gráfico de puntos
# de tamaño 1 y color blue3
ggplot(present.lows, aes(x = year.day, y = temp)) + 
  geom_point(col = "blue3", size=1)

# Unimos todas las series en un único gráfico
ggplot() + 
  geom_linerange(data = past, aes(x = year.day, ymin = min, ymax = max), col = "wheat") +
  geom_linerange(data = past, aes(x = year.day, ymin = mean_inf, ymax = mean_sup), col = "wheat4") +
  geom_line(data = present, aes(x = year.day, y = temp), size = 0.3) +
  geom_point(data = present.highs, aes(x = year.day, y = temp), col = "firebrick3", size = 1) +
  geom_point(data = present.lows, aes(x = year.day, y = temp), col = "blue3", size=1)

# Modifica la escala de los ejes X e Y para que:
# Eje X: contenga los nombres de los meses en castellano (en el punto medio de cada mes)
# EJE Y: contenga las temperaturas en formato XXº desde -5 hasta 55 cada 5 grados
# Elimina también los nombres de los ejes (o déjalos en blanco)
# Elimina el margen del gráfico
y.values <- seq(-5, 55, 5)
y.labels <- sapply(y.values, function(x) { paste0(as.character(x), 'º') })
x.values <- seq(15, 365, 30)
x.labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

ggplot() + 
  geom_linerange(data = past, aes(x = year.day, ymin = min, ymax = max), col = "wheat") +
  geom_linerange(data = past, aes(x = year.day, ymin = mean_inf, ymax = mean_sup), col = "wheat4") +
  geom_line(data = present, aes(x = year.day, y = temp), size = 0.3) +
  geom_point(data = present.highs, aes(x = year.day, y = temp), col = "firebrick3", size = 1) +
  geom_point(data = present.lows, aes(x = year.day, y = temp), col = "blue3", size=1) + 
  scale_y_continuous(breaks = y.values, labels = y.labels, name = "", expand = c(0, 0)) + 
  scale_x_continuous(breaks = x.values, labels = x.labels, name = "", expand = c(0, 0))



