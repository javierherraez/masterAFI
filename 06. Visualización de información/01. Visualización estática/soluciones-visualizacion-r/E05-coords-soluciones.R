# Incluimos los paquetes necesarios
library(ggplot2)

# Pintamos un gráfico en el que se muestre:
# - Serie de temperaturas de 2014 como linea
# - Serie de temperaturas históricas como linerange
# - Serie de puntos azules con los días en los que la temperatura de 2014 es inferior a la histórica
ggplot() + 
  geom_linerange(data = past, aes(x = year.day, ymin = min, ymax = max), col = "wheat") +
  geom_line(data = present, aes(x = year.day, y = temp), size = 0.3) +
  geom_point(data = present.lows, aes(x = year.day, y = temp), col = "blue3", size=1)

# Modificando el gráfico anterior, consigue uno que permita analizar el primer día en el que la temperatura actual 
# es menor que la histórica
ggplot() + 
  geom_linerange(data = past, aes(x = year.day, ymin = min, ymax = max), col = "wheat") +
  geom_line(data = present, aes(x = year.day, y = temp), size = 0.3) +
  geom_point(data = present.lows, aes(x = year.day, y = temp), col = "blue3", size=1) + 
  coord_cartesian(xlim = c(present.lows$year.day[1] - 10, present.lows$year.day[1] + 10),
                  ylim = c(present.lows$temp[1] - 5, present.lows$temp[1] + 5))
