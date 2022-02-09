# Incluimos los paquetes necesarios
library(ggplot2)
# Pintamos un gráfico de linea (geom_line) en el que:
# - Información: 2014
# - Eje X: día del año
# - Eje Y: temperatura
ggplot(present, aes(x = year.day, y = temp)) + geom_line()

# Modificamos el gráfico anterior para que:
# - La línea tenga colores que vayan del azul (más frío) al rojo (más caliente) con blanco en su punto medio (15ºC)
ggplot(present, aes(x = year.day, y = temp, col=temp)) + 
  geom_line() + 
  scale_color_gradient2(low = "blue", high = "red", mid="white", midpoint = 15)

# Modificamos el gráfico anterior para que:
# - La línea sea negra semi transparente y de puntos
# - Incluya una nueva capa de puntos de tamaño fijo (1) 
# - Los puntos tengan un border negro semi transparente y un relleno cuyo color vaya 
#   del azul (más frío) al rojo (más caliente)
ggplot(present, aes(x = year.day, y = temp)) + 
  geom_line(alpha = 0.5, linetype = "dotted") +
  geom_point(aes(col = temp), size = 1) +
  scale_color_gradient2(low = "blue", high = "red", mid="white", midpoint = 15) +
  geom_point(col = "black", shape = 1, size = 1, alpha = 0.5)

ggplot(present, aes(x = year.day, y = temp)) + 
  geom_line(alpha = 0.5, linetype = "dotted") +
  geom_point(aes(fill = temp), shape= 21, size = 1, col = "black") + #sería poner en lugar de black color hexadecimal
  scale_fill_gradient2(low = "blue", high = "red", mid="white", midpoint = 15)

# Modicamos el gráfico anterior para que:
# - Sólo muestre los meses fríos (temperatura menor o igual a 15 º C)
# - Pinte el punto más caliente 5 veces más grandes que el más frío

ggplot(present, aes(x = year.day, y = temp)) + 
  geom_line(alpha = 0.5, linetype = "dotted") +
  geom_point(aes(fill = temp, size = temp), shape= 21, col = "black") + 
  scale_fill_gradient2(low = "blue", high = "red", mid="white", midpoint = 15) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_size_continuous(range = c(1, 5))




