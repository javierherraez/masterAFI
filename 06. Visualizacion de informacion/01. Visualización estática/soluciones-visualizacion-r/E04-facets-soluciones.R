# Incluimos los paquetes necesarios
library(ggplot2)

# Utilizando el set de datos orignal muestra un gráfico que contenga:
# - La regresión lineal de las temperaturas diarias por mes (un color cada mes)
# - Sin intervalo de confianza
ggplot(weather, aes(x = day, y = temp, col = as.factor(month))) + 
  stat_smooth(method = "lm", se = F)

# Modifica el gráfico anterior para que tengamos cada regresión en un gráfico separado
ggplot(weather, aes(x = day, y = temp)) + 
  stat_smooth(method = "lm", se = F) + 
  facet_wrap(~month)

# Modifica el gráfico anterior para que tengamos la regresión de 2014 (continua) comparada con el agregado del resto de 
# años (discontinua)
ggplot(weather, aes(x = day, y = temp, linetype = as.factor(ifelse(year == 2014, 0, 1)))) + 
  stat_smooth(method = "lm", se = F) + 
  facet_wrap(~month) + 
  scale_linetype_discrete(name = "Present / Past")

# Modifica el gráfico anterior para que cada serie quede en un gráfico separado (filas = presente/pasado, 
# columnas = meses)
ggplot(weather, aes(x = day, y = temp)) + 
  stat_smooth(method = "lm", se = F) + 
  facet_grid(as.factor(ifelse(year == 2014, 0, 1))~month) + 
  scale_linetype_discrete(name = "Present / Past")

