# Incluimos los paquetes necesarios
library(ggplot2)

# Recuperamos el gráfico en el que teníamos los geoms y los scales correctos
y.values <- seq(-5, 55, 5)
y.labels <- sapply(y.values, function(x) { paste0(as.character(x), 'º') })
x.values <- seq(15, 365, 30)
x.labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

chart <- ggplot() + 
  geom_linerange(data = past, aes(x = year.day, ymin = min, ymax = max), col = "wheat") +
  geom_linerange(data = past, aes(x = year.day, ymin = mean_inf, ymax = mean_sup), col = "wheat4") +
  geom_line(data = present, aes(x = year.day, y = temp), size = 0.3) +
  geom_point(data = present.highs, aes(x = year.day, y = temp), col = "firebrick3", size = 1) +
  geom_point(data = present.lows, aes(x = year.day, y = temp), col = "blue3", size=1) + 
  scale_y_continuous(breaks = y.values, labels = y.labels, name = "", expand = c(0, 0)) + 
  scale_x_continuous(breaks = x.values, labels = x.labels, name = "", expand = c(0, 0))

# Ajustamos el estilo del panel del gráfico:
# - Fondo blanco / Sin fondo
chart <- chart + theme(panel.background = element_blank())

# Añade una línea vertical (geom_vline) con:
# - Valor (xintercept) en 0
# - Color: wheat4
# - Tamaño: 1
chart <- chart + geom_vline(xintercept = 0, col = "wheat4", size = 1)

# Ajustamos el estilo de los ejes:
# - Sin ticks
# - Tamaño de fuente: 8
# - Color de fuente: #bbbbbb
chart <- chart + theme(axis.ticks = element_blank(), axis.text = element_text(colour = "#bbbbbb", size=8))

# Ajustamos el estilo del grid:
# - Eje X: línea de puntos, tamaño 0.2, color wheat4 sólo en cortes menores
chart <- chart + theme(panel.grid.minor.x = element_line(linetype = 3, size = .2, colour = "wheat4"))

# Establecemos el título del gráfico: Datos de Madrid 2014
chart <- chart + ggtitle(label = "Datos de Madrid 2014")

# Ajustamos el estilo del título:
# - Negrita
# - Tamaño: 20
# - Color: #3c3c3c
chart <- chart + theme(plot.title = element_text(size = 20, face = "bold", colour = "#3c3c3c"))

# Reajustemos los ejes para que el eje y esté mucho más holgado
chart <- chart + coord_cartesian(ylim = c(-5, 40))

# Introduzcamos el subtítulo "Temperatura" como anotación
# - Negrita
# - Tamaño: 4
# - Color: #3c3c3c
# - Ajuste horizontal: 0
# - Ajuste veritcal: 1
chart <- chart + annotate("text", x = 8, y = 40, size = 4, colour = "#3c3c3c", label = "Temperatura", hjust = 0, vjust = 1, fontface = "bold")

# Introduzcamos el texto de explicación como anotación
# - Tamaño: 4
# - Color: #bbbbbb
texto <- "Los datos representan la temperatura media diaria."
chart <- chart + annotate("text", x = 8, y = 38, size = 4, colour = "#bbbbbb", label = texto, hjust = 0, vjust = 1)

# Introduzcamos los textos y segmentos de explicación de un punto de mínimo histórico
chart <- chart + annotate("segment", x = present.lows$year.day[1] + 4, 
                          y = present.lows$temp[1] - 5, 
                          xend = present.lows$year.day[1] + 0.5, 
                          yend = present.lows$temp[1] - 0.5, 
                          colour = "blue3", size = 0.5, 
                          arrow = arrow(length = unit(0.2, "cm")))
chart <- chart + annotate("text", x = present.lows$year.day[1] + 4, y = present.lows$temp[1] - 5.5, 
                 label = "Hay 2 días para los que\ntenemos mínimo histórico", hjust = 0.2, vjust = 1,
                 size = 2.5, colour = "blue3")
chart




