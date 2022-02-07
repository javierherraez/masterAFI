# Incluimos el paquete
library(ggplot2)

# Gráfico básico
gg <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
  geom_jitter()
gg

# Anotaciones de texto
gg + annotate("text", label = "Anotación de texto", x = 4, y = 5, hjust = 0, face = "bold", family = "Courier")

# Anotaciones de segmento
gg + annotate("segment", x = 4.5, y = 2.5, xend = 6.5, yend = 4, col = "red", size = 2)

# Anotaciones de rectángulo
gg + annotate("rect", xmin = 5.8, xmax = 7, ymin = 2.5, ymax = 3.5, fill = "blue", alpha = 0.1)