# Incluimos el paquete
library(ggplot2)

# Estudiemos el set de datos
str(diamonds)
head(diamonds)


# ESCALAS DE PROPÓSITO GENERAL
# ----------------------------

# Modificación de escala sobre variable continua
gg.cont <- ggplot(diamonds, aes(x = x * y * z, y = price)) + 
  geom_point()
gg.cont

gg.cont + 
  scale_x_continuous(name = "size",
                     limits = c(0, 800),
                     breaks = seq(0, 800, 200),
                     minor_breaks = seq(0, 800, 100))


# Modificación de escala sobre variable discreta
gg.disc <- ggplot(diamonds, aes(x = clarity, y = price)) + 
  geom_jitter()
gg.disc

gg.disc + 
  scale_x_discrete(name = "clarity",
                  limits = c("I1", "SI2", "VVS2", "IF"),
                  label = c("Bad", "Normal", "Good", "Ideal")) + 
  scale_y_continuous(limits = c(1000, 2000))


# ESCALAS DE POSICIÓN
# ----------------------------

# Modificación de escala sobre variable continua
gg.cont <- ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point()
gg.cont

# Escala logarítmica
gg.cont + 
  scale_y_log10(name = "log(price)",
                breaks = c(500, 1000, 5000, 10000, 15000))

# Escala invertida
gg.cont + 
  scale_y_reverse(name = "price (descendant)")


# Modificación de escala sobre variable discreta
gg.disc <- ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_jitter()
gg.disc

# Escala invertida (INCORRECTO)
gg.disc + 
  scale_x_reverse(name = "cut (descendant)")

# Escala invertida
gg.disc + 
  scale_x_discrete(name = "cut (descendant)",
                   limits = rev(levels(diamonds$cut)))


# ESCALAS DE COLOR Y RELLENO
# --------------------------

# Modificación de escala de color sobre variable continua
gg.cont <- ggplot(diamonds, aes(x = carat, y = price, col = carat)) + 
  geom_point()
gg.cont

# Escala de color degradado de dos colores
gg.cont + 
  scale_color_gradient(low = "green4", high = "red1")

# Escala de color degradado de tres colores con punto medio
gg.cont + 
  scale_color_gradient2(low = "green4", high = "red1", mid = "yellow", midpoint = mean(diamonds$carat))

# Escala de color degradado de colores fijos
gg.cont + 
  scale_color_gradientn(colours = c("green4", "yellow", "red1", "blue4"))

# Escala de color degradado de colores fijos (desde funciónes de paleta)
gg.cont + 
  scale_color_gradientn(colours = rainbow(5))

# Funciones de paleta: rainbow, heat.colors, topo.colors, cm.colors

# Modificación de escala de color sobre variable discreta
gg.disc <- ggplot(diamonds, aes(x = clarity, fill=cut)) + 
  geom_bar()
gg.disc

# Escala de color en blanco y negro
gg.disc + 
  scale_fill_grey()

# Escala de color basada en paleta de colores
gg.disc + 
  scale_fill_brewer(palette = "Blues")

# Listado de paletas disponibles en ggplot2: http://colorbrewer2.org/
library(RColorBrewer)
display.brewer.all()


# ESCALAS DE TAMAÑO Y FORMA
# -------------------------

# Modificación de escala de tamaño sobre variable continua
gg.cont <- ggplot(diamonds, aes(x = carat, y = price, size = price)) + 
  geom_point()
gg.cont

# Escala de tamaño continua sobre el radio
gg.cont + 
  scale_size_continuous(range = c(0.05, 5))

# Escala de tamaño continua sobre el área
gg.cont + 
  scale_size_area(max_size = 10)


# Modificación de escala de forma sobre variable continua
gg.cont <- ggplot(diamonds, aes(x = carat, y = price, shape = price)) + 
  geom_point()
gg.cont

# Modificación de escala de forma sobre variable discreta
gg.disc <- ggplot(diamonds, aes(x = carat, y = price, shape = cut)) + 
  geom_point()
gg.disc

# Escala de forma manual
gg.disc + 
  scale_shape_manual(values = c(3:7))
