# Incluimos el paquete
library(ggplot2)

# TIPO DE SISTEMA DE COORDENADAS
# ------------------------------

# Gráfico de barras que muestra la distribución por claridades
ggplot(diamonds, aes(x = clarity, fill = clarity)) + 
  geom_bar()

# Gráfico de tarta que muestra la distribución por claridades
ggplot(diamonds, aes(x = clarity, fill = clarity)) + 
  geom_bar() + 
  coord_polar()

ggplot(diamonds, aes(x = clarity, fill = clarity)) + 
  geom_bar() + 
  coord_polar(theta="y")

ggplot(diamonds, aes(x = 0, fill = clarity)) + 
  geom_bar() +
  coord_polar(theta="y")


# LÍMITES Y ZOOM
# --------------

# Gráfico de quilates contra precio
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point()

# Zoom para observar el corte de precios
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 1.5)) + 
  scale_y_continuous(limits = c(0, 2000))

# Zoom "correcto" para observar el corte de precios
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 2000))


# RELACIÓN DE ASPECTO
# -------------------

# Gráfico de corte contra claridad
ggplot(diamonds, aes(x = cut, y = clarity)) + 
  geom_jitter()

# Mantenemos relación de aspecto para no "penalizar" variables con más categorías
ggplot(diamonds, aes(x = cut, y = clarity)) + 
  geom_jitter() +
  coord_fixed(ratio=1)


# ORIENTACIÓN DE LOS EJES
# -----------------------

# Ejes alineados con aesthetics
ggplot(diamonds, aes(x = cut)) + 
  geom_bar()

# Ejes invertidos
ggplot(diamonds, aes(x = cut)) + 
  geom_bar() + 
  coord_flip()
