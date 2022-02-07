# Incluimos el paquete
library(ggplot2)

# Estudiemos el set de datos
str(diamonds)
head(diamonds)

# Reducimos el tamaño del set de datos
diamonds <- diamonds[sample(nrow(diamonds), nrow(diamonds) * 0.1),]

# AESTHETICS DE POSICIÓN
# ----------------------

# Variables continuas
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point()

# Variables continuas vs discretas
ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_point()

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_jitter()

# Variables discretas
ggplot(diamonds, aes(x = cut, y = clarity)) + 
  geom_point()

ggplot(diamonds, aes(x = cut, y = clarity)) + 
  geom_jitter()



# AESTHETICS DE TRANSPARENCIA
# --------------------------

# Transparencia como aesthetic
ggplot(diamonds, aes(x = carat, y = price, alpha=clarity)) +
  geom_point()

# Transparencia como attribute (ASÍ NO)
ggplot(diamonds, aes(x = carat, y = price, alpha=0.4)) +
  geom_point()

# Transparencia como attribute (ASÍ TAMPOCO)
ggplot(diamonds, aes(x = carat, y = price), alpha = 0.4) +
  geom_point()

# Transparencia como attribute (ASÍ SÍ)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.4)



# AESTHETICS DE COLOR
# -------------------

# Color como aesthetic (continuo)
ggplot(diamonds, aes(x = price, y = x * y * z, col = carat)) + 
  geom_point()

# Color como aesthetic (discreto)
ggplot(diamonds, aes(x = carat, y = price, col = clarity)) + 
  geom_point()

# Color como atributo - Nombre
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(col="red1")

# Listado de colores disponibles por nombre en R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Color como atributo - RGB hexadecimal
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(col="#FF0000")

# Color como atributo - RGBA hexadecimal
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(col="#FF0000AA")

# Relleno como aesthetic
ggplot(diamonds, aes(x = clarity, fill=cut)) + 
  geom_bar()

# Relleno como atributte
ggplot(diamonds, aes(x = clarity)) + 
  geom_bar(fill="skyblue4")


# AESTHETICS DE FORMA
# -------------------

# Forma como aesthetic (continua)
ggplot(diamonds, aes(x = carat, y = price, shape=x * y *z)) + 
  geom_point()

# Forma como aesthetic (discreta)
ggplot(diamonds, aes(x = carat, y = price, shape=cut)) + 
  geom_point()

# Forma como attribute (discreta)
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(shape=1)

# Listado de formas disponibles en R: http://www.cookbook-r.com/Graphs/Shapes_and_line_types/

# Tipo de linea como aesthetic (continua)
ggplot(diamonds, aes(x = carat, y = price, linetype=x * y * z)) + 
  geom_smooth(se=F, size=1)

# Tipo de linea como aesthetic (discreta)
ggplot(diamonds, aes(x = carat, y = price, linetype=cut, col=cut)) + 
  geom_smooth(se=F, size=1)

# Tipo de linea como attribute
ggplot(diamonds, aes(x = carat, y = price, col = cut)) + 
  geom_smooth(se=F, size=1, linetype="dotdash")

# Listado de tipos de linea disponibles en R: http://www.cookbook-r.com/Graphs/Shapes_and_line_types/



# AESTHETICS DE TAMAÑO
# --------------------

# Tamaño como aesthetic (continua)
ggplot(diamonds, aes(x = carat, y = price, size = price)) + 
  geom_point()

# Tamaño como aesthetic (discreta)
ggplot(diamonds, aes(x = carat, y = price, size = clarity)) + 
  geom_point()

# Tamaño como attirbute
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(size=0.5)



