# Incluimos el paquete
library(ggplot2)


# MULTIGRÁFICOS EN UN EJE
# -----------------------

# Mostremos en un grafico quilates contra precio con identificación de claridad
gg <- ggplot(diamonds, aes(x = carat, y = price, col = clarity)) + 
  geom_point()
gg

# De una forma mucho más clara
gg + 
  facet_wrap(~clarity)

# Veamos un gráfico de distribución de precios por corte
gg <- ggplot(diamonds, aes(x = price, fill = cut))
gg + 
  geom_bar()

# Mejoremos la visualización normalizando
gg +
  geom_bar(position="fill")

# Mejoremos la visualización colocando las métricas de forma adyacente
gg +
  geom_bar(position="dodge")

# Mejoremos la visualización desagregando por corte
gg + 
  geom_bar() + 
  facet_wrap(~cut)

# Mejoremos la visualización ordenando la desagregación
gg + 
  geom_bar() + 
  facet_wrap(~cut, ncol = 1)

# Mejoremos la visualización viendo densidad en lugar de conteo
gg + 
  geom_density(aes(col = cut)) + 
  facet_wrap(~cut, ncol = 1)


# MULTIGRÁFICOS EN DOS EJES
# -------------------------

# Estudiemos la relación entre caballos y consumo en coches de diferente cilindrada y cambio
gg <- ggplot(mtcars, aes(x = mpg, y = hp, col = as.factor(am), shape = as.factor(cyl)))
gg + 
  geom_point()

# Mejoremos la visualización mediante tamaños y transparencias
gg + 
  geom_point(size = 2, alpha = 0.5)

# Mejoremos la visualización para que el eje de consumo sea más fácilmente interpretable
gg <- ggplot(mtcars, aes(x = 1 / mpg, y = hp, col = as.factor(am), shape = as.factor(cyl)))
gg + 
  geom_point(size = 2, alpha = 0.5) + 
  scale_x_continuous(name = "gpm")

# Mejoremos la visualización representando cada variable según cada criterio
gg <- ggplot(mtcars, aes(x = 1 / mpg, y = hp))
gg + 
  geom_point(size = 2, col = "skyblue4") +     
  facet_grid(am ~ cyl) + 
  scale_x_continuous(name = "gpm")

# Mejoremos la visualización indicando el nombre de la variable que desagrega
gg + 
  geom_point(size = 2, col = "skyblue4") +     
  facet_grid(am ~ cyl, labeller = label_both) + 
  scale_x_continuous(name = "gpm")
