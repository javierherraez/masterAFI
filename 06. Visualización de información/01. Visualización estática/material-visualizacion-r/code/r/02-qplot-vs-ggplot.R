# Incluimos el paquete
library(ggplot2)

# Analizamos el contenido del set de datos
str(iris)
head(iris)

#############
## qplot vs. ggplot
#############

# Parámetros de qplot
?qplot
?ggplot

# Plot básico (+ aesthetics básicas)
qplot(Sepal.Width, Sepal.Length, data=iris)

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length)) # MAL
ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length)) + 
  geom_point()


# Plot + aesthetics
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, alpha=I(0.6))

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) + 
  geom_point(alpha=0.6)


# Plot + aesthetics + geometries
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, alpha=I(0.6), geom="jitter")

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) + 
  geom_jitter(alpha=0.6)

qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, alpha=I(0.6), geom=c("jitter", "smooth"))

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) + 
  geom_jitter(alpha=0.6) + 
  geom_smooth(alpha=0.6)


# Plot + aesthetics + geometries + facets
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, geom=c("jitter"), facets=~Species)

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) + 
  geom_jitter() + 
  facet_wrap(~Species)


# Plot + aesthetics + geometries + statistics + facets + coordinates
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, geom=c("jitter"), facets=~Species, xlim=c(0, 5), ylim=c(0,8))

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) + 
  geom_jitter() + 
  facet_wrap(~Species) + 
  coord_cartesian(xlim=c(0,5), ylim=c(0,8))


# Plot + aesthetics + geometries + statistics + facets + coordinates + themes
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, geom=c("jitter"), facets=~Species, xlim=c(0, 5), ylim=c(0,8), main="Sepal", xlab="Width", ylab="Length")

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) + 
  geom_jitter() + 
  facet_wrap(~Species) + 
  coord_cartesian(xlim=c(0,5), ylim=c(0,8)) + 
  labs(x="Width", y="Length", title="Sepal")



# Ventaja 1: Control especifico de capas y herencia
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, alpha=I(0.6), geom=c("jitter", "smooth"))

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) + 
  geom_jitter(alpha=0.2) + 
  geom_smooth(alpha=1.0)


# Ventaja 2: Almacenamiento, reutilización y creación iterativa
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, alpha=I(0.6), geom="jitter")
qplot(Sepal.Width, Sepal.Length, data=iris, col=Species, alpha=I(0.6), geom=c("jitter", "smooth"))

iris.plot <- ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species))
iris.plot + geom_jitter(alpha=0.6)
iris.plot
iris.plot + geom_smooth(method="lm", se=F, size=1.5)
