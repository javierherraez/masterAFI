
library(ggplot2)
data(diamonds)

summary(diamonds)

# Calcular outliers por la regla 3sigma

mu <- mean(diamonds$carat)
sigma <- sd(diamonds$carat)

sum(diamonds$carat < mu - 3*sigma | diamonds$carat > mu + 3*sigma)

# Calcular outliers por IQR

QI <- quantile(diamonds$carat, 0.25)
QS <- quantile(diamonds$carat, 0.75)
IQR = QS-QI

sum(diamonds$carat < QI - 1.5*IQR | diamonds$carat > QS + 1.5*IQR)


### Diagrama Q-Q

qqnorm(diamonds$carat)
qqline(diamonds$carat)

#####
#Binning

library(binr)

x <- exp(rnorm(200000))
y <- c(10, 21,56,79,114,122,47,44,42,21,20,31,11,4,4)

out1  <- bins(y,3, minpts = 6)
bins.getvals(out1)


library(outliers)

set.seed(1234)
y=rnorm(100)

outlier(y) ##Cuentuentra el valor con la mayor diferencia con respecto a la muestra. Candidato a ser outlier (aunque no tiene por qué serlo)


