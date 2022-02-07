################################################## 
# Intervalos 
##################################################
norm.interval = function(datos, varianza = var(datos),
      nivel.conf = 0.95)
{
z = qnorm((1 - nivel.conf)/2, lower.tail = FALSE)
m = mean(datos)
dt = sqrt(varianza/length(datos))
c(m - z * dt, m + z * dt)
}

#Resultado 
X = rnorm(50,0,1)
norm.interval(X)
  
norm.interval(X,1)


#Simulacion intervalos
nMC=100;n=30
mu=0;sigma=1
muestras=matrix(rnorm(nMC*n,mu,sigma),n)
int.conf = apply(muestras,2,norm.interval)
sum(int.conf[1,]<=mu&int.conf[2,]>=mu)

plot(range(int.conf), c(0, 1+nMC), type = "n", 
xlab = "IC", ylab = "numero de muestra")
for (i in 1:nMC) {
lines(int.conf[, i], rep(i,2),
lwd=2)
}
abline(v=0,lwd=2,lty=2)
#______________________________________

var.interval = function(datos, nivel.conf = 0.95) {
gl = length(datos) - 1
chiinf = qchisq((1 - nivel.conf)/2, gl)
chisup = qchisq((1 - nivel.conf)/2, gl, lower.tail=FALSE)
v = var(datos)
c(gl * v/chisup, gl * v/chiinf)
}


##################################################
# Algunos contrastes
##################################################


#__________________________________________
binom.test(c(300,300),p=0.5)
binom.test(c(30,100),p=0.5)
##__________________________________________
caras = rbinom(1, size=100, pr = .5)
caras
prop.test(caras,100)
##__________________________________________
cartera = c(100,200,50,500)
names(cartera) = c("A","B","C","D")
cartera
cartera.default = c(60,120,10,200)
prop.test(cartera.default,cartera)
##__________________________________________
#Tabla de contingencia
#From Agresti (2009), p. 39
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
barplot(M)
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
##__________________________________________
x = seq(-5,5)
y = x^2
plot(x,y)
z = x^3
plot(x,z)
cor.test(x,y)
wilcox.test(x,y)
cor.test(x,z)
##__________________________________________
x = rnorm(50)
y = runif(100)
ks.test(x, y)
shapiro.test(x)
shapiro.test(y)
##__________________________________________
# https://rpubs.com/osoramirez/316691

library(corrplot)
data(mtcars)
head(mtcars)
M <- cor(mtcars)  #permite ejecutar una matriz de correlación
corrplot(M, method = "ellipse")
# Visualice otros metodos gráficos como, 'circle', 'square', 'ellipse',
# 'number', 'shade', 'color', 'pie'
##__________________________________________
#Algodon-Resistencia a la tracción de la tela
#Porcentaje de algodon que contiene

algodon<-c(7,7,15,11,9,12,17,12,18,18,14,18,18,19,19,19,25,22,19,23,7,10,11,15,11)
porcentaje.algodon = c(15,15,15,15,15,20,20,20,20,20,25,25,25,25,25,30,30,
                       30,30,30,35,35,35,35,35)
datos.algodon = cbind(algodon,porcentaje.algodon)
datos.algodon
oneway.test(algodon~porcentaje.algodon,data=datos.algodon)
boxplot(algodon~porcentaje.algodon)
##__________________________________________
x = rnorm(50, mean = 0, sd = 2)
y = rnorm(30, mean = 1, sd = 1)
var.test(x,y)

