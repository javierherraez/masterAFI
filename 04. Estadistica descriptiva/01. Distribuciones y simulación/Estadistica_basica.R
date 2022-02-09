################################################## 
# Distribuciones 
##################################################


library(help = "stats")
library("stats")
library(stats4)
##############################################
p<-c(0.01,0.025,0.05,0.10,0.9,0.95,0.975,0.99)
q<-c(0.01,0.025,0.05,0.10,0.9,0.95,0.975,0.99)
x<-seq(from = 0, to =1, by =0.01)
##############################################
#Uniforme
a<-runif(1000, min = 0, max = 1)
hist(a,20)
boxplot(a)
stem(a)
summary(a)
mean(a);
sd(a);
var(a);
quantile(a, probs = seq(0, 1, 0.25))
quantile(a, probs = c(0.1, 0.5, 1))

punif(0.5, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
punif(0.5, min = 0, max = 2, lower.tail = TRUE, log.p = FALSE)
qunif(0.75, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(0.75, min = 0, max = 2, lower.tail = TRUE, log.p = FALSE)
b<-dunif(x, min = 0, max = 1, log = FALSE)
plot(b)

##############################################
#DADO
dadoBueno=sample(c('1','2','3','4','5','6'), 100, replace = TRUE); 
table(dadoBueno);
dadoBueno
barplot(table(dadoBueno))

dadoTrucoNum=sample(c(1:6), 1000, replace = TRUE,prob = c(2,3,1,9,8,14))
table(dadoTrucoNum);
dadoTrucoNum
barplot(table(dadoTrucoNum))
summary(table(dadoTrucoNum))

##############################################
install.packages("pastecs")
library(pastecs)
stat.desc(dadoTrucoNum)
##############################################

#Variable discreta
estado_informe<-c(1,2,1,2,2,2,3,3,1,4,2,2,2,3,1,4,3,2,1,1,1)
table(estado_informe)
estado_informe<-factor(estado_informe, labels=c("correcto","salvedades1","salvedades2","incorrecto"))
table(estado_informe)
prop.table(table(estado_informe))
round(100*prop.table(table(estado_informe)),1)
barplot(table(estado_informe))
barplot(table(estado_informe),col=c("blue","grey","brown","green"),main="Informe Auditoria")
##############################################

#Dos variables
prestamo <- c(56, 67, 65, 78, 49, 87, 55, 63, 70, 72, 79, 52, 60, 78, 90)
sex <- c(1,1,1,2,1,2,1,1,1,2,1,1,1,2,2)
tapply(prestamo,sex,mean)
tapply(prestamo,sex,summary)
##############################################

#Tabla de contingencia
hipoteca<-c(1,0,1,0,0,0,1,0,0,0,0,0,0,1,1)
table(sex,hipoteca)
prop.table(table(sex,hipoteca),1) # row percentages
prop.table(table(sex,hipoteca),2) # column percentages
##############################################
#ECDF

x <- rnorm(30)
Fn <- ecdf(x)
Fn     # funcion
Fn(x)  # percentiles de x
#tt <- seq(-2, 2, by = 0.1)
#12 * Fn(tt) # funcion simple valores k/12
summary(Fn)
knots(Fn)  # unicos
plot(Fn)
##############################################

#Normal
a<-rnorm(1000, mean = 0, sd = 1)
hist(a)
boxplot(a)
stem(a)
summary(a)
mean(a);
sd(a);
var(a);
quantile(a, probs = seq(0, 1, 0.25))
quantile(a, probs = c(0.1, 0.5, 1))

##############################################
#Varios graficos en una misma ventana

par(mfrow=c(1,1))      # un solo gr?fico por ventana: la opci?n por defecto
par(mfrow=c(2,1))      # Dibuja una matriz de gr?ficos 2x1: un gr?fico debajo de otro
par(mfrow=c(2,3))      # Matriz de gr?ficos 2 x 3 : dos filas por tres columnas

#Un ejemplo:
  
x = rnorm(200)          # Se generan 200 valores de una normal estandarizada
par(mfrow=c(2,2))      # Se crea una matriz de gr?ficos 2 x 2
plot(x)                # Dibujo de x frente al ?ndice 1 a 200
hist(x)              # Histograma de x
boxplot(x)             # Diagrama de caja de x
qqnorm(x)            # Gr?fico cuantil-cuantil de x frente a la distribuci?n
dev.off()



p<-c(0.01,0.025,0.05,0.10,0.9,0.95,0.975,0.99)
q<-c(-3,-2,-1,0,1,2,3)
x<-seq(from = -4, to =4, by =0.01)
##############################################
#Funcion de distribucion y densidad
pnorm(a, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
b<-dnorm(x, mean = 0, sd = 1, log = FALSE)
plot(x,b,"l")

## cuantiles/ECDF, ecdf() 
plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red")
##############################################

#LOGNORMAL
rlnorm(100, meanlog = 0, sdlog = 1)
b<-dlnorm(x, meanlog = 0, sdlog = 1, log = FALSE) 
plot(x,b,"l")
##############################################

#Poisson
## Discrete Distribution Plot:
plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
     main = "rpois(100, lambda = 5)")

##############################################

#Binomial
a<-rbinom(100, 10, 0.5)
barplot(table(a))
table(a)
b<-factor(a)
table(b)
summary(b)
summary(a)

prop.table(table(b))
round(100*prop.table(table(a)),1)
barplot(table(a))
summary(table(a))
##############################################

#chi2
dchisq(1, df = 1:3)
pchisq(1, df =  3)
pchisq(1, df =  3, ncp = 0:4)  # includes the above

x <- 1:10
## Chi-squared(df = 2) is a special exponential distribution
all.equal(dchisq(x, df = 2), dexp(x, 1/2))
all.equal(pchisq(x, df = 2), pexp(x, 1/2))

## "analytical" test
lam <- seq(0, 100, by = .25)
p00 <- pchisq(0,      df = 0, ncp = lam)
p.0 <- pchisq(1e-300, df = 0, ncp = lam)
stopifnot(all.equal(p00, exp(-lam/2)),
          all.equal(p.0, exp(-lam/2)))
##############################################
## no-centrada df = 0 con ncp > 0:  Z0 tiene probabilidad en 0
Z0 <- rchisq(100, df = 0, ncp = 2.)
graphics::stem(Z0)

## visualizacion
## do P-P plots for 1000 points at various degrees of freedom
L <- 1.2; n <- 1000; pp <- ppoints(n)
op <- par(mfrow = c(3,3), mar = c(3,3,1,1)+.1, mgp = c(1.5,.6,0),
          oma = c(0,0,3,0))
for(df in 2^(4*rnorm(9))) {
  plot(pp, sort(pchisq(rr <- rchisq(n, df = df, ncp = L), df = df, ncp = L)),
       ylab = "pchisq(rchisq(.),.)", pch = ".")
  mtext(paste("df = ", formatC(df, digits = 4)), line =  -2, adj = 0.05)
  abline(0, 1, col = 2)
}
mtext(expression("P-P plots : Noncentral  "*
                   chi^2 *"(n=1000, df=X, ncp= 1.2)"),
      cex = 1.5, font = 2, outer = TRUE)
par(op)

##############################################
#t
require(graphics)

1 - pt(1:5, df = 1)
qt(.975, df = c(1:10,20,50,100,1000))

tt <- seq(0, 10, len = 21)
ncp <- seq(0, 6, len = 31)
ptn <- outer(tt, ncp, function(t, d) pt(t, df = 3, ncp = d))
t.tit <- "Non-central t - Probabilities"
image(tt, ncp, ptn, zlim = c(0,1), main = t.tit)
persp(tt, ncp, ptn, zlim = 0:1, r = 2, phi = 20, theta = 200, main = t.tit,
      xlab = "t", ylab = "non-centrality parameter",
      zlab = "Pr(T <= t)")

plot(function(x) dt(x, df = 3, ncp = 0), -3, 11, ylim = c(0, 0.32),
     main = "Non-central t - Density", yaxs = "i")
##############################################

#t
 
x<-seq(from = -10, to =10, by =0.01)
df<-seq(3:10)
b3<-dt(x, 3, 0, log = FALSE)
b4<-dt(x, 4, 0, log = FALSE)
b10<-dt(x, 10, 0, log = FALSE)

plot(x,b3,"l")
lines(x,b10,col="red")

##############################################

#exp
x<-sort(rexp(100,2))
y<-dexp(x,2)
plot(x,y,"l")

##############################################
#TCL
TamanoMuestra=1000;NumMuestras=100;
muestra=array(0,c(TamanoMuestra,NumMuestras)) 
SumaMuestra=rep(0,TamanoMuestra)# Definir vector con 0's TamanoMuestra veces


for (i in 1:NumMuestras) {
  muestra[,i]=runif(TamanoMuestra,2,4) #llenar columna i con num. aleat. unif.
  SumaMuestra=SumaMuestra+muestra[,i]
}

hist(SumaMuestra)
qqnorm(SumaMuestra)
qqplot(SumaMuestra, dist= "norm", labels=FALSE)
numSummary(SumaMuestra, statistics=c("mean", "sd"))

##############################################

#Multivariate normal  
library(MASS)
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
b<-mvrnorm(n = 1000, rep(0, 2), Sigma)
plot(b)
var(mvrnorm(n = 1000, rep(0, 2), Sigma))
#var(mvrnorm(n = 1000, rep(0, 2), Sigma, empirical = TRUE)

