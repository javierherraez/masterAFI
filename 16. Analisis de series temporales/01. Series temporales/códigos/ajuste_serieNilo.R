#############################################
#             AJUSTE SERIE NILO             #
#############################################

# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde est?n los datos

setwd("C:/Users/jherraez/Documents/masterAFI/16. Analisis de series temporales/01. Series temporales/datos/")

# Carga de datos

# Se reservan los 10 ?ltimos a?os para hacer la predicci?n

datos <- read.csv("nilo.csv")
datos.train <- subset(datos, ANYO<1275)

# Se convierte en un objeto de tipo timeseries

datos.train.ts <- as.ts(datos.train$NIVEL_NILO)

datos.test <- subset(datos, ANYO>=1275)
datos.test.ts <- as.ts(datos.test$NIVEL_NILO)

##############################################
# [1]. Se representan gr?ficamente los datos #
##############################################

#install.packages("ggplot2")
library(ggplot2)
ggplot(aes(x= ANYO, y = NIVEL_NILO), data = datos.train) + geom_line(color = '#d84519') + 
  xlab('ANYO') + ylab('NIVEL NILO')

#install.packages("plotly")
library(plotly)

graficoSerie <- ggplot(aes(x= ANYO, y = NIVEL_NILO), data = datos.train) +
  geom_line(color = '#d84519',
            size = 1) + 
  xlab('ANYO') + ylab('NIVEL NILO')
ggplotly(graficoSerie)

#########################################################################
# [2]. Se estudia la estacionariedad en media y en varianza de la serie #
#########################################################################

# install.packages("tseries")
library(tseries)

# [2.1] ?Es estacionaria en varianza?

# Se eval?a la necesidad de transformar la serie para hacerla
# estacionaria en varianza

install.packages("MASS")
library(MASS)

box_cox <- boxcox(NIVEL_NILO ~ ANYO,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

# El valor m?s alto de la verosimilitu se obtiene para lambda = 0 -> logaritmo

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

datos.train$LOG_NIVEL_NILO=log(datos.train$NIVEL_NILO)
datos.train.ts <- as.ts(datos.train$LOG_NIVEL_NILO)

datos.test$LOG_NIVEL_NILO=log(datos.test$NIVEL_NILO)
datos.test.ts <- as.ts(datos.test$LOG_NIVEL_NILO)

# [2.2] ?Es estacionaria en media? (?Hay que diferenciarla?)

# Test de Dickey-Fuller

library(tseries)
adf.test(datos.train.ts, alternative="stationary")

# p-value = 0.01 -> Se rechaza la existencia de ra?ces unitarias

#############################################
# [3]. Ajuste de un modelo ARIMA a la serie #
#############################################

# install.packages("forecast")
library(forecast)

graphics.off()

# Funciones de autocorrelaci?n de la serie 

acf(datos.train.ts, lag.max = 48, xlab = "Retardo",
    main= "Funci?n de autocorrelaci?n simple")

# MA INFINITO

pacf(datos.train.ts, lag.max = 48, xlab = "Retardo",
     main = "Funci?n de autocorrelaci?n parcial")

# AR(1)

ajuste_AR1 <- Arima(datos.train.ts,order = c(1,0,0))

# install.packages("lmtest")
library(lmtest)

# coef diferenciar si 1 entra en (estimación +- 2*std.error)

coeftest(ajuste_AR1)
 
# install.packages("caschrono")
library(caschrono)

cor.arma(ajuste_AR1)

# No hay ruido blanco

graphics.off()

# Funciones de autocorrelaci?n del residuo tras el ajuste de un AR(1)

acf(ajuste_AR1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Funci?n de autocorrelaci?n simple")

# Box.test.2(residuals(ajuste_AR1),
#            nlag = c(6,12,18,24,30,36,42,48),
#            type="Ljung-Box")

# intentando que haya ruido blanco
# si por ejmplo, para 6 el p-valor es pequeño rechazamos que sea ruido blanco para los 6 primeros valores
# es decir, que se salen de la banda
LjungBox(ajuste_AR1$residuals, c(6,12,18,24,30,36,42,48))

# MA(1) -> ARMA(1,1)

pacf(ajuste_AR1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Funci?n de autocorrelaci?n parcial")

ajuste_AR1_MA1 <- Arima(datos.train.ts,order = c(1,0,1))

coeftest(ajuste_AR1_MA1)

cor.arma(ajuste_AR1_MA1)

# Correlaci?n alta entre par?metros!

# Box.test.2(residuals(ajuste_AR1_MA1),
#            nlag = c(6,12,18,24,30,36,42,48),
#            type="Ljung-Box")


LjungBox(ajuste_AR1_MA1$residuals, c(6,12,18,24,30,36,42,48))

# Ruido blanco para alfa = 0,01

graphics.off()

# Funciones de autocorrelaci?n del residuo tras el ajuste de un ARMA(1,1)

# miramos acf y pacf, ¿meter ma2 o ar2?, lo que dice el profesor? meter la q (aunque comprobar las dos) 

acf(ajuste_AR1_MA1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Funci?n de autocorrelaci?n simple")

# MA(2) -> ARMA(1,2)

pacf(ajuste_AR1_MA1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Funci?n de autocorrelaci?n parcial")

# O AR(2) -> ARMA(2,1)

# Empezamos con el ARMA(1,2)

ajuste_AR1_MA2 <- Arima(datos.train.ts,order = c(1,0,2))

coeftest(ajuste_AR1_MA2)

cor.arma(ajuste_AR1_MA2)

# Podr?a sugerirse un orden de diferenciaci?n
# Las correlaciones entre los par?metros no son altas

Box.test.2(residuals(ajuste_AR1_MA2),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Ruido blanco

graphics.off()

# Probamos el ARMA(2,1)

ajuste_AR2_MA1 <- Arima(datos.train.ts,
                        order = c(2,0,1),
                        method = "ML")

coeftest(ajuste_AR2_MA1)

cor.arma(ajuste_AR2_MA1)

# Las correlaciones entre los par?metros son muy altas

Box.test.2(residuals(ajuste_AR2_MA1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se tiene ruido blanco, pero las correlaciones altas
# nos llevan a decantarnos por el ARMA(1,2)

graphics.off()

###################
# [4]. Predicci?n #
###################

# Error sobre training

pred <- as.data.frame(ajuste_AR1_MA2$fitted)
names(pred) <- "prediccion"
errorTraining <- cbind(datos.train,pred)
errorTraining$APEDiario <- abs(100*(errorTraining$NIVEL_NILO-exp(errorTraining$prediccion))/errorTraining$NIVEL_NILO)
mean(errorTraining$APEDiario)

# 4.577018 #

# Error sobre test

prediccionTest <- as.data.frame(predict(ajuste_AR1_MA2, n.ahead=10))

# C?lculo de errores

errorTest <- cbind(datos.test,prediccionTest)
errorTest$APEDiario <- abs(100*(errorTest$NIVEL_NILO-exp(errorTest$pred))/errorTest$NIVEL_NILO)
mean(errorTest$APEDiario)

# 5.290345. A horizonte 1: 4.8874394 #

# Representaci?n gr?fica predicci?n

# L?mites de confianza al 95%

U <- exp(prediccionTest$pred + 1.96*prediccionTest$se)
L <- exp(prediccionTest$pred - 1.96*prediccionTest$se)

# Se destransforma usando Nelson para que la predicci?n quede centrada en el intervalo

datos.pred <- data.frame(ANYO = datos.test$ANYO, 
                         Prediccion = exp(prediccionTest$pred+0.5*prediccionTest$se^2),
                         LimSup = U, LimInf =L)

datos.real.pred <- merge(datos, datos.pred, by = "ANYO", all.x = T)

library(plotly)

datos.real.pred$NIVEL_NILO[datos.real.pred$ANYO>1975] <- NA

graficoPrediccion <- ggplot(data = datos.real.pred) +
  geom_line(aes(x= ANYO, y = NIVEL_NILO), color = 'steelblue',
            alpha = 0.8, size = 0.8) +
  geom_line(aes(x= ANYO, y = Prediccion), color = 'darkred',
            size = 1)   +
  geom_line(aes(x= ANYO, y = LimSup), color = 'orange',
            size = 1)  +
  geom_line(aes(x= ANYO, y = LimInf), color = 'orange',
            size = 1) +
  xlab('ANYO') + ylab('NIVEL NILO')

ggplotly(graficoPrediccion)