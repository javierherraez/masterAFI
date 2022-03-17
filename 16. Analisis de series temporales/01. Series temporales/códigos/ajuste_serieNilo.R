#############################################
#             AJUSTE SERIE NILO             #
#############################################

# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("C:/Proyectos/UNIVERSIDAD/20212022/SSTT/data")

# Carga de datos

# Se reservan los 10 últimos años para hacer la predicción

datos <- read.csv("nilo.csv")
datos.train <- subset(datos, ANYO<1275)

# Se convierte en un objeto de tipo timeseries

datos.train.ts <- as.ts(datos.train$NIVEL_NILO)

datos.test <- subset(datos, ANYO>=1275)
datos.test.ts <- as.ts(datos.test$NIVEL_NILO)

##############################################
# [1]. Se representan gráficamente los datos #
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

# [2.1] ¿Es estacionaria en varianza?

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

install.packages("MASS")
library(MASS)

box_cox <- boxcox(NIVEL_NILO ~ ANYO,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

# El valor más alto de la verosimilitu se obtiene para lambda = 0 -> logaritmo

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

datos.train$LOG_NIVEL_NILO=log(datos.train$NIVEL_NILO)
datos.train.ts <- as.ts(datos.train$LOG_NIVEL_NILO)

datos.test$LOG_NIVEL_NILO=log(datos.test$NIVEL_NILO)
datos.test.ts <- as.ts(datos.test$LOG_NIVEL_NILO)

# [2.2] ¿Es estacionaria en media? (¿Hay que diferenciarla?)

# Test de Dickey-Fuller

library(tseries)
adf.test(datos.train.ts, alternative="stationary")

# p-value = 0.01 -> Se rechaza la existencia de raíces unitarias

#############################################
# [3]. Ajuste de un modelo ARIMA a la serie #
#############################################

# install.packages("forecast")
library(forecast)

graphics.off()

# Funciones de autocorrelación de la serie 

acf(datos.train.ts, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# MA INFINITO

pacf(datos.train.ts, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# AR(1)

ajuste_AR1 <- Arima(datos.train.ts,order = c(1,0,0))

# install.packages("lmtest")
library(lmtest)

coeftest(ajuste_AR1)

# install.packages("caschrono")
library(caschrono)

cor.arma(ajuste_AR1)

Box.test.2(residuals(ajuste_AR1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# No hay ruido blanco

graphics.off()

# Funciones de autocorrelación del residuo tras el ajuste de un AR(1)

acf(ajuste_AR1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# MA(1) -> ARMA(1,1)

pacf(ajuste_AR1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

ajuste_AR1_MA1 <- Arima(datos.train.ts,order = c(1,0,1))

coeftest(ajuste_AR1_MA1)

cor.arma(ajuste_AR1_MA1)

# Correlación alta entre parámetros!

Box.test.2(residuals(ajuste_AR1_MA1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Ruido blanco para alfa = 0,01

graphics.off()

# Funciones de autocorrelación del residuo tras el ajuste de un ARMA(1,1)

acf(ajuste_AR1_MA1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# MA(2) -> ARMA(1,2)

pacf(ajuste_AR1_MA1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# O AR(2) -> ARMA(2,1)

# Empezamos con el ARMA(1,2)

ajuste_AR1_MA2 <- Arima(datos.train.ts,order = c(1,0,2))

coeftest(ajuste_AR1_MA2)

cor.arma(ajuste_AR1_MA2)

# Podría sugerirse un orden de diferenciación
# Las correlaciones entre los parámetros no son altas

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

# Las correlaciones entre los parámetros son muy altas

Box.test.2(residuals(ajuste_AR2_MA1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se tiene ruido blanco, pero las correlaciones altas
# nos llevan a decantarnos por el ARMA(1,2)

graphics.off()

###################
# [4]. Predicción #
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

# Cálculo de errores

errorTest <- cbind(datos.test,prediccionTest)
errorTest$APEDiario <- abs(100*(errorTest$NIVEL_NILO-exp(errorTest$pred))/errorTest$NIVEL_NILO)
mean(errorTest$APEDiario)

# 5.290345. A horizonte 1: 4.8874394 #

# Representación gráfica predicción

# Límites de confianza al 95%

U <- exp(prediccionTest$pred + 1.96*prediccionTest$se)
L <- exp(prediccionTest$pred - 1.96*prediccionTest$se)

# Se destransforma usando Nelson para que la predicción quede centrada en el intervalo

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