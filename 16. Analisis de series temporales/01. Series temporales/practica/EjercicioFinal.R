#############################################
#             SERIES TEMPORALES             #
#############################################

# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde est?n los datos

setwd("C:/Users/Javier/Documents/masterAFI/16. Analisis de series temporales/01. Series temporales/practica/")

# Carga de datos

datos <- read.csv("_11_CACERES.csv")
datos$FECHA=as.Date(datos$FECHA,format='%d/%m/%Y')
datos.train <- subset(datos, FECHA<=as.Date('01/12/2018',format='%d/%m/%Y'))
datos.test <- subset(datos, FECHA>as.Date('01/12/2018',format='%d/%m/%Y'))

# [1]. Se convierten los datos en un objeto de tipo time series
#      para poder aplicar sobre ellos las funciones de R
################################################################

datos.train.ts <- as.ts(datos.train$TARGET, frequency = 12)
datos.test.ts <- as.ts(datos.test$TARGET, frequency=12)

# [2]. Se representan gr?ficamente los datos
################################################################

# install.packages("ggplot2")
library(ggplot2)

# install.packages("plotly")
library(plotly)

names(datos.train)

graficoInicial <- ggplot(aes(x= FECHA, y = TARGET), data = datos.train) +
  geom_line(color = '#d84519',
            size = 1) + 
  xlab('FECHA') + ylab('Matriculaciones')

ggplotly(graficoInicial)



# [3]. Se estudia la estacionariedad en media y en varianza
#      de la serie
################################################################

# install.packages("tseries")
library(tseries)

# [3.1] ?Es estacionaria en varianza?

# Se eval?a la necesidad de transformar la serie para hacerla
# estacionaria en varianza

# install.packages("MASS")
library(MASS)

box_cox <- boxcox(TARGET ~ FECHA,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# -> Se toma logaritmo # ha tomado logaritmo porque esta cerca de 0

datos.train$log_target=log(datos.train$TARGET)
datos.test$log_target=log(datos.test$TARGET)
datos.train.ts <- as.ts(datos.train$log_target, frequency = 12)
datos.test.ts <- as.ts(datos.test$log_target, frequency=12)

# [3.2] ?Es estacionaria en media? (?Hay que diferenciarla?)

# Test de Dickey-Fuller # NO somos muy amigos de este test porque es poco exigente

adf.test(datos.train.ts)

# p-value = 0.7159 -> Podría ser necesaria una diferencia regular, pero nosotros ajustamos directamente sin nada

# [4]. Ajuste de un modelo ARIMA a la serie

# install.packages("forecast")
library(forecast)

graphics.off()

acf(datos.train.ts, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

pacf(datos.train.ts, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# Se propone un SARIMA(1,0,0)x(0,0,0)12 + MU

ajuste1 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(0,0,0), 
                                 period = 12),
                 method = "ML")
ajuste1

# install.packages("lmtest")
library(lmtest)

coeftest(ajuste1)

# install.packages("caschrono")
library(caschrono)

cor.arma(ajuste1) # debajo de 0.8 todo, no correlaciones

Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")                   # queremos valores >0.05 en todos

graphics.off()

acf(ajuste1$residuals, lag.max = 48, xlab = "Retardo", # aquí vemos que sólo se salen en multiplos de 12 y que hay patrón de simetría
    main= "Función de autocorrelación simple")

pacf(ajuste1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# Se propone un SARIMA(1,0,0)x(1,0,0)12 + MU

ajuste2 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,0,0), period = 12),
                 method = "ML")

coeftest(ajuste2)

cor.arma(ajuste2)

Box.test.2(residuals(ajuste2),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box") # test de jung-box

graphics.off()

acf(ajuste2$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

pacf(ajuste2$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")
