#############################################
#             SERIES TEMPORALES             #
#############################################

# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("C:/Proyectos/UNIVERSIDAD/20212022/SSTT/data")

# Carga de datos

datos <- read.csv("demandaGasIndustrial.csv",sep=";")

# Breve estudio de los datos cargados

class(datos)
str(datos)
dim(datos)
colnames(datos)
summary(datos)
head(datos)
tail(datos)

# Se convierte la FECHA de factor a objeto de tipo FECHA

datos$FECHA <- as.Date(datos$FECHA, "%m/%d/%Y")
head(datos)

# [1]. Se representan gráficamente los datos
################################################################

install.packages("ggplot2")
library(ggplot2)

ggplot(aes(x= FECHA, y = DEMANDA_GAS), data = datos) + geom_line(color = '#d84519') + 
  xlab('FECHA') + ylab('Demanda de gas') + scale_x_date(date_breaks = '4 months', date_labels = "%m/%y")

# [2]. Se convierten los datos en un objeto de tipo time series
#      para poder aplicar sobre ellos las funciones de R
################################################################

datos.ts <- ts(datos$DEMANDA_GAS, frequency=7)
class(datos.ts)

# [3]. Se divide la muestra en entrenamiento y validación
################################################################

datos.train <- subset(datos, "1996-07-01"<=FECHA & FECHA<="1999-06-30")
datos.train.ts <- as.ts(datos.train$DEMANDA_GAS, frequency = 7)

datos.validate <- subset(datos, FECHA>"1999-06-30")
datos.validate.ts <- as.ts(datos.validate$DEMANDA_GAS, frequency=7)

# [4]. Se estudia la estacionariedad en media y en varianza
#      de la serie
################################################################

install.packages("tseries")
library(tseries)

# [5.1] ¿Es estacionaria en media? (¿Hay que diferenciarla?)

# Test de Dickey-Fuller

adf.test(datos.train.ts, alternative="stationary")

# p-valor bajo < 0.05 -> es estacionaria en media

# [5.2] ¿Es estacionaria en varianza?

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

install.packages("MASS")
library(MASS)

box_cox <- boxcox(DEMANDA_GAS ~ FECHA,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# -> No es necesario transformar los datos

# [5]. Ajuste de un modelo ARIMA a la serie
################################################################

# [5.1]. Funciones de autocorrelación simple y parcial

acf(datos.train.ts, lag.max = 25, xlab = "Retardo",
    main= "Función de autocorrelación simple")

pacf(datos.train.ts, lag.max = 25, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# NOTA: Se puede comprobar que el dato de correlación es
# prácticamente igual que el obtenido en el gráfico:

# [5.2]. Ajuste de modelo ARIMA

# [5.2.1]. AR(1)

install.packages("forecast")
library(forecast)

ajuste1 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 method = "ML")
ajuste1

# Para ver p-valores de los parámetros

install.packages("lmtest")
library(lmtest)

coeftest(ajuste1)

# Matriz de correlación de parámetros estimados

install.packages("caschrono")
library(caschrono)

cor.arma(ajuste1)

# Test de ruido blanco

Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.2]. SAR(1)x(1)[7]

graphics.off()

acf(ajuste1$residuals, lag.max = 25, xlab = "Retardo", main="")
pacf(ajuste1$residuals, lag.max = 25, xlab = "Retardo", main="")

# Los gráficos son muy parecidos a estos, porque como el
# parámetro del AR está muy cerca de 1, es casi como si
# hubiésemos diferenciado:

acf(diff(datos.train.ts))
pacf(diff(datos.train.ts))

ajuste2 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,0,0), period = 7),
                 method = "ML")

ajuste2
coeftest(ajuste2)

# Matriz de correlación de parámetros estimados

cor.arma(ajuste2)

# Test de ruido blanco

Box.test.2(residuals(ajuste2),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.3]. SARIMA(1,0,0)x(1,0,1)[7]

graphics.off()

acf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")
pacf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")

ajuste3 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,0,1), period = 7),
                 method = "ML")

ajuste3
coeftest(ajuste3)

# Raíz unitaria -> Se reemplaza AR(7) por diferencia estacional

# [5.2.4]. SARIMA(1,0,0)x(0,1,1)[7]

graphics.off()

ajuste4 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(0,1,1), period = 7),
                 #include.constant = T,
                 method = "ML")

ajuste4
coeftest(ajuste4)

# Matriz de correlación de parámetros estimados

cor.arma(ajuste4)

# Test de ruido blanco

Box.test.2(residuals(ajuste4),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.5]. SARIMA(1,0,0)x(1,1,1)[7]

graphics.off()

acf(ajuste4$residuals, lag.max=25, xlab="Retardo", main="")
pacf(ajuste4$residuals, lag.max=25, xlab="Retardo", main="")

ajuste5 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,1,1), period = 7),
                 method = "ML")

ajuste5
coeftest(ajuste5)

# Matriz de correlación de parámetros estimados

cor.arma(ajuste5)

# Test de ruido blanco

Box.test.2(residuals(ajuste5),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.6]. SARIMA(1,0,2)x(1,1,1)[7]

graphics.off()

acf(ajuste5$residuals, lag.max=25, xlab="Retardo", main="")
pacf(ajuste5$residuals, lag.max=25, xlab="Retardo", main="")

ajuste6 <- Arima(datos.train.ts,
                 order = c(1,0,2),
                 seasonal = list(order = c(1,1,1), period=7),
                 method="ML")

ajuste6
coeftest(ajuste6)

# Matriz de correlación de parámetros estimados

cor.arma(ajuste6)

# Test de ruido blanco

Box.test.2(residuals(ajuste6),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# MODELO ALTERNATIVO: Se cambia el AR(1) por una diferencia regular

graphics.off()

ajusteAlt <- Arima(datos.train.ts,
                   order = c(0,1,2),
                   seasonal = list(order = c(1,1,1), period=7),
                   method="ML")

ajusteAlt
coeftest(ajusteAlt)

# Matriz de correlación de parámetros estimados

cor.arma(ajusteAlt)

# Test de ruido blanco

Box.test.2(residuals(ajusteAlt),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

acf(ajusteAlt$residuals, lag.max=25, xlab="Retardo", main="")

# Residuo antes de incluir intervenciones

acf(ajuste6$residuals, lag.max=25, xlab="Retardo", main="")
pacf(ajuste6$residuals, lag.max=25, xlab="Retardo", main="")

# [6]. Inclusión de variables explicativas
################################################################

# Se definen los regresores (festividades y temperaturas)
# Dejamos FECHA, TMAX y TMIN

calendario <- datos[,c(-2)]

# Se crean el día de la semana, el mes, etc, y a partir de
# ellos se definen los festivos

install.packages("lubridate")
library(lubridate)

calendario$diaSemana <- wday(calendario$FECHA)
calendario$diaMes <- day(calendario$FECHA)
calendario$mes <- month(calendario$FECHA)

# Se definen los festivos nacionales

calendario$p_01ene <- ifelse(calendario$diaMes==1 & calendario$mes==1, 1, 0)
calendario$p_06ene <- ifelse(calendario$diaMes==6 & calendario$mes==1, 1, 0)
calendario$p_19mar <- ifelse(calendario$diaMes==19 & calendario$mes==3, 1, 0)
calendario$p_01may <- ifelse(calendario$diaMes==1 & calendario$mes==5, 1, 0)
calendario$p_15ago <- ifelse(calendario$diaMes==15 & calendario$mes==8, 1, 0)
calendario$p_12oct <- ifelse(calendario$diaMes==12 & calendario$mes==10,1, 0)
calendario$p_01nov <- ifelse(calendario$diaMes==1 & calendario$mes==11, 1 ,0)
calendario$p_06dic <- ifelse(calendario$diaMes==6 & calendario$mes==12, 1 ,0)
calendario$p_08dic <- ifelse(calendario$diaMes==8 & calendario$mes==12, 1 ,0)
calendario$p_25dic <- ifelse(calendario$diaMes==25 & calendario$mes==12, 1 ,0)

#######################################
#     Cálculo de la Semana Santa      #
#######################################

# Función Easter de timeDate

# install.packages("timeDate")
library(timeDate)

domingoResurrecion <- as.Date(Easter(year(min(calendario$FECHA)):year(max(calendario$FECHA))))
juevesSanto <- domingoResurrecion-3
juevesSanto <- data.frame(FECHA=juevesSanto, juevesSanto=rep(1,length(juevesSanto)))
viernesSanto <- domingoResurrecion-2
viernesSanto <- data.frame(FECHA=viernesSanto, viernesSanto=rep(1,length(viernesSanto)))
lunesPascua <- domingoResurrecion+1
lunesPascua <- data.frame(FECHA=lunesPascua, lunesPascua=rep(1,length(lunesPascua)))

# Se añaden a la tabla maestra de fechas

calendario <- merge(x = calendario, y = juevesSanto, by = "FECHA", all.x = TRUE)
calendario <- merge(x = calendario, y = viernesSanto, by = "FECHA", all.x = TRUE)
calendario <- merge(x = calendario, y = lunesPascua, by = "FECHA", all.x = TRUE)

# Se reemplazan los NAs por 0

calendario$juevesSanto[is.na(calendario$juevesSanto)] <- 0
calendario$viernesSanto[is.na(calendario$viernesSanto)] <- 0
calendario$lunesPascua[is.na(calendario$lunesPascua)] <- 0

# Se generan variables de temperatura retardadas

calendario$TMAX_1=(dplyr::lag(calendario$TMAX, 1))
calendario$TMAX_2=(dplyr::lag(calendario$TMAX, 2))
calendario$TMAX_1[is.na(calendario$TMAX_1)] <- 0
calendario$TMAX_2[is.na(calendario$TMAX_2)] <- 0

calendario$TMIN_1=(dplyr::lag(calendario$TMIN, 1))
calendario$TMIN_2=(dplyr::lag(calendario$TMIN, 2))
calendario$TMIN_1[is.na(calendario$TMIN_1)] <- 0
calendario$TMIN_2[is.na(calendario$TMIN_2)] <- 0

variablesExcluidas <- names(calendario) %in% c("diaSemana", "diaMes", "mes", "anyo") 
calendario <- calendario[!variablesExcluidas]

calendario.train <- subset(calendario, "1996-07-01"<=FECHA & FECHA<="1999-06-30")
calendario.train <- as.matrix(calendario.train[,2:ncol(calendario.train)])
names(as.data.frame(calendario.train))

calendario.validate <- subset(calendario,FECHA>"1999-06-30")
calendario.validate <- as.matrix(calendario.validate[,2:ncol(calendario.validate)])

# Se añaden los festivos al Arima ajustado:

ajuste6ConFestivos <- Arima(datos.train.ts,
                            order = c(1,0,2),
                            seasonal = list(order = c(1,1,1), period=7),
                            method="ML",
                            xreg = calendario.train[,3:15])

ajuste6ConFestivos
coeftest(ajuste6ConFestivos)

Box.test.2(residuals(ajuste6ConFestivos),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# El MA(2) y el Jueves Santo no son significativos

ajuste6ConFestivos <- Arima(datos.train.ts,
                            order = c(1,0,1),
                            seasonal = list(order = c(1,1,1), period=7),
                            method="ML",
                            xreg = calendario.train[,c(3:12,14:15)])

ajuste6ConFestivos
coeftest(ajuste6ConFestivos)

Box.test.2(residuals(ajuste6ConFestivos),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se añaden las temperaturas al Arima ajustado:

ajuste6ConFestivosYTemperaturas <- Arima(datos.train.ts,
                            order = c(1,0,1),
                            seasonal = list(order = c(1,1,1), period=7),
                            method="ML",
                            xreg = calendario.train[,c(3:12,14:15,1:2,16:19)])

ajuste6ConFestivosYTemperaturas
coeftest(ajuste6ConFestivosYTemperaturas)

# Solo es significativa TMAX, TMIN y TMAX_1

ajuste6ConFestivosYTemperaturas <- Arima(datos.train.ts,
                                         order = c(1,0,1),
                                         seasonal = list(order = c(1,1,1), period=7),
                                         method="ML",
                                         xreg = calendario.train[,c(3:12,14:15,1:2,16:16)])

ajuste6ConFestivosYTemperaturas
coeftest(ajuste6ConFestivosYTemperaturas)

# Y ahora ni siquiera TMAX_1

ajuste6ConFestivosYTemperaturas <- Arima(datos.train.ts,
                                         order = c(1,0,1),
                                         seasonal = list(order = c(1,1,1), period=7),
                                         method="ML",
                                         xreg = calendario.train[,c(3:12,14:15,1:2)])

ajuste6ConFestivosYTemperaturas
coeftest(ajuste6ConFestivosYTemperaturas)

Box.test.2(residuals(ajuste6ConFestivosYTemperaturas),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Comparativa del test de ruido blanco con el modelo alternativo

ajuste6AltConFestivosYTemperaturas <- Arima(datos.train.ts,
                                         order = c(0,1,2),
                                         seasonal = list(order = c(1,1,1), period=7),
                                         method="ML",
                                         xreg = calendario.train[,c(3:12,14:15,1:2)])

ajuste6AltConFestivosYTemperaturas
coeftest(ajuste6AltConFestivosYTemperaturas)

# El MA(2) se vuelve a perder

ajuste6AltConFestivosYTemperaturas <- Arima(datos.train.ts,
                                            order = c(0,1,1),
                                            seasonal = list(order = c(1,1,1), period=7),
                                            method="ML",
                                            xreg = calendario.train[,c(3:12,14:15,1:2)])

ajuste6AltConFestivosYTemperaturas
coeftest(ajuste6AltConFestivosYTemperaturas)

Box.test.2(residuals(ajuste6AltConFestivosYTemperaturas),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# El RB del otro parece un poco mejor

# [7]. Identificación de outliers
################################################################

install.packages("tsoutliers")
library(tsoutliers)

# Esto identifica outliers

listaOutliers <- locate.outliers(ajuste6ConFestivosYTemperaturas$residuals,
                                 pars = coefs2poly(ajuste6ConFestivosYTemperaturas),
                                 types = c("AO", "LS", "TC"),cval=5)
listaOutliers

# Para incluir los outliers. No es necesario porque ya se tenía RB

outliers <- outliers(c("AO", "LS", "TC"), c(830, 907, 177))
outliersVariables <- outliers.effects(outliers, length(ajuste6ConFestivosYTemperaturas$residuals))
calendarioMasOutliers <- cbind(calendario.train[,c(3:12,14:15,1:2)],outliersVariables)
ajusteConOutliers <- Arima(datos.train.ts,
                            order = c(1,0,1),
                            seasonal = list(order = c(1,1,1), period=7),
                            method="ML",
                            xreg = calendarioMasOutliers)


ajusteConOutliers
coeftest(ajusteConOutliers)

# [8]. Pintamos la predicción a futuro
################################################################

# [8.1]. Sin festivos:

horizontePrediccion <- 366

prediccion <- as.data.frame(predict(ajuste6, n.ahead=horizontePrediccion))

# Límites de confianza al 95%

U <- prediccion$pred + 2*prediccion$se
L <- prediccion$pred - 2*prediccion$se

ts.plot(datos.train.ts, prediccion$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),
       col=c(1,2,4), lty=c(1,1,2))

# [8.2]. Incluyendo festivos:

prediccionFes <- as.data.frame(predict(ajuste6ConFestivosYTemperaturas,
                                       newxreg = calendario.validate[1:horizontePrediccion,c(3:12,14:15,1:2)],
                                       n.ahead= horizontePrediccion))

# Límites de confianza al 95%

U <- prediccionFes$pred + 2*prediccionFes$se
L <- prediccionFes$pred - 2*prediccionFes$se

ts.plot(datos.train.ts, prediccionFes$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),
       xjust = 0, yjust = 1,
       col=c(1,2,4), lty=c(1,1,2))

# [9]. Cálculo de errores (MAPE)
################################################################

# [9.1]. Error in-sampling

# Sin incluir festivos:

pred <- as.data.frame(ajuste6$fitted)
names(pred) <- "DEMANDA_GAS_PRED"
realYPrediccion <- cbind(datos.train,pred)
realYPrediccion$MAPEDiario <- abs(100*(realYPrediccion$DEMANDA_GAS-realYPrediccion$DEMANDA_GAS_PRED)/realYPrediccion$DEMANDA_GAS)

mean(realYPrediccion$MAPEDiario)

# 2.982843

# Incluyendo festivos

predFes <- as.data.frame(ajuste6ConFestivos$fitted)
names(predFes) <- "DEMANDA_GAS_PRED"
realYPrediccionFes <- cbind(datos.train,predFes)
realYPrediccionFes$MAPEDiario <- abs(100*(realYPrediccionFes$DEMANDA_GAS-realYPrediccionFes$DEMANDA_GAS_PRED)/realYPrediccionFes$DEMANDA_GAS)

mean(realYPrediccionFes$MAPEDiario)

# 2.44827

# El error in-sampling global disminuye al incluir los festivos.

# Incluyendo festivos y temperaturas

predFes <- as.data.frame(ajuste6ConFestivosYTemperaturas$fitted)
names(predFes) <- "DEMANDA_GAS_PRED"
realYPrediccionFes <- cbind(datos.train,predFes)
realYPrediccionFes$MAPEDiario <- abs(100*(realYPrediccionFes$DEMANDA_GAS-realYPrediccionFes$DEMANDA_GAS_PRED)/realYPrediccionFes$DEMANDA_GAS)

mean(realYPrediccionFes$MAPEDiario)

# 2.38915

# [9.2]. Error out-of-sampling

# Sin festivos:

predFutura <- as.data.frame(predict(ajuste6, n.ahead=nrow(datos.validate)))
names(predFutura) <- c("DEMANDA_GAS_PRED","SE")
realYPrediccionFut <- cbind(datos.validate,predFutura)
realYPrediccionFut$MAPEDiario <- abs(100*(realYPrediccionFut$DEMANDA_GAS-realYPrediccionFut$DEMANDA_GAS_PRED)/realYPrediccionFut$DEMANDA_GAS)

# MAPE global 

mean(realYPrediccionFut[,"MAPEDiario"])

# 17.92781

# MAPE a 1 día a futuro:

mean(realYPrediccionFut[1:1,"MAPEDiario"])

# 6.399155 (Anecdótico, poca muestra. Meter en bucle)

# MAPE durante los 7 primer días a futuro

mean(realYPrediccionFut[1:7,"MAPEDiario"])

# 5.289403 (Anecdótico, poca muestra. Meter en bucle)

# Incluyendo festivos:

predFuturaFes <- as.data.frame(predict(ajuste6ConFestivos, newxreg = calendario.validate[,c(3:12,14:15)], n.ahead=nrow(datos.validate)))
names(predFuturaFes) <- c("DEMANDA_GAS_PRED","SE")
realYPrediccionFutFes <- cbind(datos.validate,predFuturaFes)
realYPrediccionFutFes$MAPEDiario <- abs(100*(realYPrediccionFutFes$DEMANDA_GAS-realYPrediccionFutFes$DEMANDA_GAS_PRED)/realYPrediccionFutFes$DEMANDA_GAS)

# MAPE global 

mean(realYPrediccionFutFes[,"MAPEDiario"])

# 16.85464<17.92781

# El error out-of-sampling global disminuye al incluir los festivos.

# MAPE a 1 día a futuro:

mean(realYPrediccionFutFes[1:1,"MAPEDiario"])

# 6.707075 (Anecdótico, poca muestra. Meter en bucle)

# MAPE durante los 7 primer días a futuro

mean(realYPrediccionFutFes[1:7,"MAPEDiario"])

# 5.57677 (Anecdótico, poca muestra. Meter en bucle)

# Incluyendo festivos y temperaturas:

predFuturaFesTemp <- as.data.frame(predict(ajuste6ConFestivosYTemperaturas, newxreg = calendario.validate[,c(3:12,14:15,1:2)], n.ahead=nrow(datos.validate)))
names(predFuturaFesTemp) <- c("DEMANDA_GAS_PRED","SE")
realYPrediccionFutFesTemp <- cbind(datos.validate,predFuturaFesTemp)
realYPrediccionFutFesTemp$MAPEDiario <- abs(100*(realYPrediccionFutFesTemp$DEMANDA_GAS-realYPrediccionFutFesTemp$DEMANDA_GAS_PRED)/realYPrediccionFutFesTemp$DEMANDA_GAS)

# MAPE global 

mean(realYPrediccionFutFesTemp[,"MAPEDiario"])

# 16.25688<16.85464

# El error out-of-sampling global disminuye al incluir las temperaturas.

# MAPE a 1 día a futuro:

mean(realYPrediccionFutFesTemp[1:1,"MAPEDiario"])

# 6.979638 (Anecdótico, poca muestra. Meter en bucle)

# MAPE durante los 7 primer días a futuro

mean(realYPrediccionFutFesTemp[1:7,"MAPEDiario"])

# 6.127714 (Anecdótico, poca muestra. Meter en bucle)