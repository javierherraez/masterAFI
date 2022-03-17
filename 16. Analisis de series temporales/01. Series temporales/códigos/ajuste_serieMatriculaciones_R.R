#############################################
#             SERIES TEMPORALES             #
#############################################

# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("C:/Proyectos/UNIVERSIDAD/20212022/SSTT/data")

# Carga de datos

datos <- read.csv("series_matriculaciones_tur_paro.csv")
datos$fecha=as.Date(datos$fecha,format='%m/%d/%Y')
datos.train <- subset(datos, fecha<=as.Date('12/01/2016',format='%m/%d/%Y'))
datos.test <- subset(datos, fecha>as.Date('12/01/2016',format='%m/%d/%Y'))

# [1]. Se convierten los datos en un objeto de tipo time series
#      para poder aplicar sobre ellos las funciones de R
################################################################

datos.train.ts <- as.ts(datos.train$target, frequency = 12)
datos.test.ts <- as.ts(datos.test$target, frequency=12)

# [2]. Se representan gráficamente los datos
################################################################

install.packages("ggplot2")
library(ggplot2)

install.packages("plotly")
library(plotly)

names(datos.train)

graficoInicial <- ggplot(aes(x= fecha, y = target), data = datos.train) +
  geom_line(color = '#d84519',
            size = 1) + 
  xlab('FECHA') + ylab('Matriculaciones')

names(datos.train)
ggplot(aes(x= fecha, y = target), data = datos.train) + geom_line(color = '#d84519') + 
  xlab('FECHA') + ylab('IPI')

ggplotly(graficoInicial)

# [3]. Se estudia la estacionariedad en media y en varianza
#      de la serie
################################################################

install.packages("tseries")
library(tseries)

# [3.1] ¿Es estacionaria en varianza?

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

install.packages("MASS")
library(MASS)

box_cox <- boxcox(target ~ fecha,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# -> Se toma logaritmo

datos.train$log_target=log(datos.train$target)
datos.test$log_target=log(datos.test$target)
datos.train.ts <- as.ts(datos.train$log_target, frequency = 12)
datos.test.ts <- as.ts(datos.test$log_target, frequency=12)

# [3.2] ¿Es estacionaria en media? (¿Hay que diferenciarla?)

# Test de Dickey-Fuller

adf.test(datos.train.ts)

# p-value = 0.329 -> Podría ser necesaria una diferencia regular

# [4]. Ajuste de un modelo ARIMA a la serie

install.packages("forecast")
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

install.packages("lmtest")
library(lmtest)

coeftest(ajuste1)

install.packages("caschrono")
library(caschrono)

cor.arma(ajuste1)

Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

graphics.off()

acf(ajuste1$residuals, lag.max = 48, xlab = "Retardo",
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
           type="Ljung-Box")

graphics.off()

acf(ajuste2$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

pacf(ajuste2$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# Se propone un SARIMA(1,0,1)x(1,0,0)12 + MU

ajuste3 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(1,0,0), period = 12),
                 method = "ML")

coeftest(ajuste3)

cor.arma(ajuste3)

Box.test.2(residuals(ajuste3),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

graphics.off()

acf(ajuste3$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

pacf(ajuste3$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# Se propone un SARIMA(1,0,1)x(1,0,1)12 + MU

ajuste4 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(1,0,1), period = 12),
                 method = "ML")

coeftest(ajuste4)

cor.arma(ajuste4)

Box.test.2(residuals(ajuste4),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

graphics.off()

acf(ajuste4$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

pacf(ajuste4$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# Se propone un SARIMA(1,0,1)x(0,1,1)12 + MU

ajuste5 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(0,1,1), period = 12),
                 method = "ML")

coeftest(ajuste5)

# Automáticamente ha quitado la constante

ajuste5 <- Arima(datos.train.ts,
                 include.constant = TRUE,
                 order = c(1,0,1),
                 seasonal = list(order = c(0,1,1), period = 12),
                 method = "ML")

coeftest(ajuste5)

# Efectivamente, no era significativa. Está bien quitada

# Se propone un SARIMA(0,1,1)x(0,1,1)12 + MU

ajuste6 <- Arima(datos.train.ts,
                 order = c(0,1,1),
                 seasonal = list(order = c(0,1,1), period = 12),
                 method = "ML")

coeftest(ajuste6)

# Peligro de no invertibilidad 

cor.arma(ajuste6)

Box.test.2(residuals(ajuste6),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5]. Análisis de intervenciones

# Se carga y llama a la función que define los festivos

# Función para crear explicativas en series mensuales

# La variable de entrada contiene todas las fechas mensuales, asumiendo que el primer día
# de cada mes es el día 1.

# Función para crear explicativas en series mensuales

# Variables de entrada:
# --variableFecha: contiene todas las fechas mensuales, asumiendo que el primer día
# de cada mes es el día 1.
# --domingoYFestivosJuntos: booleano que vale 1 si se desea que el efecto de los festivos
# sea como el de un domingo, y que en la def. de laborables no se consideren festivos.

calculoExplicativasCalendario <- function(variableFecha, domingoYFestivosJuntos){
  
  #######################################
  #     Creación de todas las fechas    #
  #######################################
  
  # Se crean las fechas a nivel diario entre una primera y una última fecha dada.
  # Para llegar al último día del mes de la última fecha, se suman días para llegar
  # al 28/29, 30 o 31 según proceda.
  
  library(lubridate)
  
  if (month(max(variableFecha)) %in% c(1,3,5,7,8,10,12)) {
    diasHastaFinMes <- 30
  } else if (month(max(variableFecha)) %in% c(4,6,9,11)) {
    diasHastaFinMes <- 29
  } else if (year(max(variableFecha))%%4==0) {
    diasHastaFinMes <- 28
  } else {diasHastaFinMes <- 27}
  
  todasLasFechas <- data.frame(fechas=seq(min(variableFecha),
                                          max(variableFecha)+diasHastaFinMes,
                                          by="days"))
  
  #######################################
  #     Cálculo de la Semana Santa      #
  #######################################
  
  # Función Easter de timeDate
  
  # install.packages("timeDate")
  library(timeDate)
  
  domingoResurrecion <- as.Date(Easter(year(min(variableFecha)):year(max(variableFecha))))
  lunesPascua <- domingoResurrecion+1
  sabadoSanto <- domingoResurrecion-1
  viernesSanto <- domingoResurrecion-2
  juevesSanto <- domingoResurrecion-3
  
  # Se unen y ordenan todos los días que forman la Semana Santa
  semanaSanta <- sort(c(juevesSanto, viernesSanto, sabadoSanto, domingoResurrecion, lunesPascua))
  
  # Se pone en formato data.frame y se añade un indicador
  semanaSanta <- data.frame(fechas=semanaSanta, semanaSanta=rep(1,length(semanaSanta)))
  
  # Se añaden a la tabla maestra de fechas
  todasLasFechas_2 <- merge(x = todasLasFechas, y = semanaSanta, by = "fechas", all.x = TRUE)
  
  # Se reemplazan los NAs por 0, terminando de definir así el indicador de SemanaSanta
  todasLasFechas_2$semanaSanta[is.na(todasLasFechas_2$semanaSanta)] <- 0
  
  
  ######################################
  #     Cálculo de la variable dt      #
  ######################################
  
  # 1. Definición de festivos:
  ############################
  
  calendario <- todasLasFechas
  
  calendario$diaSemana <- as.factor(wday(calendario$fecha))
  calendario$diaMes <- as.factor(day(calendario$fecha))
  calendario$mes <- as.factor(month(calendario$fecha))
  calendario$anyo <- as.factor(year(calendario$fecha))
  
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
  
  calendario$festivo <- rowSums(subset(calendario, select=p_01ene:p_25dic))
  
  # La definición de la variable dt varía según la opción domingoYFestivosJuntos.
  
  if (domingoYFestivosJuntos==0){
    
    calendario$sabado <- ifelse(calendario$diaSemana==7, 1 ,0)
    calendario$domingo <- ifelse(calendario$diaSemana==1, 1 ,0)
    
    # Días laborables: todos menos sábados y domingos
    calendario$laborable <- 1-calendario$sabado-calendario$domingo
    
  } else {
    
    calendario$sabado <- ifelse(calendario$diaSemana==7, 1 ,0)
    calendario$domingo <- ifelse(calendario$diaSemana==1, 1 ,0)
    # Domingo=1 si domingo=1 o festivo=1
    calendario$domingo <- ifelse(calendario$domingo==1 | calendario$festivo==1, 1 ,0)
    
    # Días laborables: todos menos sábados y domingos/festivos
    calendario$laborable <- 1-calendario$sabado-calendario$domingo    
  }
  
  
  # 2. Definición de variable dt:
  ###############################
  
  # Se filtran las columnas de interés y se añade la Semana Santa
  
  calendario_2 <- calendario[, c("fechas", "mes", "anyo", "sabado", "domingo", "laborable", "festivo")]
  
  todasLasFechasFinal <- merge(x = todasLasFechas_2, y = calendario_2,
                               by = "fechas", all.x = TRUE)
  
  # Agregamos la serie a nivel año-mes
  
  calendarioAnyoMes <- aggregate(todasLasFechasFinal[,c("sabado","domingo",
                                                        "laborable", "semanaSanta", "festivo")],
                                 by=list(mes=todasLasFechasFinal$mes,
                                         anyo=todasLasFechasFinal$anyo),
                                 "sum")
  
  # Se calcula la variable dt:
  
  calendarioAnyoMes$dt <- calendarioAnyoMes$laborable-(5/2)*(calendarioAnyoMes$sabado+calendarioAnyoMes$domingo)
  
  
  ######################################
  #     Cálculo de años bisiestos      #
  ######################################
  
  calendarioAnyoMes$anyoNum <- as.numeric(levels(calendarioAnyoMes$anyo))[calendarioAnyoMes$anyo]
  
  calendarioAnyoMes$bisiesto <- ifelse(calendarioAnyoMes$mes==2 &(calendarioAnyoMes$anyoNum %% 4)==0, 1 ,0)
  
  
  #######################################################
  #     Tabla final con explicativas de calendario      #
  #######################################################
  
  if (domingoYFestivosJuntos==0){
    explicativasCalendario <- cbind(fecha=variableFecha, calendarioAnyoMes[, c("semanaSanta", "dt", "bisiesto", "festivo")])
  } else {
    explicativasCalendario <- cbind(fecha=variableFecha, calendarioAnyoMes[, c("semanaSanta", "dt", "bisiesto")])
  }
  
  return(explicativasCalendario)
  
}

explicativasCalendarioTrain <- calculoExplicativasCalendario(datos.train$fecha,domingoYFestivosJuntos=0)

calendarioTrain <- 
  as.matrix(
    explicativasCalendarioTrain[,c("semanaSanta", "dt", "bisiesto")]
    )

# Se podríaa incluir también la variable festivo

ajuste6conCalendario <- Arima(datos.train.ts,
                              order = c(0,1,1),
                              seasonal = list(order = c(0,1,1), 
                                              period = 12),
                              xreg = calendarioTrain,
                              method = "ML")

coeftest(ajuste6conCalendario)

# Se ha arreglado el peligro de invertibilidad

cor.arma(ajuste6conCalendario)

Box.test.2(residuals(ajuste6conCalendario),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [6]. Variables explicativas

x<-lag(datos.train$tasaParoMensual)
## [1] NA  1  2  3  4


## [1] NA NA  1  2  3

datos.train$log_tasaParoMensual=log(datos.train$tasaParoMensual)
datos.train$log_tasaParoMensual_1=log(dplyr::lag(datos.train$tasaParoMensual, 1))
datos.train$log_tasaParoMensual_2=log(dplyr::lag(datos.train$tasaParoMensual, 2))

datos.train$log_tasaParoMensual_1[is.na(datos.train$log_tasaParoMensual_1)] <- 0
datos.train$log_tasaParoMensual_2[is.na(datos.train$log_tasaParoMensual_2)] <- 0

calendarioMasTasaParoTrain <- as.matrix(cbind(calendarioTrain,datos.train[,c(5:7)]))

ajuste6conCalendarioTasaParo <- Arima(datos.train.ts,
                                      order = c(0,1,1),
                                      seasonal = list(order = c(0,1,1), period = 12),
                                      xreg = calendarioMasTasaParoTrain,
                                      method = "ML")

coeftest(ajuste6conCalendarioTasaParo)

# Se ha arreglado el peligro de invertibilidad

cor.arma(ajuste6conCalendarioTasaParo)

Box.test.2(residuals(ajuste6conCalendarioTasaParo),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [7]. Identificación de outliers

# install.packages("tsoutliers")
library(tsoutliers)

# Esto identifica outliers

listaOutliersTrain <- locate.outliers(ajuste6conCalendarioTasaParo$residuals,
                                      pars = coefs2poly(ajuste6conCalendarioTasaParo),
                                      types = c("AO", "LS", "TC"),cval=3)

listaOutliersTrain$abststat=abs(listaOutliersTrain$tstat)

# Cruzamos con la tabla original para obtener la fecha

datos.train$ind <- as.numeric(rownames(datos.train))
listaOutliersTrainFecha <- merge(listaOutliersTrain, datos.train[,c("ind", "fecha")], by = "ind")

listaOutliersTrainFecha

# install.packages("dplyr")
library(dplyr)

arrange(listaOutliersTrainFecha,desc(listaOutliersTrainFecha$abststat))

outliersTrain <- outliers(c("LS","AO"), c(67,92))
outliersVariablesTrain <- outliers.effects(outliersTrain, length(ajuste6conCalendario$residuals))
calendarioMasTasaParoMasOutliersTrain <- as.matrix(cbind(calendarioMasTasaParoTrain,outliersVariablesTrain))

ajuste6conCalendarioTasaParoYOutliers <- Arima(datos.train.ts,
                                               order = c(0,1,1),
                                               seasonal = list(order = c(0,1,1), period = 12),
                                               xreg = calendarioMasTasaParoMasOutliersTrain,
                                               method = "ML")

coeftest(ajuste6conCalendarioTasaParoYOutliers)

cor.arma(ajuste6conCalendarioTasaParoYOutliers)

Box.test.2(residuals(ajuste6conCalendarioTasaParoYOutliers),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [7]. Predicción

explicativasCalendarioTest <- calculoExplicativasCalendario(datos.test$fecha,domingoYFestivosJuntos=0)
calendarioTest <- as.matrix(explicativasCalendarioTest[,c("semanaSanta", "dt", "bisiesto")])

# Solo hemos generado una explicativas con 1's a partir del dato 54
# Nos valen los últimos 12 datos de la variable (12 1's)
# para el valor futuro de la variable

outliersVariablesTest <- outliersVariablesTrain[133:144,]

datos$log_tasaParoMensual=log(datos$tasaParoMensual)
datos$log_tasaParoMensual_1=log(dplyr::lag(datos$tasaParoMensual, 1))
datos$log_tasaParoMensual_2=log(dplyr::lag(datos$tasaParoMensual, 2))

datos$log_tasaParoMensual_1[is.na(datos$log_tasaParoMensual_1)] <- 0
datos$log_tasaParoMensual_2[is.na(datos$log_tasaParoMensual_2)] <- 0

calendarioMasTasaParoMasOutliersTest <- as.matrix(cbind(calendarioTest,datos[133:144,c(4:6)],LS54=outliersVariablesTest))

prediccion <- as.data.frame(predict(ajuste6conCalendarioTasaParoYOutliers, n.ahead=12,
                                    newxreg=calendarioMasTasaParoMasOutliersTest))

#Límites de confianza al 95%

U <- exp(prediccion$pred + 2*prediccion$se)
L <- exp(prediccion$pred - 2*prediccion$se)

datos.pred <- data.frame(fecha = datos.test$fecha, Prediccion = exp(prediccion$pred+0.5*prediccion$se^2),
                         LimSup = U, LimInf =L)

datos.real.pred <- merge(datos[,c("fecha","target")], datos.pred, by = "fecha", all.x = T)

# Gráfico 1: Real vs Predicción

grafico1 <- ggplot(data = datos.real.pred) +
  geom_line(aes(x= fecha, y = target), color = 'steelblue',
            alpha = 0.8, size = 1) +
  geom_line(aes(x= fecha, y = Prediccion), color = 'darkred',
            alpha = 0.9, linetype = 2, size = 1) + 
  xlab('FECHA') + ylab('Matriculaciones')

ggplotly(grafico1)

# Gráfico 2: Real + Predicción + límites

datos.real.pred$target[datos.real.pred$fecha>as.Date('12/01/2016',format='%m/%d/%Y')] <- NA

grafico2 <- ggplot(data = datos.real.pred) +
  geom_line(aes(x= fecha, y = target), color = 'steelblue',
            alpha = 0.8, size = 0.8) +
  geom_line(aes(x= fecha, y = Prediccion), color = 'darkred',
            size = 1)   +
  geom_line(aes(x= fecha, y = LimSup), color = 'orange',
            size = 1)  +
  geom_line(aes(x= fecha, y = LimInf), color = 'orange',
            size = 1) +
  xlab('FECHA') + ylab('Matriculaciones')

ggplotly(grafico2)

# [8]. Ajuste automático

datos.train.ts <- ts(datos.train$log_target, frequency = 12)

ajusteAutomatico <- auto.arima(datos.train.ts,
                               max.d=1, max.D=1,
                               max.p=2, max.P=2,
                               max.q=2, max.Q=2, 
                               seasonal=TRUE,
                               ic="aic",
                               allowdrift=FALSE,
                               xreg=calendarioMasTasaParoMasOutliersTrain,
                               stepwise=TRUE)

coeftest(ajusteAutomatico)

cor.arma(ajusteAutomatico)

Box.test.2(residuals(ajusteAutomatico),
nlag = c(6,12,18,24,30,36,42,48),
type="Ljung-Box")
