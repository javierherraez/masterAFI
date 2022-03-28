#############################
#############################
#############################
# Ajuste masivo (BOTTOM-UP) #
#############################
#############################
#############################

datosConsumo<-data.frame(read.csv("C:/Users/daniel/Nube/GoogleDriveUniversidad/escuelasDeNegocio/AFI/cursos_20212022/Regular/seriesTemporalesAjusteMasivo/datosConsumoHorario.csv"))
datosTemperatura<-data.frame(read.csv("C:/Users/daniel/Nube/GoogleDriveUniversidad/escuelasDeNegocio/AFI/cursos_20212022/Regular/seriesTemporalesAjusteMasivo/datosTemperatura.csv"))

# Número de cUPS a los que se ajustarán series (para hacer una prueba)

numeroSeries<- 3
horizonteDiario<-31

ID_CUPS_distintos<-head((unique(datosConsumo$ID_CUPS)),numeroSeries)
ID_CUPS_distintos.df<-as.data.frame(ID_CUPS_distintos)
names(ID_CUPS_distintos.df)="ID_CUPS"

datosConsumo<-merge.data.frame(datosConsumo,ID_CUPS_distintos.df,by=c("ID_CUPS"))

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("forecast")
# install.packages("car")
# install.packages("lubridate")

library(dplyr)
library(tidyr)
library(forecast)
library(car)
library(lubridate)

# Se adjuntan los datos de temperatura

tablaEntrada<-merge.data.frame(datosConsumo,datosTemperatura,by=c("ID_Provincia","Fecha"))
colnames(datosConsumo)
colnames(datosTemperatura)

# Se transforma la fecha de carácter a fecha

tablaEntrada$Fecha<-as.Date(tablaEntrada$Fecha,"%d/%m/%Y")

# Generamos la columna con el consumo diario

tablaEntrada$Consumo<-rowSums(tablaEntrada[,4:27])

# Seleccionamos las columnas de interés

consumos_T<-arrange(spread(tablaEntrada[c("ID_CUPS","Fecha","Consumo")],ID_CUPS,Consumo),Fecha)
consumos_T$ID_Dato="Cons"
dim(consumos_T)

################################################################################
# Se prepara el set de datos para poder ajustar un ARIMA a cada columna (CUPS) #
# del dataset a través de la apliación de la función auto.arima usando la      #
# la función APPLY. Para ello, pondremos debajo de los datos de consumo de cada#
# CUPS, los datos de temperatura de la provincia a la que pertenece            #
# Se asume que el calendario es común (solo festivos nacionales), aunque       #
# para hacerlo perfectamente se deberían poner también los de la comunidad     #
################################################################################

Temperatura_max_T<-arrange(spread(tablaEntrada[c("ID_CUPS","Fecha","Temperatura_max")],ID_CUPS,Temperatura_max),Fecha)
Temperatura_max_T$ID_Dato="TMax"
dim(consumos_T)
Temperatura_min_T<-arrange(spread(tablaEntrada[c("ID_CUPS","Fecha","Temperatura_min")],ID_CUPS,Temperatura_min),Fecha)
Temperatura_min_T$ID_Dato="TMin"
dim(consumos_T)

# Definición de variable festivo 

calendario<-consumos_T[c("Fecha")]
calendario$diaSemana <- wday(calendario$Fecha)
calendario$diaMes <- day(calendario$Fecha)
calendario$mes <- month(calendario$Fecha)
calendario$anyo <- year(calendario$Fecha)

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
calendario$festivo <- calendario$p_01ene+calendario$p_06ene+calendario$p_19mar+calendario$p_01may+calendario$p_15ago+
                      calendario$p_12oct+calendario$p_01nov+calendario$p_06dic+calendario$p_08dic+calendario$p_25dic  

#calendario<-calendario[c(6:15)]
calendario<-calendario[c(6:15)]

####################
####################
####################
# Modelos diarios  #
####################
####################
####################

tablaParaARIMA<-rbind(consumos_T,Temperatura_max_T,Temperatura_min_T)

ajustaArima<-function(serie,horizonte=horizonteDiario,tablaCalendario=calendario)
  {
  
  ##############################################################
  # Se prepara para cada CUPS el datset que servirá de entrada #
  # para ajustar el ARIMA: una columna para el consumo y luego #
  # las columnas asociadas a las explicativas: TMAX, TMIN y    #
  # una variable asociada a cada una de las festividades       #
  ##############################################################
  
  serie.df<-as.data.frame(serie)
  print("##################")
  print(c("# CUPS:",colnames(serie.df)," #"))
  print("##################")
  
  longitudSerie=nrow(serie.df)/3
  longitudSerieMenosHorizonte<-longitudSerie-horizonte-1
  
  serieConsumo<-serie.df[1:longitudSerieMenosHorizonte,]
  inicioTmaxTrain<-longitudSerie+1
  finTmaxTrain<-inicioTmaxTrain+longitudSerieMenosHorizonte-1
  serieTmaxTrain<-serie.df[inicioTmaxTrain:finTmaxTrain,]
  serieTmaxTrain2<-serie.df[inicioTmaxTrain:finTmaxTrain,]**2
  inicioTminTrain<-longitudSerie*2+1
  finTminTrain<-inicioTminTrain+longitudSerieMenosHorizonte-1
  serieTminTrain<-serie.df[inicioTminTrain:finTminTrain,]
  serieTminTrain2<-serie.df[inicioTminTrain:finTminTrain,]**2

  calendarioTrain<-as.matrix(tablaCalendario[1:(longitudSerieMenosHorizonte),])
  
  inputsTrain<-cbind(serieTmaxTrain,serieTminTrain,serieTmaxTrain2,serieTminTrain2,calendarioTrain)
  
  inicioTmaxTest<-finTmaxTrain+1
  finTmaxTest<-finTmaxTrain+horizonte
  serieTmaxTest<-serie.df[inicioTmaxTest:finTmaxTest,]
  serieTmaxTest2<-serie.df[inicioTmaxTest:finTmaxTest,]**2
  inicioTminTest<-finTminTrain+1
  finTminTest<-inicioTminTest+horizonte-1
  serieTminTest<-serie.df[inicioTminTest:finTminTest,]
  serieTminTest2<-serie.df[inicioTminTest:finTminTest,]**2
  
  inicioTest<-longitudSerieMenosHorizonte+1
  
  calendarioTest<-as.matrix(tablaCalendario[inicioTest:(longitudSerie-1),])
  
  inputsTest<-cbind(serieTmaxTest,serieTminTest,serieTmaxTest2,serieTminTest2,calendarioTest)
  
  # Para decidir la transformación, la serie no debe tener 0's
  # porque se aplican logaritmos
  
  series_aux<-serieConsumo+1
  
  # Se busca la mejor transformación entre none, sqrt y log
  
  box_cox<-powerTransform(series_aux)
  
  if (box_cox$lambda>0.75)
  {
    serieConsumoTransformada<-ts(series_aux,frequency=7)
  }
  else if (box_cox$lambda>0.25)
  {
    serieConsumoTransformada<-ts(sqrt(series_aux),frequency=7)
  }
  else
  {
    serieConsumoTransformada<-ts(log(series_aux),frequency=7)
  }

  # Se busca el mejor ARIMA de acuerdo al AIC

  arima<-auto.arima(serieConsumoTransformada,max.d=1, max.D=1, max.p=2, max.q=2, max.P=2, max.Q=2, 
                    seasonal=TRUE,ic="aic",allowdrift=FALSE,xreg=inputsTrain)
  #,stepwise=TRUE) # Para que vaya rápido en clase
  
  print(arima)
  
  # Se aplica el ARIMA para hacer una predicción
  
  prediccion<-predict(arima,n.ahead=horizonte,newxreg=inputsTest)
  
  if (box_cox$lambda>0.75)
  {
    prediccion$pred<-prediccion$pred-1
  }
  else if (box_cox$lambda>0.25)
  {
    prediccion$pred<-prediccion$pred**2+prediccion$se**2-1
  }
  else
  {
    prediccion$pred<-exp(prediccion$pred+0.5*prediccion$se**2)-1
  } 
  
}

# Se aplica la función definida "ajustaArima" por columnas */

tablaParaAjustaARIMA<-tablaParaARIMA[,2:4]
prediccionDiaria<-as.data.frame(apply(tablaParaAjustaARIMA,2,ajustaArima))

prediccionDiaria_T<-gather(prediccionDiaria)

fechasFuturo<-unique(tablaEntrada$Fecha)
fechasFuturo<-as.data.frame(sort(fechasFuturo))
fechasFuturo<-fechasFuturo[(nrow(fechasFuturo)-30):nrow(fechasFuturo),]
fechasFuturo<-expand.grid(fechasFuturo,1:nrow(ID_CUPS_distintos.df))

prediccionDiaria_T<-cbind(fechasFuturo[,1],prediccionDiaria_T)
names(prediccionDiaria_T)<-c("Fecha","ID_CUPS","PrediccionDiaria")

# Se adjunta el dato real

comparativaDiariaFuturo<-merge(tablaEntrada,prediccionDiaria_T,c("Fecha","ID_CUPS"))

####################
# Modelos horarios #
####################

# Calculamos el % de consumo horario

consumoHorario <- matrix(nrow = nrow(tablaEntrada),
                            ncol = 24)

for (hora in 1:24)
{
  consumoHorario[,hora] <- tablaEntrada[,hora+3]/tablaEntrada$Consumo
}

consumoHorario<-as.data.frame(consumoHorario)
names(consumoHorario) <- paste("pcthora",names(consumoHorario),sep="")

consumoHorarioFinal <- cbind.data.frame(tablaEntrada,consumoHorario)

# Si todos los consumos son 0 se generan NA's (0/0)
# Se reemplazan por 1/24

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

consumoHorarioFinal[is.nan(consumoHorarioFinal)] <- 1/24

# Definición de festivos

consumoHorarioFinal$diaSemana <- as.factor(wday(consumoHorarioFinal$Fecha))
consumoHorarioFinal$diaMes <- as.factor(day(consumoHorarioFinal$Fecha))
consumoHorarioFinal$mes <- as.factor(month(consumoHorarioFinal$Fecha))
consumoHorarioFinal$anyo <- as.factor(year(consumoHorarioFinal$Fecha))

consumoHorarioFinal$p_01ene <- ifelse(consumoHorarioFinal$diaMes==1 & consumoHorarioFinal$mes==1, 1, 0)
consumoHorarioFinal$p_06ene <- ifelse(consumoHorarioFinal$diaMes==6 & consumoHorarioFinal$mes==1, 1, 0)
consumoHorarioFinal$p_19mar <- ifelse(consumoHorarioFinal$diaMes==19 & consumoHorarioFinal$mes==3, 1, 0)
consumoHorarioFinal$p_01may <- ifelse(consumoHorarioFinal$diaMes==1 & consumoHorarioFinal$mes==5, 1, 0)
consumoHorarioFinal$p_15ago <- ifelse(consumoHorarioFinal$diaMes==15 & consumoHorarioFinal$mes==8, 1, 0)
consumoHorarioFinal$p_12oct <- ifelse(consumoHorarioFinal$diaMes==12 & consumoHorarioFinal$mes==10,1, 0)
consumoHorarioFinal$p_01nov <- ifelse(consumoHorarioFinal$diaMes==1 & consumoHorarioFinal$mes==11, 1 ,0)
consumoHorarioFinal$p_06dic <- ifelse(consumoHorarioFinal$diaMes==6 & consumoHorarioFinal$mes==12, 1 ,0)
consumoHorarioFinal$p_08dic <- ifelse(consumoHorarioFinal$diaMes==8 & consumoHorarioFinal$mes==12, 1 ,0)
consumoHorarioFinal$p_25dic <- ifelse(consumoHorarioFinal$diaMes==25 & consumoHorarioFinal$mes==12, 1 ,0)
consumoHorarioFinal$festivo <- consumoHorarioFinal$p_01ene+consumoHorarioFinal$p_06ene+consumoHorarioFinal$p_19mar+consumoHorarioFinal$p_01may+consumoHorarioFinal$p_15ago+
                               consumoHorarioFinal$p_12oct+consumoHorarioFinal$p_01nov+consumoHorarioFinal$p_06dic+consumoHorarioFinal$p_08dic+consumoHorarioFinal$p_25dic  

# Se separa el conjunto de train y el de test

consumoHorarioFinal<-arrange(consumoHorarioFinal,Fecha)

filasTrain<-nrow(consumoHorarioFinal)-(horizonteDiario)*nrow(ID_CUPS_distintos.df)
consumoHorarioFinalTrain <- consumoHorarioFinal[1:filasTrain,]
consumoHorarioFinalTest <- consumoHorarioFinal[(filasTrain+1):(filasTrain+nrow(ID_CUPS_distintos.df)*horizonteDiario),]

# Ajustamos GLM's

models <- list()
prediccionHorariaFinal<-data.frame()

targets <- paste("pcthoraV", 1:24, sep='')

for (recorre_ID_CUPS in ID_CUPS_distintos)
{
  cupsAuxTrain<-subset(consumoHorarioFinalTrain,ID_CUPS==recorre_ID_CUPS)
  cupsAuxTest<-subset(consumoHorarioFinalTest,ID_CUPS==recorre_ID_CUPS)
  prediccionHorariaCUPS<-cupsAuxTest[,2:3]
  for (recorreTargets in targets){
    form <- formula(paste(recorreTargets, "~", "diaSemana+mes+Temperatura_max+Temperatura_min+festivo"))
    models[[recorreTargets]] <- glm(form, data=cupsAuxTrain)
    print(models[[recorreTargets]])
    prediccionHorariaCUPSAux<-as.data.frame(predict(models[[recorreTargets]],cupsAuxTest))
    names(prediccionHorariaCUPSAux)=paste("pred",recorreTargets,sep="")
    prediccionHorariaCUPS<-cbind(prediccionHorariaCUPS,prediccionHorariaCUPSAux)
  }
  prediccionHorariaFinal<-rbind(prediccionHorariaFinal,prediccionHorariaCUPS)
}

prediccionHorariaFinal$comprobacion<-rowSums(prediccionHorariaFinal[,3:26])

# Se adjunta la predicción del consumo diario para desagregarlo

comparativaHorariaFuturo<-merge(comparativaDiariaFuturo,prediccionHorariaFinal,c("Fecha","ID_CUPS"))
