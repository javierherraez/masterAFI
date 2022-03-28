#######################
#######################
#######################
# Ajuste masivo (TOP) #
#######################
#######################
#######################

datosConsumo<-data.frame(read.csv("Z:/MDS_F/Series Temporales/seriesTemporalesAjusteMasivo/seriesTemporalesAjusteMasivo/data/datosConsumoHorario.csv"))
datosTemperatura<-data.frame(read.csv("Z:/MDS_F/Series Temporales/seriesTemporalesAjusteMasivo/seriesTemporalesAjusteMasivo/data/datosTemperatura.csv"))

# Horizonte al que se har? la predicci?n (tabla de test)

horizonteDiario<-31

# Se adjuntan los datos de temperatura

tablaEntrada<-merge.data.frame(datosConsumo,datosTemperatura,by=c("ID_Provincia","Fecha"))

# Se transforma la fecha de car?cter a fecha

tablaEntrada$Fecha<-as.Date(tablaEntrada$Fecha,"%d/%m/%Y")
datosTemperatura$Fecha<-as.Date(datosTemperatura$Fecha,"%d/%m/%Y")

##########################################################
# Predicci?n a nivel TOP de la demanda diaria            #
# Agregamos el consumo horario y diario a nivel fecha    #
##########################################################

# Por fecha y CUPS, todas las horas 

tablaHoraria <- tablaEntrada[,2:27]

# Por fecha todas las horas

tablaHoraria <- tablaHoraria[,c(-2)]

# Se inicializa la estructura en la que se guardar? la serie TOP

serieTOP <- data.frame(matrix(nrow=length(unique(tablaHoraria$Fecha)),ncol=0))

for (hora in 2:25){
  
  tablaAux <- tablaHoraria[,c(1,hora)]
  Fecha <- unique(tablaAux$Fecha)
  sumaConsumos <- aggregate(list(consumo = tablaAux[,2]),
                            by = list(Fecha = tablaAux$Fecha),
                            FUN = sum)
  
  Fecha <- sumaConsumos[1]
  consumoHorarioAgregado <- sumaConsumos[2]
  names(consumoHorarioAgregado) <- paste("hora",hora-1,sep="")
  
  if(hora==2){serieTOP <- cbind.data.frame(Fecha,consumoHorarioAgregado)}
  else{serieTOP <- cbind(serieTOP,consumoHorarioAgregado)}
  
}

serieTOP$consumoDiario <- rowSums(serieTOP[2:25])

# Representaci?n gr?fica de la serie

# install.packages("ggplot2")
library(ggplot2)

ggplot(aes(x= Fecha, y = consumoDiario), data = serieTOP) + geom_line(color = '#d84519') + 
  xlab('Fecha') + ylab('Demanda de gas a nivel agregado') + scale_x_date(date_breaks = '6 months', date_labels = "%m/%y")

# Definici?n del calendario

# install.packages("lubridate")
library(lubridate)

calendario<-serieTOP[c("Fecha")]
calendario$diaSemana <- as.factor(wday(calendario$Fecha))
calendario$diaMes <- as.factor(day(calendario$Fecha))
calendario$mes <- as.factor(month(calendario$Fecha))
calendario$anyo <- as.factor(year(calendario$Fecha))

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
calendario$escalonJulio <- ifelse(calendario$mes==7, 1, 0)
calendario$festivo <- calendario$p_01ene+calendario$p_06ene+calendario$p_19mar+calendario$p_01may+calendario$p_15ago+
  calendario$p_12oct+calendario$p_01nov+calendario$p_06dic+calendario$p_08dic+calendario$p_25dic  

# Se contruye tambi?n la serie del calendario para los modelos de desagregaci?n horaria

calendarioGLM <- calendario[c("diaSemana","mes","festivo")]

# Nos quedamos solo con los regresores que entrar?n a los ARIMA

calendario<-calendario[c(6:16)]

# Creamos un concepto de temperatura a nivel nacional a trav?s de la media

TMax <- aggregate(list(TempMaxNac = datosTemperatura$Temperatura_max),
                  by = list(Fecha = datosTemperatura$Fecha),
                  FUN = mean)

TMin <- aggregate(list(TempMinNac = datosTemperatura$Temperatura_min),
                  by = list(Fecha = datosTemperatura$Fecha),
                  FUN = mean)

temperaturasNac <- merge(TMax, TMin, by = "Fecha")

# Se amplian los regresores con las nuevas variables explicativas

calendario <- cbind(calendario, temperaturasNac[,c(-1)])

# Se hace lo mismo para la tabla que servir? de entrada a los modelos de desagregaci?n horaria

calendarioGLM <- cbind(calendarioGLM, temperaturasNac[,c(-1)])

# Separamos los conjuntos de train y test

serieTOP.train <- subset(serieTOP, Fecha<=max(Fecha)-horizonteDiario)
serieTOP.test <- subset(serieTOP, Fecha>max(serieTOP.train$Fecha))

calendario.train <- calendario[1:nrow(serieTOP.train),]
calendario.test <- calendario[(nrow(serieTOP.train)+1):(nrow(serieTOP.train)+horizonteDiario),]

# Estacionariedad en varianza de la serie

#install.packages("MASS")
library(MASS)

box_cox <- boxcox(consumoDiario ~ Fecha,
                  data = serieTOP.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# Hay que hacer una transformaci?n logar?tmica

serieTOP.train$log_consumoDiario <- log(serieTOP.train$consumoDiario)
serieTOP.test$log_consumoDiario <- log(serieTOP.test$consumoDiario)

# Ajuste autom?tico de modelo ARMA
# Convertimos el conjunto en un objeto ts y aplicamos el auto.arima

serieTOP.train.ts <- ts(serieTOP.train$log_consumoDiario, frequency = 7)

# install.packages("forecast")
library(forecast)

ajusteTOP <- auto.arima(serieTOP.train.ts,
                        max.d=1, max.D=1,
                        max.p=2, max.P=2,
                        max.q=2, max.Q=2, 
                        seasonal=TRUE,
                        ic="aic",
                        allowdrift=FALSE,
                        xreg=as.matrix(calendario.train),
                        stepwise=TRUE)

# install.packages("lmtest")
library(lmtest)

ajusteTOP
coeftest(ajusteTOP)

# Observaci?n: No todos los par?metros son significativos

# Correlaciones entre par?metros

# install.packages("caschrono")
library(caschrono)

cor.arma(ajusteTOP)

# M?xima correlaci?n: -0.7196219580

# Test de ruido blanco

Box.test.2(residuals(ajusteTOP),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# No hay RB

acf(ajusteTOP$residuals, lag.max = 25, xlab = "Retardo", main="")
pacf(ajusteTOP$residuals, lag.max = 25, xlab = "Retardo", main="")

# Pintamos las predicciones diarias y calculamos los errores sobre test
# Obtenemos predicciones sobre test

prediccionDiaria <- predict(ajusteTOP,
                            n.ahead = horizonteDiario,
                            newxreg = calendario.test)

# Deshacemos el logaritmo

prediccionDiaria$pred <- exp(prediccionDiaria$pred+0.5*prediccionDiaria$se**2)

# Calculamos el error de predicci?n sobre test:

realYPredDiaria <- data.frame(Fecha = serieTOP.test$Fecha,
                              Real = serieTOP.test$consumoDiario,
                              Prediccion = prediccionDiaria$pred)

realYPredDiaria$MAPE <- abs(100*(realYPredDiaria$Real-realYPredDiaria$Prediccion)/realYPredDiaria$Prediccion)

# Pasamos a ts la serie original, ya que la que hay guardada
# tiene el logaritmo

serieTOP.Orig.ts <- ts(serieTOP$consumoDiario, frequency = 7)

# Pintamos el dato real y la predicci?n sobre test

ts.plot(serieTOP.Orig.ts, prediccionDiaria$pred, col=c(1,2))

####################
# Modelos horarios #
####################

# Calculamos el % de consumo horario

# Inicializamos la tabla que contendr? los consumos horarios

consumoHorario <- matrix(nrow = nrow(serieTOP),
                         ncol = 24)

for (hora in 1:24)
{
  consumoHorario[,hora] <- serieTOP[,hora+1]/serieTOP$consumoDiario
}

consumoHorario<-as.data.frame(consumoHorario)
names(consumoHorario) <- paste("pcthora",1:24,sep="")

# Se adjuntan las variables explicativas

consumoHorarioCalendario <- cbind.data.frame(consumoHorario,calendarioGLM)

# Si todos los consumos son 0 se generan NA's (0/0)
# Se reemplazan por 1/24

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

consumoHorarioCalendario[is.nan(consumoHorarioCalendario)] <- 1/24

# Ajustamos GLM's

models <- list()
prediccionPctHorarios<-data.frame(matrix(nrow = 943, ncol = 0))

targets <- paste("pcthora", 1:24, sep='')

for (recorreTargets in targets){
  form <- formula(paste(recorreTargets, "~", "diaSemana+mes+TempMaxNac+TempMinNac+festivo"))
  models[[recorreTargets]] <- glm(form, data=consumoHorarioCalendario)
  print(models[[recorreTargets]])
  prediccionHorariaAux<-as.data.frame(predict(models[[recorreTargets]],consumoHorarioCalendario))
  names(prediccionHorariaAux)=paste("pred",recorreTargets,sep="")
  prediccionPctHorarios<-cbind(prediccionPctHorarios,prediccionHorariaAux)
}

# Comprobaci?n de que los %'s suman 1
# rowSums(prediccionPctHorarios[1:24])

# Predicci?n de consumo a nivel horario:

# Unimos las predicciones de train y de test en una ?nica

prediccionDiariaTrain <- as.data.frame(exp(ajusteTOP$fitted))
names(prediccionDiariaTrain) <- "prediccionDiaria"
prediccionDiariaTrain$prediccionDiaria <- as.numeric(prediccionDiariaTrain$prediccionDiaria)
prediccionDiariaTest <- as.data.frame(predict(ajusteTOP, n.ahead = horizonteDiario, newxreg = calendario.test)$pred)
names(prediccionDiariaTest) <- "prediccionDiaria"
prediccionDiariaTest$prediccionDiaria <- exp(prediccionDiariaTest$prediccionDiaria)
prediccionDiariaTest$prediccionDiaria <- as.numeric(prediccionDiariaTest$prediccionDiaria)
prediccionDiariaFinal <- rbind(prediccionDiariaTrain, prediccionDiariaTest)
prediccionDiariaFinal <- data.frame(Fecha=serieTOP$Fecha,prediccionDiariaFinal)

# Unimos la predicci?n diaria con la horaria:

paraPrediccionHoraria <- cbind(prediccionDiariaFinal,prediccionPctHorarios)

# Multiplicamos las predicciones diarias por las predicciones de los
# %'s de consumo horario:

multiplicaPctPred <- function(serieHora){
  predDiaria <- paraPrediccionHoraria[,2]
  producto <- predDiaria*serieHora
}

prediccionHorariaFinal <- as.data.frame(apply(paraPrediccionHoraria[,3:26],2,multiplicaPctPred))
names(prediccionHorariaFinal) <- paste("PrediccionHora",1:24,sep="")
prediccionHorariaFinal <- data.frame(Fecha=paraPrediccionHoraria$Fecha,prediccionHorariaFinal)

# Gr?fico de las curvas de consumo en laborables y festivos

# Ajustamos GLM's solo con el d?a de la semana y mes (_dsm)

models <- list()
prediccionPctHorarios_dsm <- data.frame(matrix(nrow = 943, ncol = 0))

targets <- paste("pcthora", 1:24, sep='')

for (recorreTargets in targets){
  form <- formula(paste(recorreTargets, "~", "diaSemana+mes"))
  models[[recorreTargets]] <- glm(form, data=consumoHorarioCalendario)
  print(models[[recorreTargets]])
  prediccionHorariaAux<-as.data.frame(predict(models[[recorreTargets]],consumoHorarioCalendario))
  names(prediccionHorariaAux)=paste("pred",recorreTargets,sep="")
  prediccionPctHorarios_dsm<-cbind(prediccionPctHorarios_dsm,prediccionHorariaAux)
}

prediccionPctHorarios_dsm_2<-cbind(serieTOP[c("Fecha")],prediccionPctHorarios_dsm)

# Trasponemos los datos para pintar las curvas diarias

# install.packages("dplyr")
library(dplyr)
# install.packages("magrittr")
library(magrittr)

prediccionPctHorarios_dsm_T <- prediccionPctHorarios_dsm_2 %>% 
  gather("predpcthora","consumo", -Fecha)

prediccionPctHorarios_dsm_T <- prediccionPctHorarios_dsm_T[order(prediccionPctHorarios_dsm_T[c("Fecha")]),]

prediccionPctHorarios_dsm_T$diaSemana <- wday(prediccionPctHorarios_dsm_T$Fecha)
prediccionPctHorarios_dsm_T$mes <- month(prediccionPctHorarios_dsm_T$Fecha)

# Hacemos el gr?fico de las curvas de consumo para cada dia

# Domingos de Enero #

curvaDiaria <- subset(prediccionPctHorarios_dsm_T,prediccionPctHorarios_dsm_T$diaSemana==1 & prediccionPctHorarios_dsm_T$mes==1)
curvaDiaria <- curvaDiaria[1:24,]
num_hora <- as.data.frame(matrix(1:24,byrow = T,ncol=1))
curvaDiaria <- cbind(curvaDiaria,num_hora)
names(curvaDiaria)[3] <- "porcentaje"
names(curvaDiaria)[6] <- "hora"

grafico <- ggplot(aes(x= hora, y = porcentaje), data = curvaDiaria) +
  geom_line(color = '#d84519', size = 1.5, alpha = 0.7) +
  xlab('Hora del dia') +
  ylab('Pct consumo horario') + 
  ggtitle(paste("Curva horaria tipo dia =", 1, ", mes =", 1))

ggplotly(grafico)

# Domingos de Febrero #

curvaDiaria <- subset(prediccionPctHorarios_dsm_T,prediccionPctHorarios_dsm_T$diaSemana==1 & prediccionPctHorarios_dsm_T$mes==2)
curvaDiaria <- curvaDiaria[1:24,]
num_hora <- as.data.frame(matrix(1:24,byrow = T,ncol=1))
curvaDiaria <- cbind(curvaDiaria,num_hora)
names(curvaDiaria)[3] <- "porcentaje"
names(curvaDiaria)[6] <- "hora"

grafico <- ggplot(aes(x= hora, y = porcentaje), data = curvaDiaria) +
  geom_line(color = '#d84519', size = 1.5, alpha = 0.7) +
  xlab('Hora del dia') +
  ylab('Pct consumo horario') + 
  ggtitle(paste("Curva horaria tipo dia =", 1, ", mes =", 2))

ggplotly(grafico)

# Lunes de Febrero #

curvaDiaria <- subset(prediccionPctHorarios_dsm_T,prediccionPctHorarios_dsm_T$diaSemana==2 & prediccionPctHorarios_dsm_T$mes==2)
curvaDiaria <- curvaDiaria[1:24,]
num_hora <- as.data.frame(matrix(1:24,byrow = T,ncol=1))
curvaDiaria <- cbind(curvaDiaria,num_hora)
names(curvaDiaria)[3] <- "porcentaje"
names(curvaDiaria)[6] <- "hora"

grafico <- ggplot(aes(x= hora, y = porcentaje), data = curvaDiaria) +
  geom_line(color = '#d84519', size = 1.5, alpha = 0.7) +
  xlab('Hora del dia') +
  ylab('Pct consumo horario') + 
  ggtitle(paste("Curva horaria tipo dia =", 2, ", mes =", 2))

ggplotly(grafico)

graphics.off()

for(dia in 1:7){
  
  for(mes in 1:12){
    
    curvaDiaria <- subset(prediccionPctHorarios_dsm_T,prediccionPctHorarios_dsm_T$diaSemana == dia && prediccionPctHorarios_dsm_T$mes == mes)
    curvaDiaria <- curvaDiaria[1:24,]
    num_hora <- as.data.frame(matrix(1:24,byrow = T,ncol=1))
    curvaDiaria <- cbind(curvaDiaria,num_hora)
    names(curvaDiaria)[3] <- "porcentaje"
    names(curvaDiaria)[6] <- "hora"
    
    grafico <- ggplot(aes(x= hora, y = porcentaje), data = curvaDiaria) +
      geom_line(color = '#d84519', size = 1.5, alpha = 0.7) +
      xlab('Hora del dia') +
      ylab('Pct consumo horario') + 
      ggtitle(paste("Curva horaria tipo dia =", dia, ", mes =", mes))
    
    ggplotly(grafico)
    
  }
}
