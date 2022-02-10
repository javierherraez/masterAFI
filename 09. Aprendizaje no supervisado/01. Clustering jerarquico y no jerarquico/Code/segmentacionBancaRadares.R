# limpiar espacio de trabajo
rm(list=ls())

# directorio de trabajo
setwd("Z:/MDS_F/Aprendizaje no supervisado/Data")

datosBanca <- read.csv("datosBanca.csv", header = TRUE, sep=",")
summary(datosBanca)

# Estandarizaci?n mediante discretizaci?n

# install.packages("nima")
library(nima)

datosBanca$checkingAccount_CAT<-discrete_by_quantile(datosBanca$checkingAccount)/4
datosBanca$deposit_CAT<-discrete_by_quantile(datosBanca$deposit)/4
datosBanca$shareOfStock_CAT<-discrete_by_quantile(datosBanca$shareOfStock)/4
datosBanca$pensionPlan_CAT<-discrete_by_quantile(datosBanca$pensionPlan)/4

datosBanca$mortgage_CAT<-discrete_by_quantile(datosBanca$mortgage)/4
# Como da un error, la asignamos con IF
summary(datosBanca$mortgage)

datosBanca$mortgage_CAT<-datosBanca$mortgage
datosBanca$mortgage_CAT <- ifelse(datosBanca$mortgage <= 0, 1, datosBanca$mortgage_CAT)
datosBanca$mortgage_CAT <- ifelse(0<datosBanca$mortgage & datosBanca$mortgage<= 44752, 2, datosBanca$mortgage_CAT)
datosBanca$mortgage_CAT <- ifelse(44752<datosBanca$mortgage & datosBanca$mortgage<= 125483, 3, datosBanca$mortgage_CAT)
datosBanca$mortgage_CAT <- ifelse(125483<datosBanca$mortgage, 4, datosBanca$mortgage_CAT)
datosBanca$mortgage_CAT <- datosBanca$mortgage_CAT/4
summary(datosBanca$mortgage_CAT)

datosBanca$loan_CAT<-discrete_by_quantile(datosBanca$loan)/4
datosBanca$cards_CAT<-discrete_by_quantile(datosBanca$cards)/4
datosBanca$insurance_CAT<-discrete_by_quantile(datosBanca$insurance)/4
datosBanca$billPayment_CAT<-discrete_by_quantile(datosBanca$billPayment)/4

# La domiciliaci?n de n?mina es binaria y no es preciso estandarizarla

datosBanca$salary_CAT<-as.numeric(datosBanca$salary)
  
summary(datosBanca)

# Cambiar missings por 0 #

datosBanca$checkingAccount_CAT[is.na(datosBanca$checkingAccount_CAT)]<-0
datosBanca$deposit_CAT[is.na(datosBanca$deposit_CAT)]<-0
datosBanca$shareOfStock_CAT[is.na(datosBanca$shareOfStock_CAT)]<-0
datosBanca$pensionPlan_CAT[is.na(datosBanca$pensionPlan_CAT)]<-0
datosBanca$mortgage_CAT[is.na(datosBanca$mortgage_CAT)]<-0
datosBanca$loan_CAT[is.na(datosBanca$loan_CAT)]<-0
datosBanca$cards_CAT[is.na(datosBanca$cards_CAT)]<-0
datosBanca$insurance_CAT[is.na(datosBanca$insurance_CAT)]<-0
datosBanca$billPayment_CAT[is.na(datosBanca$billPayment_CAT)]<-0

help(cor)
cor(datosBanca[,12:21],method = c("spearman"))
# install.packages("sqldf")
library(sqldf)

centroideTotalCartera <-  sqldf("Select 
                        avg(checkingAccount_CAT) as checkingAccount_CAT,
                        avg(deposit_CAT) as deposit_CAT,
                        avg(shareOfStock_CAT) as shareOfStock_CAT,
                        avg(pensionPlan_CAT) as pensionPlan_CAT,
                        avg(mortgage_CAT) as mortgage_CAT,
                        avg(loan_CAT) as loan_CAT,
                        avg(cards_CAT) as cards_CAT,
                        avg(insurance_CAT) as insurance_CAT,
                        avg(billPayment_CAT) as billPayment_CAT,
                        avg(salary_CAT) as salary_CAT
                        from datosBanca")

clientesNominados <- subset(datosBanca,datosBanca$salary==1)

centroideNominados <-  sqldf("Select 
                        avg(checkingAccount_CAT) as checkingAccount_CAT,
                        avg(deposit_CAT) as deposit_CAT,
                        avg(shareOfStock_CAT) as shareOfStock_CAT,
                        avg(pensionPlan_CAT) as pensionPlan_CAT,
                        avg(mortgage_CAT) as mortgage_CAT,
                        avg(loan_CAT) as loan_CAT,
                        avg(cards_CAT) as cards_CAT,
                        avg(insurance_CAT) as insurance_CAT,
                        avg(billPayment_CAT) as billPayment_CAT,
                        avg(salary_CAT) as salary_CAT
                        from clientesNominados")

clientesHipotecados <- subset(datosBanca,datosBanca$mortgage_CAT>0)

centroideHipotecados <-  sqldf("Select 
                        avg(checkingAccount_CAT) as checkingAccount_CAT,
                        avg(deposit_CAT) as deposit_CAT,
                        avg(shareOfStock_CAT) as shareOfStock_CAT,
                        avg(pensionPlan_CAT) as pensionPlan_CAT,
                        avg(mortgage_CAT) as mortgage_CAT,
                        avg(loan_CAT) as loan_CAT,
                        avg(cards_CAT) as cards_CAT,
                        avg(insurance_CAT) as insurance_CAT,
                        avg(billPayment_CAT) as billPayment_CAT,
                        avg(salary_CAT) as salary_CAT
                        from clientesHipotecados")

clientesInversores <- subset(datosBanca,datosBanca$shareOfStock_CAT>0)

centroideInversores <-  sqldf("Select 
                        avg(checkingAccount_CAT) as checkingAccount_CAT,
                        avg(deposit_CAT) as deposit_CAT,
                        avg(shareOfStock_CAT) as shareOfStock_CAT,
                        avg(pensionPlan_CAT) as pensionPlan_CAT,
                        avg(mortgage_CAT) as mortgage_CAT,
                        avg(loan_CAT) as loan_CAT,
                        avg(cards_CAT) as cards_CAT,
                        avg(insurance_CAT) as insurance_CAT,
                        avg(billPayment_CAT) as billPayment_CAT,
                        avg(salary_CAT) as salary_CAT
                        from clientesInversores")

centroides<-rbind(centroideTotalCartera,centroideNominados,centroideHipotecados,centroideInversores)

# Adjuntamos los l?mites del gr?fico de radar (0 y 1)
# Esto es necesario para utilizar la funci?n gr?fica de r?dar
# Tambi?n adjuntamos el comportamiento medio de la cartera
# para poder comparar cada centroide con la media total

# install.packages("fmsb")
library(fmsb)

centroidesParaRadar<-rbind(
  rep(1,10) , 
  rep(0,10) , 
  centroides)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

for (i in 3:nrow(centroidesParaRadar)-3)
{
  radarchart( as.data.frame(centroidesParaRadar[c(1:3,3+i),])  , axistype=1 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8,
              #custom labels
              vlcex=0.8
  )
}
