rm(list = ls())
setwd("C:/Users/jherraez/Documents/masterAFI/11. Aprendizaje supervisado I/01. Aprendizaje Estadístico Supervisado/Ejercicio Final")
df <- read.csv("BreastCancerData.csv", na.strings = 100)

hist(rowSums(is.na(df)))
table(rowSums(is.na(df)))

hist(colSums(is.na(df)))
table(colSums(is.na(df)))

#borrar columnas con todos missing values

nasColumns <- sapply(df, function(x) all(is.na(x)))

df <- df[, !(nasColumns)]
