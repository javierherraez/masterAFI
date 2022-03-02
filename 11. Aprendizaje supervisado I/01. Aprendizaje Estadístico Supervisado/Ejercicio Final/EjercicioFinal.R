library(dplyr)
library(caret)

rm(list = ls())
setwd("C:/Users/jherraez/Documents/masterAFI/11. Aprendizaje supervisado I/01. Aprendizaje Estadístico Supervisado/Ejercicio Final")
df <- read.csv("BreastCancerData.csv", na.strings = 100)

target <- colnames(df)[ncol(df)]
target
table(df[, target])

hist(rowSums(is.na(df)))
table(rowSums(is.na(df)))

hist(colSums(is.na(df)))
table(colSums(is.na(df)))

#borrar columnas con todos missing values

nasColumns <- sapply(df, function(x) all(is.na(x)))

df <- df[, !(nasColumns)]
table(rowSums(is.na(df)))
table(colSums(is.na(df)))
####################################################

df <- df[!(rowSums(is.na(df)) > 10000),]

table(colSums(is.na(df)))

nasColumns_2 <- sapply(df, function(x) any(is.na(x)))
df <- df[, !(nasColumns_2)]

table(colSums(is.na(df)))

genes <- df[, 1:ncol(df)-1]

pca = prcomp(genes, scale=T)
summary(pca)


ctrl <- trainControl(method = "cv", number = 5,
                     classProbs = TRUE, 
                     #verboseIter=T,
                    )

pre <- preProcess(df[,1:ncol(df) - 1], method = c("scale","pca"), thresh=0.7)

df_pca <- predict(pre, df)
df_pca$Class <- as.factor(df_pca$Class)
levels(df_pca$Class) <- c("No", "Yes")

rdaFit <- train(Class ~ ., 
                method = "rda",
                tuneGrid = expand.grid(gamma = seq(0, 1, 0.2), lambda = seq(0, 1, .2)),
                metric = "Kappa",
                data = df_pca,
                trControl = ctrl)
