library(dplyr)
library(caret)

rm(list = ls())
setwd("C:/Users/jherraez/Documents/masterAFI/11. Aprendizaje supervisado I/01. Aprendizaje Estadístico Supervisado/Ejercicio Final")
# setwd("C:/Users/Javier/Documents/masterAFI/11. Aprendizaje supervisado I/01. Aprendizaje Estadístico Supervisado/Ejercicio Final")
cancer_df <- read.csv("BreastCancerData.csv", na.strings = 100, stringsAsFactors = T)

target <- colnames(cancer_df)[ncol(cancer_df)]
target
table(cancer_df[, target])

colnames(cancer_df)[ncol(cancer_df)] <- "Relapse"
levels(cancer_df$Relapse) <- c("No", "Yes")

hist(rowSums(is.na(cancer_df)))
table(rowSums(is.na(cancer_df)))

hist(colSums(is.na(cancer_df)))
table(colSums(is.na(cancer_df)))

#borrar columnas con todos missing values

nasColumns <- sapply(cancer_df, function(x) all(is.na(x)))

cancer_df <- cancer_df[, !(nasColumns)]
table(rowSums(is.na(cancer_df)))
table(colSums(is.na(cancer_df)))

cancer_df <- cancer_df[!(rowSums(is.na(cancer_df)) > 10000),]
row.names(cancer_df) <- 1:nrow(cancer_df)

table(colSums(is.na(cancer_df)))

nasColumns_2 <- sapply(cancer_df, function(x) any(is.na(x)))
cancer_df <- cancer_df[, !(nasColumns_2)]

table(colSums(is.na(cancer_df)))

####################################################

ctrl <- trainControl(method = "cv", 
                     number = 5,
                     classProbs = TRUE
                    )
 
genes <- cancer_df[,1:ncol(cancer_df) - 1]
pre <- preProcess(genes, method = c("pca"), thresh = 0.9)

rdaFit <- train(Relapse ~ ., 
                method = "rda",
                tuneGrid = expand.grid(gamma = seq(0.1, 1, .1), lambda = seq(0.1, 1, .1)),
                metric = "Kappa",
                data = predict(pre, cancer_df),
                trControl = ctrl,
                preProcess = c("center", "scale"))

rdaPred = predict(rdaFit, predict(pre, cancer_df))
confusionMatrix(rdaPred, cancer_df$Relapse)


########################################################

library(cluster)
library(vegan)
library(dplyr)
library(factoextra)

genes_traspose <- as.data.frame(t(genes))


set.seed(1404)
samples <- createDataPartition(genes_traspose$`1`, p = 0.1, list = FALSE)
genes_traspose.subset  <- genes_traspose[samples, ]

fviz_nbclust(genes_traspose.subset, pam, method="wss", k.max = 50) + theme_classic()
# "wss" (for total within sum of square)

pm <- eclust(genes_traspose.subset, FUNcluster="pam", k = 30, hc_metric = "euclidean", hc_method = "ward.D2", graph = F)
medoids <- rownames(pm$medoids)

cancer_clustering <- cancer_df[, c(medoids, "Relapse")]

rdaFitClust <- train(Relapse ~ ., 
                method = "rda",
                tuneGrid = expand.grid(gamma = seq(0.1, 1, .1), lambda = seq(0.1, 1, .1)),
                metric = "Kappa",
                data = cancer_clustering,
                trControl = ctrl,
                preProcess = c("center", "scale")
                )

rdaPredClust = predict(rdaFitClust, genes)
confusionMatrix(rdaPredClust, cancer_df$Relapse)


cost.unit <- c(0, 1, 5, 0)
CM = confusionMatrix(factor(rdaPredClust), cancer_df$Relapse)$table
cost = sum(as.vector(CM)*cost.unit)/sum(CM)
cost

threshold <- 0.2

rdaProbClust = predict(rdaFitClust, genes, type="prob")
rdaPredClust = rep("No", nrow(genes))
rdaPredClust[which(rdaProbClust[,2] > threshold)] = "Yes"

CM = confusionMatrix(factor(rdaPredClust), cancer_df$Relapse)$table
cost = sum(as.vector(CM)*cost.unit)/sum(CM)
cost
