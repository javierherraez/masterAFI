library(dplyr)
library(caret)

rm(list = ls())
setwd("C:/Users/jherraez/Documents/masterAFI/11. Aprendizaje supervisado I/01. Aprendizaje Estadístico Supervisado/Ejercicio Final")
genes <- read.csv("BreastCancerData.csv", na.strings = 100, stringsAsFactors = T)

target <- colnames(genes)[ncol(genes)]
target
table(genes[, target])

colnames(genes)[ncol(genes)] <- "Relapse"
levels(genes$Relapse) <- c("No", "Yes")

hist(rowSums(is.na(genes)))
table(rowSums(is.na(genes)))

hist(colSums(is.na(genes)))
table(colSums(is.na(genes)))

#borrar columnas con todos missing values

nasColumns <- sapply(genes, function(x) all(is.na(x)))

genes <- genes[, !(nasColumns)]
table(rowSums(is.na(genes)))
table(colSums(is.na(genes)))

genes <- genes[!(rowSums(is.na(genes)) > 10000),]
row.names(genes) <- 1:nrow(genes)

table(colSums(is.na(genes)))

nasColumns_2 <- sapply(genes, function(x) any(is.na(x)))
genes <- genes[, !(nasColumns_2)]

table(colSums(is.na(genes)))

####################################################

ctrl <- trainControl(method = "cv", 
                     number = 5,
                     classProbs = TRUE
                    )

pre <- preProcess(genes[,1:ncol(genes) - 1], method = c("pca"), thresh = 0.9)

rdaFit <- train(Relapse ~ ., 
                method = "rda",
                tuneGrid = expand.grid(gamma = seq(0.1, 1, .1), lambda = seq(0.1, 1, .1)),
                metric = "Kappa",
                data = predict(pre, genes),
                trControl = ctrl)

ctrl2 = trainControl(method = "cv", 
                     number = 5, 
                     classProbs = TRUE, 
                     preProcOptions = list(thresh = 0.9))

rdaFit2 <- train(Relapse ~ ., 
                method = "rda",
                tuneGrid = expand.grid(gamma = seq(0.1, 1, .1), lambda = seq(0.1, 1, .1)),
                metric = "Kappa",
                data = genes,
                trControl = ctrl2,
                preProcess = c('pca'))


########################################################

library(cluster)
library(vegan)
library(dplyr)

genes_traspose <- as.data.frame(t(as.matrix(genes)))
genes_traspose.genes <- as.data.frame(genes_traspose[1:nrow(genes_traspose) - 1,])
genes_traspose.genes <- as.data.frame(lapply(genes_traspose.genes, function(x) { as.numeric(x)}))

genes_traspsoe.genes.subset <- genes_traspose.genes[sample(nrow(genes_traspose.genes), nrow(genes_traspose.genes) * 0.3), ]
matrizDistancias <- vegdist(genes_traspsoe.genes.subset, method = "euclidean")
clusterJerarquico <- hclust(matrizDistancias, method="ward.D2")

kmax <- ncol(genes_traspsoe.genes.subset)
asw <- numeric(kmax)
for(k in 2:kmax){
  sil <- silhouette(cutree(clusterJerarquico, k = k), matrizDistancias)
  asw[k] <- summary(sil)$avg.width
}
k.best <- which.max(asw)

plot(1:kmax, asw, type="h", 
     main = "Silhouette-optimal number of clusters", 
     xlab = "k (number of groups)", ylab = "Average silhouette width")
axis(1, k.best, paste("optimum", k.best, sep = "\n"), col = "red", font = 2,
     col.axis = "red")
points(k.best, max(asw), pch = 16, col = "red", cex = 1.5)

asignacionJerarquica <- cbind(genes_traspsoe.genes.subset, cutree(clusterJerarquico, k = 4))
colnames(asignacionJerarquica)[97] <- "cluster"
as.data.frame(asignacionJerarquica) %>% group_by(cluster) %>% summarise(across(everything(), list(mean)))

