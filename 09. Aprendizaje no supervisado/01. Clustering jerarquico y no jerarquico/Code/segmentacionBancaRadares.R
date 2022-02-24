require(knitr)
library(caret)
library(dplyr)
library(corrplot)
library(nima)
library(cluster)
library(factoextra)
library(vegan)
library(dendextend)

# limpiar espacio de trabajo
rm(list=ls())

# directorio de trabajo
setwd("C:/Users/jherraez/Documents/masterAFI/09. Aprendizaje no supervisado/01. Clustering jerarquico y no jerarquico/")

datosBanca <- read.csv("Data/datosBanca.csv", header = TRUE, sep=",")
summary(datosBanca)

set.seed(1404)
datosBanca <- datosBanca[sample(nrow(datosBanca), nrow(datosBanca) * 0.8), ]
rownames(datosBanca) <- 1:nrow(datosBanca)


datosBanca$checkingAccount_CAT<-discrete_by_quantile(datosBanca$checkingAccount)/4
datosBanca$deposit_CAT<-discrete_by_quantile(datosBanca$deposit)/4
datosBanca$shareOfStock_CAT<-discrete_by_quantile(datosBanca$shareOfStock)/4
datosBanca$pensionPlan_CAT<-discrete_by_quantile(datosBanca$pensionPlan)/4

#datosBanca$mortgage_CAT<-discrete_by_quantile(datosBanca$mortgage)/4
# Como da un error, la asignamos con IF
summary(datosBanca$mortgage)

datosBanca$mortgage_CAT <- datosBanca$mortgage
datosBanca$mortgage_CAT <- ifelse(datosBanca$mortgage <= 0, 1, datosBanca$mortgage_CAT)
datosBanca$mortgage_CAT <- ifelse(0 < datosBanca$mortgage & datosBanca$mortgage <= 45104, 2, datosBanca$mortgage_CAT)
datosBanca$mortgage_CAT <- ifelse(45104 < datosBanca$mortgage & datosBanca$mortgage <= 125979, 3, datosBanca$mortgage_CAT)
datosBanca$mortgage_CAT <- ifelse(125979 < datosBanca$mortgage, 4, datosBanca$mortgage_CAT)
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

datosBanca.cat <- datosBanca %>% 
  select(ends_with('_CAT'))

Mcor <- cor(datosBanca.cat,method = c("spearman"))
corrplot(Mcor, method = 'number')


datosBanca.cat.subset <- datosBanca.cat[sample(nrow(datosBanca.cat), nrow(datosBanca.cat) * 0.1), ]
rownames(datosBanca.cat.subset) <- 1:nrow(datosBanca.cat.subset)


fviz_nbclust(x = datosBanca.cat.subset, FUNcluster = kmeans, method = "wss", k.max = 15,
             diss = get_dist(datosBanca.cat.subset, method = "euclidean"), nstart = 3)


matrizDistancias <- vegdist(datosBanca.cat.subset, method = "euclidean")
clusterJerarquico <- hclust(matrizDistancias, method="ward.D2")

# 
# dend <- as.dendrogram(clusterJerarquico)
# dend <- color_branches(dend, k=8) 
# plot(dend, leaflab = "none")


plot(as.dendrogram(clusterJerarquico),  main = "Dendrograma", leaflab = 'none')

rect.hclust(clusterJerarquico, k=2, border="red") 
rect.hclust(clusterJerarquico, k=3, border="blue") 
rect.hclust(clusterJerarquico, k=4, border="green") 
rect.hclust(clusterJerarquico, k=5, border="yellow") 
rect.hclust(clusterJerarquico, k=6, border="purple") 
rect.hclust(clusterJerarquico, k=7, border="gray") 
rect.hclust(clusterJerarquico, k=8, border="pink")

calinsky <- cascadeKM(datosBanca.cat.subset, inf.gr = 2, sup.gr = 10, criterion = "calinski")
calinsky$results

kmax <- 30
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


# ---------------

asignacionJerarquica <- cbind(datosBanca.cat.subset, cutree(clusterJerarquico, k = 4))

colnames(asignacionJerarquica)[11] <- "cluster"

centroidesJerarquico <- 
  asignacionJerarquica %>% 
  group_by(cluster) %>% 
  summarise(size = n(),
            checkingAccount = mean(checkingAccount_CAT),
            deposit = mean(deposit_CAT),
            shareOfStock = mean(shareOfStock_CAT),
            pensionPlan = mean(pensionPlan_CAT),
            mortgage = mean(mortgage_CAT),
            loan = mean(loan_CAT),
            cards = mean(cards_CAT),
            insurance = mean(insurance_CAT),
            billPayment = mean(billPayment_CAT),
            salary = mean(salary_CAT)
            )

kmeans <- kmeans(datosBanca.cat, centers=centroidesJerarquico[,3:12])
kmeans$centers
kmeans$size

fviz_cluster(object = kmeans, data = datosBanca.cat, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

