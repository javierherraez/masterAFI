# Load libraries
library(ISLR)
library(leaps)
library(stats)

# Load data
df <- Hitters

head(df)
str(df)
summary(df)

# ==========================================
# Best subset selection
lm_bss <- regsubsets(Salary ~ ., data = df)
summary(lm_bss) #x por defecto fija el parámetro nvmax en 8

lm_bss_best <- regsubsets(Salary ~ ., data = df, nvmax = 20)
lm_bss_best_sumary <- summary(lm_bss_best)

# Métricas incorporadas
names(lm_bss_best_sumary)

# R2
plot(lm_bss_best_sumary$rsq, xlab = "Numero de variables", ylab = "R2", type = "b")

# R2 ajustado
plot(lm_bss_best_sumary$adjr2, xlab = "Numero de variables", ylab = "R2 ajustado", type = "b")
plot(lm_bss_best,scale="adjr2")

# BIC
plot(lm_bss_best_sumary$bic, xlab = "Numero de variables", ylab = "BIC", type = "b")
plot(lm_bss_best,scale="bic")

# Cp Mallows
plot(lm_bss_best_sumary$cp, xlab = "Numero de variables", ylab = "Cp", type = "b")

# AIC
n <- nrow(df)
p <- apply(lm_bss_best_sumary$which, 1, sum)
aic <- lm_bss_best_sumary$bic - log(n) * p + 2 * p
plot(aic, xlab = "Numero de variables", ylab = "AIC", type = "b")

# ==========================================
# Forward stepwise selection
lm_bss_forward <- regsubsets(Salary ~ ., data = df, nvmax = 20, method = 'forward')
lm_bss_forward_sumary <- summary(lm_bss_forward)
lm_bss_forward_sumary

# ==========================================
# Backward stepwise selection
lm_bss_backward <- regsubsets(Salary ~ ., data = df, nvmax = 20, method = 'backward')
lm_bss_backward_sumary <- summary(lm_bss_backward)
lm_bss_backward_sumary

# ==========================================
# Decido quedarme con 6 variables.
lm_6 <- lm(Salary ~ AtBat + Hits + Walks + CRBI + Division + PutOuts, data = df)
summary(lm_6)


