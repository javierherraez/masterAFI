# Selección y generación de variables -------------------------------------
# J. Ramón Sánchez Leo
# Febrero 2022

# Usando métodos de filtrado -------------------------------------

## Queremos predecir las millas por galón en función del desplazamiento, 
#   la potencia, el peso y la variable qsec (medida de aceleración).

### 1. Correlación 

df <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec") ]

cor(df)

library(corrplot)
corrplot(cor(df))

### 2. Ganancia de información 
#  Nos dice cuánta información aporta una variable independiente sobre la dependiente. 
library(FSelectorRcpp)

information_gain(mpg~ disp + hp + wt + qsec, mtcars)
df <- information_gain(mpg~ ., mtcars)
df[order(- df$importance),]
