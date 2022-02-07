# Fuente: https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/


library(mice) 
library(VIM)

data(iris)
summary(iris)


### Generamos NA's aleatorios
iris_na <- ampute(iris[,1:4],  0.1)

iris_na <- iris_na$amp

sum(is.na(iris_na))
summary(iris_na)

########## Distribución de NA's
### Número de NA's en cada columna o combinaciones de ellas (si las hubiera)

md.pattern(iris_na)

###########  Distribución de NA's gráficamente

aggr(iris_na, col=c('navyblue','yellow'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(iris_na), cex.axis=.7,
     gap=3, ylab=c("Missing data","Pattern"))

###########   Imputación de NA's usando la media

iris_na$Sepal.Length[is.na(iris_na$Sepal.Length)] <- mean(iris_na$Sepal.Length, na.rm = T)

aggr(iris_na, col=c('navyblue','yellow'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(iris_na), cex.axis=.7,
     gap=3, ylab=c("Missing data","Pattern"))


########### Imputación de NA's usando modelos
### MICE:
''' Cada variable tiene su propio modelo de imputación. 
Se proporcionan modelos de imputación incorporados para datos
continuos (pmm), datos binarios (regresión logística),
datos categóricos no ordenados (regresión logística politómica) y
datos categóricos ordenados (odds proporcional) '''

#m: nº de imputaciones múltiples
# maxit: número  de iteraciones para cada imputación. Es importante que las imputaciones alcancen la convergencia

imputed_iris <- mice(iris_na[,1:4], m = 5, maxit = 50,
                     method = 'pmm', seed = 500)
summary(imputed_iris)

# Completa el dataframe original con los datos imputados, quedándose con el resultado de la 2ª imputación

complete_iris <- complete(imputed_iris, 2)


# Modelos con las diferentes completaciones 
## Supongamos que el siguiente paso de nuestro análisis es ajustar un modelo lineal a los datos. 
## Tendríamos decidir qué conjunto de datos imputados elegir.
## Aquí, la función With devuelve una lista con los resultados del modelo para cada imputación, y 
## la función pool hace agregación de las mismas

fit <- with(data = imputed_iris, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))
summary(fit)

### pool: se usa para mostrar los estadísticos básico de las combinaciones de parámetros

combine <- pool(fit)
summary(combine)


##################################
##################################
### OTRO EJEMPLO
##################################
##################################



### Información sobre películas

library(dplyr)

movies <- read.csv('movies.csv', sep =',', 
                   stringsAsFactors = FALSE)

str(movies)
summary(movies)

# Distribución de NA's

table(is.na(movies))

movies_na <- sapply(movies,function(x) sum(is.na(x)))

movies_na

table(movies_na)

barplot(sort(movies_na, decreasing = T))


# Distribución de NA's -- Paquete MICE

library(mice)
library(VIM)

movies_na_pattern <- md.pattern(movies)

aggr(movies, col=c('lightblue','orange'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(movies), cex.axis=.7,
     gap=1, ylab=c("Missing data","Pattern"))


#Imputación Missing Values con MICE


columns_na <- names(movies_na[movies_na > 0])
columns_ok <- names(movies_na[movies_na == 0])


imputed_movies <- mice(movies, m = 5, maxit = 5, method = 'mean')


summary(imputed_movies)

imputed_movies <- mice(movies[,columns_na], m = 5, maxit = 5,
                       method = 'rf', seed = 500)

### Exportamos los datos con los missing values imputados

complete_movies <- complete(imputed_movies, 3)

summary(complete_movies)

### INDEPENDENCIA DE VARIABLES 

library(corrplot)

numericas <- names(complete_movies)[sapply(complete_movies,is.numeric)]

corrplot(cor(complete_movies[numericas]), method= "circle", main = "Correlation Matrix")


#### Outliers

library(ggplot2)


columns_na

complete_movies <- cbind(complete_movies, movies[, columns_ok])
        


ggplot(complete_movies, aes(x=color, y = imdb_score,fill=color, color = color,alpha = 0.35)) +  
        geom_boxplot() 

complete_movies$color[complete_movies$color == ''] <- 'unknown'

ggplot(complete_movies, aes(x = color, y = imdb_score, fill = color, color = color,alpha = 0.35)) +  
        geom_boxplot() 

###########





