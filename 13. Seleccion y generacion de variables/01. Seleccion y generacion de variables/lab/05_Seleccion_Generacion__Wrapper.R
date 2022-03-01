##### Métodos wrapper


# Métodos tradicionales -------------------------------------

# Forward Selection – The algorithm starts with an empty model and 
#                     keeps on adding the significant variables 
#                     one by one to the model.

# Backward Selection – In this technique, we start with all the variables
#                      in the model and then keep on deleting the worst features one by one.

# Stepwise Selection – the Stepwise algorithm is a combination of both 
#                      forward and backward algorithm. 
#                      Which essentially means that at each iteration, 
#                      a variable can be considered for 
#                      addition or deletion from the model.

library(MASS)

# Utilizamos el dataset mtcars
summary(mtcars)

# Ajustamos un modelo lineal frente a todas las variables.
model <- lm(mpg ~., data = mtcars)

# Utilizamos stepwise (both) y backward
stepAIC(model, direction = "both", trace = TRUE)
stepAIC(model, direction = "backward", trace = TRUE)

# El método forward añade variables por lo que no debemos comenzar con 
#   un modelo que incluya todas las variables.
# Empezamos desde el intercepto y le damos, en scope, el modelo máximo
#   que se puede construir
model_forward <- lm(mpg ~1, data = mtcars)
stepAIC(model_forward, direction = "forward", trace = TRUE,
        scope=list(lower = model_forward, 
                   upper=~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb))

# Recursive Feature Elimination (RFE) -------------------------------------
library(caret)
set.seed(42)

inTrain <- createDataPartition(y = mtcars$mpg,
                               p = .80,
                               list = FALSE)
training <- mtcars[ inTrain,]
testing <- mtcars[-inTrain,]

# Parámetros para validación cruzada
ctrl_param <- rfeControl(functions = lmFuncs,
                         method = "repeatedcv",
                         repeats = 5,
                         verbose = FALSE,
                         returnResamp = "all")

# Aplicamos Recursive Feature Elimination
rfe_lm_profile <- rfe(training[, -1], training[, 1],
                      sizes = 3:9,
                      rfeControl = ctrl_param,
                      newdata = testing[, -1])

rfe_lm_profile

# Variables seleccionadas en el modelo óptimo
rfe_lm_profile$optVariables

