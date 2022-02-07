
library(ISLR)
library(caret)

data(Auto)
str(Auto)

p <- 0.80
trainIndex <- createDataPartition(Auto$mpg, p = p, list = F)
training_data <- Auto[ trainIndex,]
testdata <- Auto[-trainIndex,]

mod_1 <- lm(mpg ~ horsepower, data = training_data)

# LOOCV

train_control <- trainControl(method="LOOCV") 

# LGOCV

train_control <- trainControl(method="LGOCV", number=10, p = 0.8) #p: training percentage 


# k-fold CV

train_control <- trainControl(method="cv", number = 10) #10 - fold

# Bootstrap 

train_control <- trainControl(method="boot", number=100)

########################################################################################
#train the model
model <- train(mpg~., data=Auto, trControl=train_control, method="lm")

summary(model)
print(model)
