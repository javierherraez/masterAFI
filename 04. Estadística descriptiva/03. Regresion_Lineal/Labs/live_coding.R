library(dplyr)
advertising <- read.csv('advertising.csv', sep = ';', header = T, fileEncoding = 'utf-8')
glimpse(advertising)

# RLM ----
lm_fit_sales_all <- lm(Sales ~ TV + Radio + Newspaper, data = advertising)
summary(lm_fit_sales_all)

# RLM with all ---- 
lm_fit_sales_all <- lm(Sales ~ ., data = advertising)

# RLM all without Newspaper ----
lm_fit_sales_TV_Radio <- lm(Sales ~ . - Newspaper, data = advertising)

# Summary of the model
summary(lm_fit_sales_TV_Radio)

# Plot residuals
plot(advertising$Sales, lm_fit_sales_TV_Radio$residuals, 
     type = 'p', col = 'red', xlab = 'Sales', ylab = 'Residuals')
abline(h=0, col="blue")
