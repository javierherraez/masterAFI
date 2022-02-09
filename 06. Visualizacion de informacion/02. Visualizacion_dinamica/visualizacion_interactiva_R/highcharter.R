library(dplyr)
library(ggplot2)
library(highcharter)

data("diamonds")

##########################

#scatter plot

set.seed(42)

index <- sample.int(nrow(diamonds), 1000)

hchart(diamonds, "scatter", hcaes(x = "carat", 
                                  y = "price"))

hchart(diamonds[index,], "scatter", hcaes(x = "carat", 
                                 y = "price", group = "cut"))

hchart(diamonds[index,], "scatter", hcaes(x = "carat", 
                                  y = "price", group = "cut",
                                  size = "depth"))

#Bar plot

diamonds %>% count(cut) %>%
  arrange(n) %>%
  hchart(type = "bar", hcaes(x = "cut", y = "n"))

by_clarity <- diamonds %>% 
  group_by(cut, clarity) %>% 
  summarise(n = n()) %>%  
  arrange(cut,n) 


 diamonds %>% 
 count(cut,clarity) %>%
  hchart(type = "column", hcaes(x = "cut", y = "n", group = "clarity"))

# Histogram
 
hchart(diamonds$price) 
 
# Density

hchart(density(diamonds$price),
       color = "lightsalmon", name = "Price Density")

#Correlation Matrix

numeric <- sapply(diamonds, is.numeric)

hchart(cor(diamonds[numeric]))
 
