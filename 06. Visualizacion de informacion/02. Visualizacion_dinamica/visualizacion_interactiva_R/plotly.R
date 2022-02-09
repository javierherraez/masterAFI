library(dplyr)
library(ggplot2)
library(plotly)


data("diamonds")

# ggplot2 + plotly


graf1 <- diamonds %>% ggplot(aes(x= carat, y = price, color = clarity)) +
          geom_point(alpha = 0.4)
graf1

ggplotly(graf1)

#######################################

# plotly

## Scatter plot

# add_trace o plot_ly

p <- plot_ly(data = iris, type = "scatter",
             mode = "markers", x = ~Sepal.Length, y = ~Petal.Length)
p

p <- plot_ly(data = iris,type = "scatter", mode = 'markers',
             x = ~Sepal.Length, y = ~Petal.Length,
             marker = list(size = 10,
                           color = 'lightsalmon',
                           line = list(color = 'salmon',
                                       width = 3))) %>%
  layout(title = 'Diagrama de puntos personalizado',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

p

## Barplot

diamonds %>% count(cut, clarity) %>%
  plot_ly(x = ~cut, y = ~n,type = 'bar', color = ~clarity)

#Otra opción, con histograma, que por defecto usa la función "count"
diamonds %>% 
  plot_ly(x = ~cut, type = 'histogram', color = ~clarity)

## Boxplot

diamonds %>% 
  plot_ly(x = ~cut, y = ~price, type = 'box')


diamonds %>% 
  plot_ly(y = ~cut, x = ~price, type = 'box', color = ~cut)


