library(dygraphs)


#Temperatura media anual en New Haven de 1912 a 1971.

data(nhtemp)
nhtemp

dygraph(nhtemp, main = "Temperatura en New Haven")

dygraph(nhtemp, main = "Temperatura en New Haven")%>% 
  dyRangeSelector(dateWindow = c("1930-01-01", "1960-01-01"))


#Número de pasajeros en una aerolínea de 1949-1960 (datos mensuales)

data(AirPassengers)
AirPassengers
dygraph(AirPassengers, main = "Num. Pasajeros")

#3 series temporales mensuales: muertes por bronquitis, enfisema y asma en UK (1974-1979).
#Ambos sexos (ldeaths), hombres (mdeaths) y mujeres (fdeaths).


dygraph(ldeaths)

dygraph(cbind(fdeaths,mdeaths)) %>% dyRangeSelector()

dygraph(cbind(fdeaths,mdeaths))%>%
  dySeries("mdeaths", label = "Hombres") %>%
  dySeries("fdeaths", label = "Mujeres") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 80)

