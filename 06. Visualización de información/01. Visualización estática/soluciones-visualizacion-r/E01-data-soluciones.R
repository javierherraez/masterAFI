# Incluimos los paquetes necesarios
library(dplyr)

# Leemos el set de datos
weather <- read.table('Visualización estática/material-visualizacion-r/code/data/weather-madrid.txt')

# Comprobamos su contenido
nrow(weather)
str(weather)
head(weather)

# Identificamos las columnas y renombramos para facilitar su tratamiento a day, month, year y temp
unique(weather$V1)
unique(weather$V2)
unique(weather$V3)
colnames(weather) <- c('month', 'day', 'year', 'temp')
head(weather)

# Revisamos la información de fechas y vemos si hay "cosas raras"
table(weather$year)
weather <- weather[weather$year < 2015,]
table(weather$day)
table(weather$month)

# Revisamos la información de temperaturas y vemos si hay "cosas raras"
summary(weather$temp)
boxplot(weather$temp)
sum(weather$temp < 0)
sum(weather$temp < 0) / nrow(weather)
weather <- weather[weather$temp > 0, ]

# Convertimos la información de fechas en grados Celsius: ºC = (ºF - 32) / 1.8
weather$temp <- (weather$temp - 32) / 1.8
summary(weather$temp)

# Creamos una data.frame "past" de información histórica que contenga:
# - Información agregada desde 1995 a 2013.
# - Una fila por cada día del año (de 1 a 366)
# - El valor mínimo por cada día de los últimos años
# - El valor máximo por cada día de los últimos años
# - Los límites inferior y superior del intervalo de confianza al 95% de la media de temperaturas

past <- weather %>% 
  filter(year < 2014) %>% 
  arrange(year, month, day) %>% 
  group_by(year) %>% 
  mutate(year.day = seq(1, length(day))) %>% 
  group_by(year.day) %>% 
  summarise(min = min(temp), 
         max = max(temp), 
         mean_inf = mean(temp) - qt(0.975, 2013 - 1995) * sd(temp) / sqrt(n()),
         mean_sup = mean(temp) + qt(0.975, 2013 - 1995) * sd(temp) / sqrt(n())) %>% 
  as.data.frame()

# Creamos un data.frame "present" de información actual que contenga:
# - Información sólo de 2014
# - Una fila por cada día del año (de 1 a 366, si tenemos todos los días)
# - El valor de cada día

present <- weather %>% 
  filter(year == 2014) %>% 
  arrange(year, month, day) %>% 
  mutate(year.day = seq(1, length(day))) %>% 
  select(year.day, temp)

# Creamos un data.frame "present.highs" de información actual que contenga:
# - Información sólo de 2014
# - Una fila por cada día del año (de 1 a 366, se tenemos todos los días)
# - Un flag "Y" o "N" que indique si la temperatura del día de 2014 es mayor que las de los 18 años anteriores
# - Nos quedamos sólo con las fechas y temperaturas de los días más calurosos

present.highs <- present %>% 
  left_join(past, by = "year.day") %>% 
  mutate(is.higher = ifelse(temp > max, "Y", "N")) %>% 
  filter(is.higher == "Y") %>% 
  select(year.day, temp)
  
# Creamos un data.frame "present.lows" de información actual que contenga:
# - Información sólo de 2014
# - Una fila por cada día del año (de 1 a 366, se tenemos todos los días)
# - Un flag "Y" o "N" que indique si la temperatura del día de 2014 es menor que las de los 18 años anteriores
# - Nos quedamos sólo con las fechas y temperaturas de los días más fríos

present.lows <- present %>% 
  left_join(past, by = "year.day") %>% 
  mutate(is.lower = ifelse(temp < min, "Y", "N")) %>%
  filter(is.lower == "Y") %>% 
  select(year.day, temp)

