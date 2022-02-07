#######################################
# Manipulación: dplyr                 #
#######################################

library(hflights)
library(dplyr)

# Contiene información de vuelos de los aeropuertos de Houston
hflights

hflights.tbl <- tbl_df(hflights)

hflights.tbl

glimpse(hflights.tbl)

# Select
hflights.select <- select(hflights.tbl, ActualElapsedTime, AirTime, ArrDelay, DepDelay)
hflights.select

# Mutate
hflights.mutate <- mutate(hflights.select, loss = ArrDelay - DepDelay)
hflights.mutate

# Filter
hflights.select <- select(hflights.tbl, starts_with("Cancel"), DepDelay)
hflights.select
hflights.filter <- filter(hflights.select, Cancelled == 1)
hflights.filter

# Arrange
hflights.select <- select(hflights.tbl, TailNum, contains("Delay"))
hflights.select
hflights.arrange <- arrange(hflights.select, DepDelay)
hflights.arrange
hflights.arrange <- arrange(hflights.select, DepDelay, ArrDelay)
hflights.arrange

# Summarise
hflights.select <- select(hflights.tbl, TailNum, contains("Delay"))
hflights.select <- filter(hflights.select, !is.na(DepDelay))
hflights.summarise <- summarise(hflights.select, min = min(DepDelay), max = max(DepDelay), mean = mean(DepDelay), 
                                median = median(DepDelay))
hflights.summarise

# Group by
hflights.group <- group_by(hflights.tbl, UniqueCarrier)
hflights.summarise.group <- summarise(hflights.group, 
                                      avgDep = mean(DepDelay, na.rm = T), 
                                      avgArr = mean(ArrDelay, na.rm = T))
hflights.summarise.group

# %>%
hflights.tbl %>%
  filter(!is.na(DepDelay)) %>%
  summarise(min = min(DepDelay), max = max(DepDelay), mean = mean(DepDelay), median = median(DepDelay))


