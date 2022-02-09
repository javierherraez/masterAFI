###################################
# dplyr                           #
###################################
library(hflights)
library(dplyr)

# Contiene información de vuelos de los aeropuertos de Houston
hflights

hflights.tbl <- tbl_df(hflights)

hflights.tbl

glimpse(hflights.tbl)

# Recodifica la variable UniqueCarrier a partir del vector recode.carrier
recode.carrier <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
                    "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
                    "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
                    "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

hflights.tbl$UniqueCarrier <- recode.carrier[hflights.tbl$UniqueCarrier]

# Comprueba de nuevo el contenido
glimpse(hflights.tbl)

# Recodifica la variable CancellationCode a partir del vector recode.cancellation
hflights.tbl[hflights.tbl$CancellationCode == "", "CancellationCode"] <- "E"
recode.cancellation <-  c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")
hflights.tbl$CancellationCode <- recode.cancellation[hflights.tbl$CancellationCode]

# Comprueba de nuevo el contenido
glimpse(hflights.tbl)

# Selecciona las variables: ActualElapsedTime, AirTime, ArrDelay, DepDelay
select(hflights.tbl, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

# Selecciona las variables desde Origin a Cancelled
select(hflights.tbl, Origin:Cancelled)

# Selecciona todas las variables menos aquellas entre DepTime y AirTime
select(hflights.tbl, -DepTime:-AirTime)

# Selecciona todas las variables que terminan en "Delay"
select(hflights.tbl, ends_with("Delay"))

# Selecciona todas las variables que terminan en "Num" o empiezan por "Cancell"
select(hflights.tbl, ends_with("Num"), starts_with("Cancell"))

# Selecciona todas las variables que terminan en "Num" o empiezan por "Cancell" y además la variable UniqueCarrier
select(hflights.tbl, UniqueCarrier, ends_with("Num"), starts_with("Cancell"))

# Selecciona todas las variables que terminan en "Time" o "Delay"
select(hflights.tbl, ends_with("Time"), ends_with("Delay"))

# Traduce las siguiente selecciones tradicionales empleando "select"
ex1r <- hflights.tbl[c("TaxiIn", "TaxiOut", "Distance")]
ex1d <- select(hflights.tbl, starts_with('Taxi'), Distance)

ex2r <- hflights.tbl[c("Year", "Month", "DayOfWeek", "DepTime", "ArrTime")]
ex2d <- select(hflights.tbl, Year:ArrTime, -DayofMonth)

ex3r <- hflights.tbl[c("TailNum", "TaxiIn", "TaxiOut")]
ex3d <- select(hflights.tbl, TailNum, starts_with("Taxi"))

# Crea una nueva variable ActualGroundTime como la resta de ActualElapsedTime y AirTime. Guarda el resultado en g1
g1 <- mutate(hflights.tbl, ActualGroundTime = ActualElapsedTime - AirTime)

# Añade a g1 una nueva variable GroundTime como la suma TaxiIn y TaxiOut
g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut)

# Añade a g2 una nueva variable AverageSpeed como Distance/AirTime*60
g3 <- mutate(g2, AverageSpeed = Distance/AirTime*60)

# Pinta g3
g3

# Crea dos variables al tiempo y guardalas en m1:
#     loss como la resta de ArrDelay y DepDelay
#     loss_percent como el ratio de ArrDelay - DepDelay entre DepDelay
m1 <- mutate(hflights.tbl, loss = ArrDelay - DepDelay, loss_percent = (ArrDelay - DepDelay) / DepDelay * 100)

# ¿Puedes simplificar la creación de la segunda variable con la primera? Guardalo en m2
m2 <- mutate(hflights.tbl, loss = ArrDelay - DepDelay, loss_percent = loss / DepDelay * 100)

# Crea tres variables al timepo y guardalas en m3. Utiliza la simplificación anterior
#     TotalTaxi como la suma de TaxiIn y TaxiOut
#     ActualGroundTime como la diferencia de ActualElapsedTime y AirTime
#     Diff como la diferencia de las dos variables anteriores ¿Es siempre cero?
m3 <- mutate(hflights.tbl, TotalTaxi = TaxiIn + TaxiOut, ActualGroundTime = ActualElapsedTime - AirTime, Diff = TotalTaxi - ActualGroundTime)

# Filtra los vuelos con distancia recorrida mayor o igual que 3000
filter(hflights.tbl, Distance >= 3000)

# Filtra los vuelos de JetBlue, Southwest, o Delta
filter(hflights.tbl, UniqueCarrier %in% c("JetBlue", "Southwest", "Delta"))

# Filtra los vuelos en los que el tiempo en Taxi fue mayor que el tiempo de vuelo (No uses mutate)
filter(hflights.tbl, TaxiOut + TaxiIn  > AirTime)

# Filtra los vuelos que salieron antes de las 5am y llegaron después de las 10pm
filter(hflights.tbl, DepTime < 500 & ArrTime > 2200)

# Filtra los vuelos que salieron con retraso y llegaron a tiempo
filter(hflights.tbl, DepDelay > 0 & ArrDelay < 0)

# Filtra los vueltos cancelados en fin de semana
filter(hflights.tbl, Cancelled == 1 & DayOfWeek %in% c(6, 7))

# Filtra los vueltos cancelados después de haber sido retrasados
filter(hflights.tbl, Cancelled == 1, DepDelay > 0)

# Selecciona los vuelos con destino JFK
# Crea una nueva variable Date a partir de  Year, Month y DayofMont
# Muestra una selección de las variables Date, DepTime, ArrTime y TailNum
c1 <- filter(hflights.tbl, Dest == "JFK")
c2 <- mutate(c1, Date = paste(Year, Month, DayofMonth, sep = "-"))
select(c2, Date, DepTime, ArrTime, TailNum)

# Ordena la tabla dtc según el retraso de salida
dtc <- filter(hflights.tbl, Cancelled == 1, !is.na(DepDelay))
arrange(dtc, DepDelay)

# Ordena la tabla dtc según el motivo de cancelación
arrange(dtc, CancellationCode)

# Ordena la tabla dtc según la compañía y el retraso de salida
arrange(dtc, UniqueCarrier, DepDelay)

# Ordena todos los vuelos según la compañía y el retraso de salida descendentemente
arrange(hflights.tbl, UniqueCarrier, desc(DepDelay))

# Ordena todos los vuelos según el retraso total (No uses mutate)
arrange(hflights.tbl, ArrDelay + DepDelay)

# Filtra los vuelos con destino DFW y hora de salida anterior a las 8am, ordenalos según el tiempo en vuelo descendentemente
# Hazlo en una sola instrucción
arrange(filter(hflights.tbl, (Dest == "DFW") & (DepTime < 800)), desc(AirTime))

# Calcula la distancia mínima y máxima de todos los vuelos
summarise(hflights.tbl, min_dist = min(Distance), max_dist = max(Distance))

# Calcula la distancia máxima de todos los vuelos desviados
summarise(filter(hflights.tbl, Diverted == 1), max_div = max(Distance))

# Crea en una nueva variable con aquellos vuelos que no tienen NA en ArrDelay
# Calcula los siguientes estadísticos:
#    earliest: el mínimo retraso de llegada
#    average: el retraso medio de llegada
#    latest: el máximo retraso de llegada
#    sd: la desviación estandar en el retraso de llegada
temp1 <- filter(hflights.tbl, !is.na(ArrDelay))
summarise(temp1, earliest = min(ArrDelay), average = mean(ArrDelay), latest = max(ArrDelay), sd = sd(ArrDelay))

# Crea en una nueva variable con aquellos vuelos que no tienen NA en TaxiIn y TaxiOut
# Calcula un estadístico (max_taxi_diff) que contenga la mayor diferencia en valor absoluto entre TaxiIn y TaxiOut
temp2 <- filter(hflights.tbl, (!is.na(TaxiIn)) & (!is.na(TaxiOut)))
summarise(temp2, max_taxi_diff = max(abs(TaxiIn - TaxiOut)))

# Calcula los siguientes estadísticos:
#    n_obs: número de observaciones
#    n_carrier: número total de compañías
#    n_dest: número total de destinos
#    dest100: el destino del vuelo en la posición 100
summarise(hflights.tbl, n_obs = n(), n_carrier = n_distinct(UniqueCarrier), n_dest = n_distinct(Dest), dest100 = nth(Dest, 100))

# Crea en una nueva variable con aquellos vuelos de la compañía "American"
temp3 <- filter(hflights.tbl, UniqueCarrier == 'American')
# Calcula los siguientes estadísticos:
#    n_flights: el número de vuelos
#    n_canc: el número de vuelos cancelados
#    p_canc: el porcentaje de vuelos cancelados
#    avg_delay: el tiempo medio de los vuelos (acuerdate de eliminar los NAs)
summarise(temp3, n_flights = n(), n_canc = sum(Cancelled), p_canc = n_canc / n_flights * 100, avg_delay = mean(ArrDelay, na.rm = TRUE))

# Usando los pipes realiza:
#    1. Crea una variable diff como la diferencia de TaxiIn y TaxiOut
#    2. Filtra aquellas filas para las que diff no es NA
#    3. Calcula la media
hflights.tbl %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff))

# Usando los pipes realiza:
#    1. ¿Cuanto vuelos nocturos hay? Los vuelos nocturnos son aquellos para los que la hora de llegada es menor que la de salida
# Cuidado con los NAs
hflights.tbl %>%
  filter(!is.na(DepTime) & !is.na(ArrTime) & ArrTime < DepTime) %>%
  summarise(n = n())

# Usando los pipes realiza:
#    1. Para cada compañía calcula los siguientes estadísticos:
#       n_flights: número de vuelos
#       n_canc: número de vuelos cancelados
#       p_canc: porcentaje de vuelos cancelados
#       avg_delay: retraso medio de llegada (cuidado con los NAs)
#    2. Ordena los resultados por el retraso medio y el porcentaje de vuelos cancelados
hflights.tbl %>%
  group_by(UniqueCarrier) %>%
  summarise(n_flights = n(), 
            n_canc = sum(Cancelled), 
            p_canc = mean(Cancelled) * 100, 
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%
  arrange(avg_delay, p_canc)

# Usando los pipes realiza:
#    1. Para cada día de la semana calcula los siguientes estadísticos:
#       avg_taxi: tiempo medio de la suma de las variables TaxiIn y TaxiOut (cuidado con los NAs)
#    2. Ordena los resultados descendentemente segun avg_taxi
hflights.tbl %>% 
  group_by(DayOfWeek) %>%
  summarise(avg_taxi = mean(TaxiIn + TaxiOut, na.rm=TRUE)) %>%
  arrange(desc(avg_taxi))

# Usando los pipes realiza:
#    1. Filtra aquellos cuyo retraso de llegada es distinto de NA
#    2. Agrega por compañía
#    3. Calcula la proporción de vuelos con retraso (p_delay)
#    4. Crea una nueva variable rank, que es el ranking de p_delay
#    5. Ordena el resultado segun rank
hflights.tbl %>% 
  filter(!(is.na(ArrDelay))) %>%
  group_by(UniqueCarrier) %>%
  summarise(p_delay = mean(ArrDelay > 0)) %>%
  mutate(rank = rank(p_delay)) %>%
  arrange(rank)

# Usando los pipes realiza:
#    1. Filtra aquellos cuyo retraso de llegada es distinto de NA y mayor que cero
#    2. Agrega por compañía
#    3. Calcula el tiempo medio de restraso (avg_delay)
#    4. Crea una nueva variable rank, que es el ranking de avg_delay
#    5. Ordena el resultado segun rank
hflights.tbl %>%
  filter(!is.na(ArrDelay), ArrDelay > 0) %>%
  group_by(UniqueCarrier) %>%
  summarise(avg_delay = mean(ArrDelay)) %>%
  mutate(rank = rank(avg_delay)) %>%
  arrange(rank)

# Usando los pipes calcula que avión (TailNum) volo más veces. ¿Cuántas?
hflights.tbl %>%
  group_by(TailNum) %>%
  summarise(n = n()) %>%
  filter(n == max(n))

# ¿Cuántos aviones volaron a un único destino?
hflights.tbl %>%
  group_by(TailNum) %>%
  summarise(ndest = n_distinct(Dest)) %>%
  filter(ndest == 1) %>%
  summarise(nplanes = n())

# ¿Cuál es el destino más visitado de cada compañía?
hflights.tbl %>% 
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

# ¿Qué compañías viaja más a cada destino?
hflights.tbl %>% 
  group_by(Dest, UniqueCarrier) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)