#############################################
# José Manuel Rodríguez Madrid              #
# Datos abiertos con MicroDatosES (EPA)     #
#############################################

library(MicroDatosEs)
library(dplyr)

epa_tbl <- epa2005(".\\microdatos\\md_EPA_2021T1.txt")

summary(epa_tbl)
glimpse(epa_tbl)

epa_tbl <- select(epa_tbl, EDAD, SEXO, NFORMA, AOI, FACTOREL)

# Edad
unique(epa_tbl$EDAD)

# Sexo
unique(epa_tbl$SEXO)

# Nivel de estudios
unique(epa_tbl$NFORMA)

# Actividad
unique(epa_tbl$AOI)
epa_tbl$AOI <- recode(epa_tbl$AOI, 
                      "3" = "Ocupados", "4" = "Ocupados", 
                      "5" = "Parados", "6" = "Parados",
                      "7" = "Inactivos", "8" = "Inactivos", "9" = "Inactivos")

# Conversión de FACTOREL a numérico
epa_tbl$FACTOREL <- as.numeric(epa_tbl$FACTOREL) / 100

# TASA_PARO: número de desempleados entre población activa (ocupados + desocupados)
tasa_paro <- epa_tbl %>%
  filter(!(EDAD %in% c("de 0 A 4 años", "de 5 A 9 años", "de 10 A 15 años")))      # Se eliminan los menores de 16 años

tasa_paro <- tasa_paro %>%
  filter(AOI != "Inactivos")  # Se eliminan los inactivos

summarise(tasa_paro, tasa_paro = 100 * sum(FACTOREL * (AOI == "Parados")) / sum(FACTOREL))

summarise(tasa_paro, num_ocupados = sum(FACTOREL * (AOI == "Ocupados")))

summarise(tasa_paro, num_parados = sum(FACTOREL * (AOI == "Parados")))

# TASA_ACTIVIDAD: población activa (ocupados + desocupados) entre población en edad de trabajar
tasa_actividad <- epa_tbl %>%
  filter(!(EDAD %in% c("de 0 A 4 años", "de 5 A 9 años", "de 10 A 15 años")))      # Se eliminan los menores de 16 años

summarise(tasa_paro, tasa_actividad = 100 * sum(FACTOREL * (AOI != "Inactivos") / sum(tasa_actividad$FACTOREL)))

# Cálculo de la tasa de paro por genero
tasa_paro %>%
  group_by(SEXO) %>%
  summarise(tasa_paro = 100 * sum(FACTOREL * (AOI == "Parados")) / sum(FACTOREL))

# Cálculo de la tasa de paro por edad
tasa_paro %>%
  group_by(EDAD) %>%
  summarise(tasa_paro = 100 * sum(FACTOREL * (AOI == "Parados")) / sum(FACTOREL))

