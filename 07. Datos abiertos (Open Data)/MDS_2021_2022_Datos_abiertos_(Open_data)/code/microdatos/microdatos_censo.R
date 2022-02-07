#############################################
# José Manuel Rodríguez Madrid              #
# Datos abiertos con MicroDatosES  (Censo)  #
#############################################

library(MicroDatosEs)
library(dplyr)

censo_tbl <- censo2010(".\\microdatos\\MicrodatosCP_NV_per_BLOQUE1_3VAR.txt")

summary(censo_tbl)
glimpse(censo_tbl)

censo_tbl <- select(censo_tbl, CPRO, EDAD, SEXO, ECIVIL, ESREAL, FACTOR)

# Provincia
unique(censo_tbl$CPRO)

# Edad
unique(censo_tbl$EDAD)
censo_tbl$EDAD <- as.character(cut(censo_tbl$EDAD, 
                                   breaks = c(0, 20, 40, 60, 80, max(censo_tbl$EDAD)), 
                                   labels = FALSE, include.lowest = TRUE))
censo_tbl$EDAD <- recode(censo_tbl$EDAD, 
                         "1" = "0-20", 
                         "2" = "21-40", 
                         "3" = "41-60", 
                         "4" = "61-80",
                         "5" = "81-120")
# Sexo
unique(censo_tbl$SEXO)

# Estado civil
unique(censo_tbl$ECIVIL)

# Nivel de estudios
unique(censo_tbl$ESREAL)

# Porcentaje de hombres y mujeres por provincia
censo_tbl %>%
  group_by(CPRO) %>%
  summarise(porcentajeHombres = sum(FACTOR * (SEXO == "Hombre")) / sum(FACTOR),
            porcentajeMujeres = sum(FACTOR * (SEXO == "Mujer")) / sum(FACTOR))

# Número de mujeres con estudios universitarios por tramo de edad
censo_tbl %>%
  filter(ESREAL == "Diplomatura universitaria, Arquitectura Técnica, Ingeniería Técnica o equivalente"
         | ESREAL == "Licenciatura, Arquitectura, Ingeniería o equivalente") %>%
  filter(SEXO == "Mujer") %>%
  group_by(EDAD) %>%
  summarise(numeroMujeres = sum(FACTOR))

# Proporción de viudos y viudas por provincia y edad
censo_tbl %>%
  filter(ECIVIL == "Viudo") %>%
  group_by(CPRO, EDAD) %>%
  summarise(proporcion = sum(FACTOR * (SEXO == "Hombre")) / sum(FACTOR * (SEXO == "Mujer")))
