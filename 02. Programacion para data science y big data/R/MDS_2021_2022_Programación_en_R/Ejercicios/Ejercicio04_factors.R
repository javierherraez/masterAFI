###################################
# Factores                        #
###################################

# Crea dos factores con los siguientes vectores
#   1. El de animales no tiene orden
#   2. El de temperatura tiene orden
animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
temperature_vector <- c("High", "Low", "High","Low", "Medium")

factor_animals_vector <- factor(animals_vector)
factor_animals_vector
factor_temperature_vector <- factor(temperature_vector, ordered = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector

# Modifica los niveles del siguiente factor para que sean: "Female" y "Male"
# Presta atención al orden en que lo estableces
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)

levels(factor_survey_vector) <- c("Female", "Male")

factor_survey_vector

# Haz summary sobre el vector y sobre el factor
summary(survey_vector)

summary(factor_survey_vector)

# Batalla de sexos: ¿Male '>' qué female?
factor_survey_vector[1] > factor_survey_vector[2] 

# Crea un factor ordenado para el siguiente vector
speed_vector <- c("Fast", "Slow", "Slow", "Fast", "Ultra-fast")

factor_speed_vector <-  factor(speed_vector, ordered = TRUE, levels = c("Slow", "Fast", "Ultra-fast"))

factor_speed_vector

summary(factor_speed_vector) 

# Comprueba si la segunda muestra de tus datos es mayor que la quinta
factor_speed_vector[2] > factor_speed_vector[5]