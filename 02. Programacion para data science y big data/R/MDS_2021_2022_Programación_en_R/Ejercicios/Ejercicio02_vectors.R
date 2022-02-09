###################################
# Vectores                        #
###################################

# Crea dos vectores numéricos que representen las ganancias y pérdidas de la semana.
# For poker_vector:
#   On Monday you won 140$
#   Tuesday you lost 50$
#   Wednesday you won 20$
#   Thursday you lost 120$
#   Friday you won 240$
  
# For roulette_vector:
#   On Monday you lost 24$
#   Tuesday you lost 50$
#   Wednesday you won 100$
#   Thursday you lost 350$
#   Friday you won 10$

# Vector con las ganancias en el poker de lunes a viernes
poker_vector <- c(140, -50, 20, -120, 240)

# Vector con las ganancias en la ruleta de lunes a viernes
roulette_vector <-  c(-24, -50, 100, -350, 10)

# Crea un vector con los días de la semana y asignalo como nombre a los elementos de los vectores anteriores
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

names(poker_vector) <- days_vector
names(roulette_vector) <- days_vector

poker_vector
roulette_vector

# Calcula el total de ganancias diario
total_daily <- poker_vector + roulette_vector
total_daily

# Calcula el total de ganancias en el poker
total_poker <- sum(poker_vector)
total_poker

# Calcula el total de ganancias en la ruleta
total_roulette <- sum(roulette_vector)
total_roulette

# Calcula el total de la semana
total_week <- total_poker + total_roulette
total_week

# Selecciona las ganancias del miércoles en el poker (por índice y por nombre)
poker_wednesday <- poker_vector[3]
poker_wednesday <- poker_vector["Wednesday"]

# Selecciona las ganancias del martes, miércoles y jueves en el poker
poker_midweek <- poker_vector[c(2, 3, 4)]
poker_midweek <- poker_vector[c("Tuesday", "Wednesday", "Thursday")]

# Selecciona las ganancias desde el martes al viernes en la ruleta
roulette_selection_vector <- roulette_vector[2:5]

# Calcula la media de ganancias en el poker el lunes, martes y miércoles
average_midweek_gain <- mean(poker_vector[c("Monday", "Tuesday", "Wednesday")])

# ¿Qué días de la semana hubo ganancias al poker?
selection_vector <- poker_vector > 0

# Haz la selección anterior sobre el vector con los datos del poker (prueba a introducir directamente la expresión para indexar)
poker_winning_days <- poker_vector[selection_vector]
poker_winning_days <- poker_vector[poker_vector > 0]

# Repite lo mismo sobre el vector con los datos de la ruleta
roulette_winning_days <- roulette_vector[roulette_vector > 0]