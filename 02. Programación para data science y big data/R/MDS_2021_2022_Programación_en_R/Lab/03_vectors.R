###################################
# Vectores: Creación              #
###################################

# Creación de vectores de longitud fija
v1 <- vector(mode = "logical", length = 4)
v1
v2 <- vector(mode = "integer", length = 4)
v2
numeric(4)
character(4)

# Usando el operador de secuencia
v3 <- 1:5
v3
v4 <- 1.4:5.4
v4
v5 <- seq(from = 0, to = 1, by = 0.1)
v5

# Usando la función de combinación
v6 <- c(TRUE, FALSE)
v6
v7 <- c(1.3, 7, 7/20)
v7
v8 <- c("black", "white")
v8
v9 <- c(v1, v3)
v9

# Creación de vector nombres
v10 <- c(a = 1, b = 2, c = 3)
v10


###################################
# Vectores: Operaciones           #
###################################

a_vector <- c(1, 2, 3)
b_vector <- c(4, 5, 6)

total_vector <- a_vector + b_vector
total_vector

total_vector <- a_vector + 1
total_vector

sum(total_vector)
max(total_vector)
mean(total_vector)


###################################
# Vectores: Indexación            #
###################################

v <- 101:200

# Indexando con números positivos
v[1] # Seleccionamos el primer elemento
v[c(1, 1, 4, 5)] # Seleccionamos el primero dos veces, el cuarto y el quinto
v[20:30] # Obtenemos los elementos entre el índice 20 y 30
v[70:100] <- 0 # Asignamos el valor cero a los elemenos entre los índices 70 y 100
v[which(v > 130)] # Seleccionamos los índices de los elementos > 130

# Indexando con números negativos
v[-1] # Seleccionamos todos menos el primer elemento
v[-c(1, 3, 4, 5)] # Seleccionamos todos menos el primero, el tercero, el cuarto y el quinto
v[-length(v)] # Todos menos el último

# Indexando con vectores lógicos o expresiones booleanas
v0 <- v[1:5]
v0[c(TRUE, FALSE, TRUE, FALSE, FALSE)] # Seleccionamos el primero y el tercero
v[v > 130] # Todos los > 130
v[v > 130 & v <= 150] # Todos los > 130 y <= 150
v[v == 0] # Todos los 0
v[v %in% c(110, 120, 130)] # Seleccionamos el 110, 120 y 130

# Indexando por nombre
names(v0) <- c("alpha", "beta", "gamma", "delta", "omega")
v0["alpha"]
v0["beta"] <- 500
v0[c("delta", "omega")]
v0[!(names(v0) %in% c("alpha", "beta"))]


###################################
# Vectores: Traps                 #
###################################

x <- c(5, "a") # Convierte 5 a "5"
x <- 1:3
x[3] <- "a" # Convierte a "1", "2" y "a"
typeof(1:2) == typeof(c(1, 2))

c(T, F, T) && c(T, F, F) # TRUE !vectorizada
c(T, F, T) & c(T, F, F) # TRUE, FALSE, FALSE
