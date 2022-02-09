#######################################
# Manipulación: data.table            #
#######################################

library(data.table)

###################################
# Selección en i                  #
###################################
# Creación del data.table
DT <- data.table(x = c("a", "b", "c", "d", "e"), 
                 y = c(1, 2, 3, 4, 5))

# Selección de la tercera fila
DT[2] # Devuelve data.table
DT[[2]] # Devuelve vector

# Selección de la segunda y tercera fila (sin comas)
DT[2:3]

# Selecciona la penultima fila
DT[.N - 1]

# Nombres de las columnas
colnames(DT)

# Dimensiones (filas y columnas)
dim(DT)

# Selecciona la segunda fila dos veces y la tercera
DT[c(2, 2, 3)]


###################################
# Selección en j                  #
###################################
# Creación del data.table
DT <- data.table(A = c(1, 2, 3, 4, 5), 
                 B = c("a", "b", "c", "d", "e"), 
                 C = c(6, 7, 8, 9, 10))

DT[, .(B)] #Devuelve un data.table
DT[, B] #Devuelve un vector

D <- 2
DT[, .(D)] #Devuelve un data.table con el valor 2
DT[[D]] #Devuelve un vector con el valor 2

# Selección de las filas 1 y 3 y columnas B y C
DT[c(1, 3), .(B, C)]

# Selección de la columna B y creación de una columna val como la multiplicación de A por C
ans <- DT[, .(B, val = A * C)]

# Las dos sentencias siguientes producen la misma salida
data.table(B = c("a", "b", "c", "d", "e", "a", "b", "c", "d", "e"), 
           val = as.integer(c(6:10, 1:5)))
DT[, .(B, val = c(C, A))] #Reciclyng: repetición de valores

###################################
# Agrupando en by                 #
###################################
DT <- data.table(A = c(1, 2, 3, 4, 5), 
                 B = c("a", "b", "c", "a", "b"), 
                 C = c(6, 7, 8, 9, 10))

DT[, .(MySum = sum(A), MyMean = mean(A)), by = .(B)]

DT[, .(MySum = sum(C)), by = .(Grp = C%%2)]

###################################
# Encadenando llamadas            #
###################################
DT <- data.table(A = c("c", "b", "a", "c", "b", "a"), 
                 B = c(1, 2, 3, 4, 5, 6))

DT[, sum(B), by = A][order(A)]

DT <- data.table(A = c("b", "b", "b", "b", "a", "a", "a", "a"), 
                 B = c(1, 1, 2, 2, 3, 3, 4, 4),
                 C = c(3, 8, 4, 5, 1, 7, 2, 6))
DT[, .(C = sum(C)), by = .(A, B)][, .(C = tail(C, 2)), by = A]


###################################
# .SD                             #
###################################
DT <- data.table(A = c("b", "b", "b", "b", "a", "a", "a", "a"), 
                 B = c(1, 1, 2, 2, 3, 3, 4, 4),
                 C = c(3, 8, 4, 5, 1, 7, 2, 6))

# Media de todas las columnas
DT[, lapply(.SD, mean), by = A]

# Mediana de todas las columnas
DT[, lapply(.SD, median), by = A]

DT <- data.table(grp = c(6, 6, 8, 8, 8), 
                 Q1 = c(2, 2, 3, 5, 2),
                 Q2 = c(5, 5, 4, 4, 1),
                 Q3 = c(2, 1, 4, 2, 3),
                 H1 = c(3, 4, 5, 2, 4),
                 H2 = c(5, 2, 4, 1, 2))

# La suma de las columnas Q
DT[, lapply(.SD, sum), .SDcols = 2:4]

# La suma de H1 y H2
DT[, lapply(.SD, sum), .SDcols = paste0("H", 1:2)]

# Selecciona todas las filas excepto la primera de cada grupo, devolviendo sólo las columnas de Q
DT[, .SD[-1], by = grp, .SDcols = paste0("Q", 1:3)]

DT <- data.table(x = c(2, 1, 2, 1, 2, 2, 1), 
                 y = c(1, 3, 5, 7, 9, 11, 13),
                 z = c(2, 4, 6, 8, 10, 12, 14))

# Suma todas las columnas (x, y, z) mientras agrupas por x
DT[, lapply(.SD, sum), by = x, .SDcols = c("x", "y", "z")]

# Calcula la suma acumulada de x e y mientras agrupas por x y z > 8
DT[, lapply(.SD, cumsum), by = .(by1 = x, by2 = z > 8), .SDcols = c("x", "y")]

DT <- data.table(A = c("a", "a", "a", "b", "b"),
                 B = c(1, 2, 3, 4, 5))

##################################
# :=                             #
##################################

# Añade una nueva columna al data frame agregando por A
DT[, Total := sum(B) , by = A]

# Añade 1 a la columna B en las filas 2 y 4
DT[c(2, 4), B := B + 1L]

# Añade una nueva columna al data frame agregando por A pero solo en las filas 2, 3, 4
DT[2:4, Total2 := sum(B), by = A]

# Elimina columna
DT[, Total := NULL]

DT <- data.table(A = c(1, 1, 1, 2, 2), 
                 B = 1:5)

# Actualiza B y añade C y D
DT[, `:=`(B = B + 1,  C = A + B, D = 2)]

# Borrado de columnas
my_cols <- c("B", "C")
DT[, (my_cols) := NULL]

# Borra la segunda columna
DT[, 2 := NULL]

###################################
# set, setnames y setcolorder     #
###################################

DT <- data.table(X = c(2, 1, 2, 1, 2, 2, 1), 
                 Y = c(1, 3, 5, 7, 9, 11, 13),
                 Z = c(2, 4, 6, 8, 10, 12, 14))

# Introduce NA en posiciones aleatorias de las columnas 2 y 3
for (i in 2:3)
  set(DT, sample(7, 2), i, NA)

# Cambia el nombres de las columnas
setnames(DT, tolower(names(DT)))

DT

###################################
# key                             #
###################################
DT <- data.table(A = letters[c(2, 1, 2, 3, 1, 2, 3)], 
                 B = c(5, 4, 1, 9,8 ,8, 6), 
                 C = 6:12)

# Creación del índice
setkey(DT, A, B)

# Muestra las columnas que forman el índice
key(DT)

# Filtra por A="b"
DT["b"]

# Filtra por A="b" | A="c"
DT[c("b", "c")]
