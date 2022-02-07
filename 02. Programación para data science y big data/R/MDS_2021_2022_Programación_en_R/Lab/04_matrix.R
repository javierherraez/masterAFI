###################################
# Matrices: Creación              #
###################################

m1 <- matrix(1:9, byrow = TRUE, nrow = 3)
m1

m2 <- matrix(c(0, -1, 4)) # Crea una matriz con una columna
m2

d1 <- diag(3) # Crea una matriz diagonal 3x3
d1

d2 <- diag(c(1, 2, 3)) # Crea una matriz diagonal y asigna el vector como diagonal
d2

t_m1 <- t(m1) # Traspuesta de m1
e <- eigen(m1) # Lista con autovalores y autovectores
d <- det(m1) # Determinante de la matriz


###################################
# Matrices: Operaciones           #
###################################

a_matrix <- matrix(1:9, byrow = TRUE, nrow = 3)
b_matrix <- matrix(11:19, byrow = TRUE, nrow = 3)

total_matrix <- a_matrix + b_matrix
total_matrix

total_matrix <- a_matrix + 2
total_matrix

rowSums(total_matrix)
colMeans(total_matrix)
max(total_matrix)


###################################
# Matrices: Manipulación          #
###################################

# Unión de matrices por columnas
big_matrix_2 <- cbind(a_matrix, b_matrix)
big_matrix_2

# Unión de matriz y vector por columnas
big_matrix_2 <- cbind(big_matrix_2, c(1, 5, 6))
big_matrix_2


# Unión de matrices por filas
big_matrix_1 <- rbind(a_matrix, b_matrix)
big_matrix_1

# Unión de matriz y vector por filas
big_matrix_1 <- rbind(big_matrix_1, c(1, 5, 6))
big_matrix_1


###################################
# Matrices: Indexación            #
###################################
m <- matrix(1:9, byrow = TRUE, nrow = 3)

# Indexando con números positivos
m[1, ] # Seleccionamos la primera fila
m[1:2, ] # Seleccionamos las dos primeras filas
m[, 3] # Seleccionamos la última columna
m[, c(1, 3)] # Seleccionamos la primera y la última columna
m[1, ] <- 0 # Asigna un vector de ceros a la primera fila

# Indexando con números negativos
m[-1, ] # Seleccionamos todas las filas menos la primera
m[-nrow(m), -ncol(m)] # Quitamos la última fila y la última columna

# Indexando con vectores lógicos o expresiones booleanas
m_selection <- matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE), byrow = TRUE, nrow = 3)
m[m_selection] # Seleccionamos en función de una matriz de booleanos
m[m > 7] # Todos los > 7
m[m == 0] # Todos los 0

# Indexando por nombre
colnames(m) <- c("c1", "c2", "c3")
rownames(m) <- c("r1", "r2", "r3")
m[, c("c1", "c3")] # Selección de columnas por nombre
m[c("r2", "r3"), c("c1", "c2")] # Selección de filas y columnas por nombre
