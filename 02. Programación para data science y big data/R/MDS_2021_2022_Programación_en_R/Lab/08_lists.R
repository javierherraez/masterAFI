###################################
# Listas: Creación                #
###################################

my_vector <- 1:10 
my_matrix <- matrix(1:9, ncol = 3, byrow = T)
my_df <- mtcars[1:10,]

# Creación de lista sin nombre
l1 <- list(my_vector, my_matrix, my_df)
l1

# Creación de lista con nombre
l2 <- list(vec = my_vector, mat = my_matrix, df = my_df)
l2

# Utilizando al función de composición
l3 <- c(l1, l2)
l3


###################################
# Listas: Operaciones             #
###################################

str(l2)
head(l2)
tail(l2)

append(l1, list(my_vector), 2) #Añadimos un vector en la posición 2
l1[[2]] <- NULL #Eliminamos el elemento de la posición 2


###################################
# Listas: Indexación              #
###################################

# Selección por índice
l2[2] #Devuelve una lista
l2[[2]] #Devuelve una matriz

class(l2[2])
class(l2[[2]])

# Selección por nombre
l2[["mat"]] #Devuelve una matriz
l2$"mat" #Devuelve una matriz
l2["mat"] # Devuelve una lista

l2[["mat"]][1, ] #Selecciona la primera fila de la matriz
l2[["vec"]][3] #Selecciona el tercer elmento del vector
