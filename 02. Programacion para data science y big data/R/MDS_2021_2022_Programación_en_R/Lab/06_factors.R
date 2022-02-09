###################################
# Factors: Creación               #
###################################

# Creación de factor sin orden
gender_vector <- c("M", "F", "F", "M", "M", "F")
gender_factor <- factor(gender_vector)
gender_factor

# Creación de factor con orden (pero sin especificar un orden)
size_vector <- c("S", "L", "M", "L", "S", "M")
size_factor <- factor(size_vector, ordered = TRUE) # L < M < S
size_factor

# Creación de factor con orden (especificando el orden)
size_vector_2 <- c("S", "L", "M", "L", "S", "M")
size_factor_2 <- factor(size_vector_2, ordered = TRUE, levels = c("S", "M", "L")) # S < M < L
size_factor_2

# Creación de factor especificando etiquetas
gender_levels_2 <- c("M", "F", "-") # Como se leen los datos a la entrada
gender_labels_2 <- c("Male", "Female", NA) # Como se etiquetan
gender_vector_2 <- c("M", "F", "F", "M", "M", "-")
gender_factor_2 <- factor(gender_vector_2, levels = gender_levels_2, labels = gender_labels_2)
gender_factor_2


###################################
# Factors: Operaciones            #
###################################

# Comprobaciones en factors sin orden (solo =)
gender_factor[1] == gender_factor[2]
gender_factor[1] == size_factor[2] # ERROR: solo se pueden comparar si son del mismo tipo

# Comprobaciones en factors con orden (se puede >, <...)
size_factor[1] > size_factor[2]
gender_factor[1] < gender_factor[2] # ERROR: solo se pueden comparar si son del mismo tipo

# Obtener los niveles
levels(size_factor)
levels(size_factor)[1]

# Comprobar la existencia de niveles
any(levels(size_factor) %in% c("L", "S"))

# Añadir nuevos niveles
levels(size_factor)[length(levels(size_factor)) + 1] <- "XL"
levels(size_factor) <- c(levels(size_factor), "XS")

# Reordenar niveles
levels(size_factor)
size_factor <- factor(size_factor, ordered = TRUE, levels(size_factor)[c(5, 3:1, 4)])

# Cambiar/re-nombrar niveles
levels(size_factor)[5] <- "ExtraL"

# Eliminar niveles no utilizados
size_factor <- size_factor[drop = TRUE]
droplevels(size_factor)

# Unir factores
a <- factor(1:10)
b <- factor(letters[a])
union <- factor(c(as.character(a), as.character(b)))
cross <- interaction(a, b)
# ambos producen un conjunto no-ordenado de factors.
# levels: union: 20; cross: 100
# Items: union: 20; cross: 10
