###################################
# Data frames: Creación           #
###################################
# Creación de data frame vacío
empty <- data.frame()

# A partir de dos vectores
c1 <- 1:10 # vector de enteros
c2 <- letters[1:10] # vector de strings
df <- data.frame(col1 = c1, col2 = c2)

# Lectura desde fichero
df <- read.csv("filename.csv", header = T) #ERROR: fichero no existe


###################################
# Data frames: Operaciones        #
###################################
# Análisis exploratorio
head(mtcars)
head(mtcars, 10)
head(mtcars, -10)

tail(mtcars)
tail(mtcars, 10)
tail(mtcars, -10)

str(mtcars)

summary(mtcars)


###################################
# Data frames: Manipulacion       #
###################################
# Añadir filas
df <- rbind(mtcars, data.frame(mpg = 22, cyl = 5, disp = 202, hp = 100, drat = 2.56, wt = 3.1, 
                               qsec = 15, vs = 1, am = 0, gear =5, carb = 4, 
                               row.names=c("seat")))

# Añadir columnas
df$newcolumn <- rep(1, nrow(df))
df[, 'copyofhp'] <- df$hp
df$hp.gear <- df$hp / df$gear
v <- 1:nrow(df)
df <- cbind(df, v)


###################################
# Data frames: Indexación         #
###################################

# Indexando celdas
df <- data.frame(mtcars)
str(df)
df[5, 2] # Obtiene una única celda
df[1:5, 1:2] # Obtiene varias celdas
df[1:2, c("gear", "am")]
df[1:2, c("gear", "am")] <- 0 # Asignación de celdas
df[1:2, c("gear", "am")]

# Indexando filas (siempre devuelve data frames)
df[1, ]
df[-nrow(df), ]
df[1:5, ]
df[(df$hp > 150 & df$hp < 200), ]
subset(df, hp > 150 & hp < 200)

vrow <- as.numeric(as.vector(df[1, ])) # Convertimos el resultados de la indexación en vector

# Indexando columnas
df$hp # Devuelve un vector
df[, "hp"] # Devuelve un vector
df[, 4] # Devuelve un vector
df["hp"] # Devuelve un data frame con una columna
df[4] # Devuelve un data frame con una columna
df[["hp"]] # Devuelve un vector
df[ , c(4, 6)] # Devuelve un data frame
df[ , c("hp", "wt")] # Devuelve un data frame


###################################
# Data frames: merge              #
###################################

c1 <- 1:10
c2 <- letters[1:10]
c3 <- 5:20
c4 <- letters[5:20]
df.x <- data.frame(col1 = c1, col2 = c2)
df.y <- data.frame(col1 = c3, col2 = c4)

join <- merge(df.x, df.y, by = c("col1")) #join
join <- merge(df.x, df.y, by.x = "col1", by.y = "col1") #join
left.join <- merge(df.x, df.y, by = c("col1"), all.x = T) #left join
right.join <- merge(df.x, df.y, by = c("col1"), all.y = T) #right join
full.join <- merge(df.x, df.y, by = c("col1"), all = T) #full join
