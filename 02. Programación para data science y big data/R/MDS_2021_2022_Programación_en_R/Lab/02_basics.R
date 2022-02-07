# Operadores relacionales #
###########################
TRUE == TRUE

TRUE == FALSE

"hello" == "goodbye"

3 == 2

TRUE != TRUE

TRUE != FALSE

"hello" != "goodbye"

3 != 2

3 < 5

3 > 5

"hello" > "goodbye" # Orden alfabético

TRUE < FALSE # TRUE coerciona a 1 y FALSE a 0

5 >= 3

3 >= 3


##########################################
# Operadores relacionales sobre vectores #
##########################################
a <- c(16, 9, 13, 5, 2, 17, 14)
a
a > 10
b <- c(17, 7, 5, 16, 8, 13, 14)
a <= b


######################
# Operadores lógicos #
######################
x <- 12
x > 5 & x < 15

x <- 17
x > 5 & x < 15

y <- 4
y < 5 | y > 15

y <- 14
y < 5 | y > 15

!(x < 5)

is.numeric(5)
!is.numeric(5)
is.numeric("5")
!is.numeric("5")


#####################################
# Operadores lógicos sobre vectores #
#####################################
c(TRUE, TRUE, FALSE) & c(TRUE, FALSE, FALSE)

c(TRUE, TRUE, FALSE) && c(TRUE, FALSE, FALSE)

c(TRUE, TRUE, FALSE) | c(TRUE, FALSE, FALSE)

c(TRUE, TRUE, FALSE) || c(TRUE, FALSE, FALSE)


############################
# Sentencias condicionales #
############################
x <- -3
if (x < 0) {
  print("x es un número negativo")
}

x <- 5
if (x < 0) {
  print("x es un número negativo")
} else {
  print("x es un número postivo o cero")
}

x <- 5
if (x < 0) {
  print("x es un número negativo")
} else if (x == 0) {
  print("x es cero")
} else {
  print("x es un número postivo o cero")
}

ifelse(x > 0, "x es número positivo", "x es un número negativo")

x <- "size"
switch(x, red = "cloth", size = 5, name = "table")


##########
# Bucles #
##########
ctr <- 1
while (ctr <= 7) {
  print(paste("ctr vale", ctr))
  ctr <- ctr + 1
}

ctr <- 1
while (ctr <= 7) {
  if (ctr %% 5 == 0)
    break
  print(paste("ctr vale", ctr))
  ctr <- ctr + 1
}

cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")
for (city in cities) {
  print(city)
}

cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")
for (city in cities) {
  if (nchar(city) == 6)
    next
  print(city)
}

cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")
for (i in 1:length(cities)) {
  print(paste(cities[i], "está en la posición", i, "del vector de ciudades."))
}


######################
# Control de errores #
######################
inputs = list(1, 2, 4, -5, "oops", 0, 10)

for(input in inputs) {
  print(paste("log of", input, "=", log(input)))
}

for(input in inputs) {
  try(print(paste("log of", input, "=", log(input))))
}

for(input in inputs) {
  tryCatch(print(paste("log of", input, "=", log(input))),
           warning = function(w) { print(paste("negative argument", input)); log(-input) },
           error = function(e) { print(paste("non-numeric argument", input)); NaN })
}

#######################
# Constantes built-in #
#######################
LETTERS

letters

month.abb

month.name

pi


######################
# Entorno de trabajo #
######################
ls()
rm(x)
ls()
help("help")
?help
q()
