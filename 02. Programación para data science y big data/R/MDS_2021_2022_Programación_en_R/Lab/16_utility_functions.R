###############################
# Funciones sobre estructuras #
###############################

rep(NA, 10)

append(1:20, c(1, 2, 3))

seq(from = 5, to = 100, by = 5)

x <- c(15, 26, 5, 9, 1, -9)
sort(x)
order(x)
rank(x)
rev(x)

any(x %in% c(1, 3, 5))
all(x %in% c(1, 3, 5))
all(c(15, 9, 1) %in% x)
which(c(48, 50, 1, 20, -9) %in% x)
match(26, x)
match(6, x)


#########################
# Funciones matemáticas #
#########################
x <- seq(from = 5, to = 100, by = 5)
is.nan(x)
is.finite(x)
is.infinite(x)
abs(x)
sqrt(x)
log(x)
log10(x)
exp(x)
ceiling(log(x))
floor(log(x))
round(log(x), digits = 2)
trunc(log(x))
sin(x)
cos(x)
tan(x)
sum(x)
prod(x)
cumsum(x)
cumprod(x)


########################
# Funciones de cadenas #
########################
s <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur."
as.character(x)
toString(x)
nchar(s)
toupper(s)
tolower(s)
sub("Lorem", "hola", s)
gsub("in", "adios", s)
substr(s, 7, 11)
substr(s, 7, 11) <- "IPSUM"
paste("a", "b", "c", sep = ";")
strsplit(s, " ")
grep("in", s)


########################
# Funciones de fechas  #
########################

today <- Sys.Date()
today
class(today)

now <- Sys.time()
now
class(now)

d <- as.Date("2016-03-17")
d
d <- as.Date("17-03-2016") # No da error pero lo hace mal
d
d <- as.Date("17-03-2016", format = "%d-%m-%Y")
d

t <- as.POSIXct("2016-03-17 22:32:00")
t
t <- as.POSIXct("17-03-2016 22:32:00", format = "%d-%m-%Y %H:%M:%S")
t

# Cálculos con fechas
d + 1 #Añade un día

d2 <- as.Date("2015-03-17")
d - d2 #Diferencia en días

t + 1 #Añade un segundo

t2 <- as.POSIXct("2015-03-17 22:32:00")
t - t2 #Diferencia en días

unclass(d) #Representación como entero: número de días desde el 1 Enero 1970
unclass(t) #Representación como entero: número de segundos desde el 1 Enero 1970
