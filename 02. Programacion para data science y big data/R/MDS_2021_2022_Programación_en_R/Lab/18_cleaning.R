#######################################
# Limpieza: exploración               #
#######################################

# Cargamos el dataset
vinos <- read.csv("dat/wine.csv", 
                  header = T)

# Exploración de la estructura
class(vinos)
dim(vinos)
names(vinos)
str(vinos)
summary(vinos)

# Exploración de los datos
head(vinos)
tail(vinos)
print(vinos)

# Visualizando los datos
hist(vinos$alcohol)
plot(vinos$alcohol, vinos$proline)


#######################################
# Limpieza: ordenación                #
#######################################

library(tidyr)
library(reshape2)

wide_df <- data.frame(col = c("X", "Y"), A = c(1, 4), B = c(2, 5), C = c(3, 6))

# gather, pivot_longer, melt
long_df <- gather(wide_df, my_key, my_val, -col)
long_df_2 <- pivot_longer(wide_df, -col, names_to = "my_key", values_to = "my_val")
long_df_3 <- melt(wide_df, id.vars = c("col"), variable.name = "my_key", value.name = "my_val")

# spread, pivot_wide, dcast
new_wide_df <- spread(long_df, my_key, my_val) #Obtenemos el data.frame original
new_wide_df_2 <- pivot_wider(long_df_2, names_from = my_key, values_from = my_val)
new_wide_df_3 <- dcast(long_df_3, col ~ my_key, value.var = "my_val")


treatments <- data.frame(patient = c("X", "Y", "X", "Y", "X", "Y"), 
                         treatment = c("A", "A", "B", "B", "c", "C"), 
                         year_mo = c("2010-10", "2010-10", "2012-08", 
                                     "2012-08", "2014-12", "2014-12"), 
                         responde = c(1, 4, 2, 5, 3, 6))

# Separate
treatments_sep <- separate(treatments, year_mo, c("year", "month"))

# Unite
new_treatments <- unite(treatments_sep, year_mo, year, month, sep = "-")


#######################################
# Limpieza: preparación               #
#######################################

library(lubridate)

ymd("2015-08-25")
ymd("2015 August 25", locale = "English_USA")
mdy("August 25, 2015", locale = "English_USA")
hms("11:17:07")  #periodo tiempo No son horas del día
ymd_hms("2015/08/25 11.33.09 AM")

library(stringr)
str_trim("    this is a test    ")
str_pad("244493", width = 7, side = "left", pad = "0")
names <- c("Sarah", "Tom", "Alice")
str_detect(names, "Alice")
str_replace(names, "Alice", "David")

#NAs
df <- data.frame(A = c(1, NA, 8, NA),
                 B = c(3, NA, 88, 23),
                 C = c(2, 45, 3, 1))

#Detección
is.na(df) #Util en dataset pequeños.
any(is.na(df))
sum(is.na(df))
summary(df)

#Eliminación
df[complete.cases(df), ]
na.omit(df)

#Outliers
set.seed(10)
x <- c(rnorm(30, mean = 15, sd = 5), -5, 28, 35)

boxplot(x, horizontal = T)

df2 <- data.frame(A = rnorm(100, 50, 100),
                 B = c(rnorm(99, 50, 10), 500),
                 C = c(rnorm(99, 50, 10), -1))

summary(df2)
hist(df2$B, 20) #¿Qué le pasa al 500? Es un error de medición, 50
boxplot(df2)
