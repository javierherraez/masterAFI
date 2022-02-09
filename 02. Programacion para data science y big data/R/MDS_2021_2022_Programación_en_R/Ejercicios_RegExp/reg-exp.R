library(stringr)

# http://stringr.tidyverse.org/articles/regular-expressions.html
# http://r4ds.had.co.nz/strings.html#basic-matches

# Las expresiones regulares sirven para detectar/extraer/reemplazar patrones
# de una cadena de caracteres

dias.semana <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

# Báscics
?str_extract
?str_view
?str_detect

str_extract(dias.semana, "es")
str_extract(dias.semana, regex("es"))

str_view(dias.semana, "es")
str_view(dias.semana, regex("es"))

str_detect(dias.semana, "es")
str_detect(dias.semana, regex("es"))


los.lunes <- c("lunes", "Lunes", "LUNES")
str_detect(los.lunes, "lunes")
str_detect(los.lunes, regex("lunes", ignore_case = TRUE))

# Cualquier carácter "."
str_view(dias.semana, ".vier.")
str_view(dias.semana, ".er.")

# ¿Cómo escapar el carácter "."?
puntos <- c("abc", "c.d", "efg")
str_detect(puntos, "\\.")

# ¿Y qué pasa con el carácter "\"?
barras <- c("abc", "c\\d", "efg")
writeLines(barras)
str_detect(barras, "\\\\")

# Comienzo de cadena
str_view(dias.semana, "^m")

# Final de cadena
str_view(dias.semana, "es$")

# Regla nemotécnica: if you begin with power (^), you end up with money ($).
str_detect("$^$", "\\$\\^\\$")

# Caracteres especiales: dígitos \d y \D
str_extract_all("30 de noviembre de 2017", "\\d")[[1]]
str_extract_all("30 de noviembre de 2017", "\\D")[[1]]

# Caracteres especiales: espacios \s y \S
str_replace_all("30 de noviembre de 2017", "\\s", "_")[[1]]
str_replace_all("30 de noviembre de 2017", "\\S", "_")[[1]]

# Caracteres especiales: alfanuméricos (caracteres de palabras entendidos como una letra o número) \w y \W
str_extract_all("30 de noviembre de 2017", "\\w")[[1]]
str_replace_all("30 de noviembre de 2017", "\\W", "_")[[1]]

# Caracteres especiales: transiciones entre alfanuméricos (caracteres de palabras entendidos como una letra o número) \b y \B
str_replace_all("30 de noviembre de 2017", "\\b", "_")[[1]]
str_replace_all("30 de noviembre de 2017", "\\B", "_")[[1]]

# Clases/Grupos propios
str_view_all(dias.semana, "[aeiou]")
str_view_all(dias.semana, "[^aeiou]")
str_view(dias.semana, ".[eé]r.")

# Clases pre-built
# [:punct:]: punctuation.
# [:alpha:]: letters.
# [:lower:]: lowercase letters.
# [:upper:]: upperclass letters.
# [:digit:]: digits.
# [:xdigit:]: hex digits.
# [:alnum:]: letters and numbers.
# [:cntrl:]: control characters.
# [:graph:]: letters, numbers, and punctuation.
# [:print:]: letters, numbers, punctuation, and whitespace.
# [:space:]: space characters (basically equivalent to \s).
# [:blank:]: space and tab.

str_view_all("Strings are not glamorous, high-profile components of R, 
             but they do play a big role in many data cleaning and preparation tasks.", "[:punct:]")

# Repeticiones: 0 o 1 vez
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view_all(x, "CC?")

# Repeticiones: 1 o más veces
str_view_all(x, "CC+")
str_view_all(x, "C+")

# Repeticiones: 0 o más veces
str_view_all(x, "C[LX]*")

y <- "1888 is the longest year in Roman numerals: MDCCCXLLLXVIII"
str_view_all(y, "C[LX]*")

str_view_all(x, "CL*X*")
str_view_all(y, "CL*X*")

# Repeticiones: n veces
str_view_all(x, "C{2}")

# Repeticiones: n o más veces
str_view(x, "C{2,}")

# Repeticiones: entre n y m veces
str_view(x, "C{2,3}")

# Curiosidad: “greedy” vs "lazy"
str_view(x, "C{2,3}")
str_view(x, "C{2,3}?")
str_view(x, 'C[LX]+')
str_view(x, 'C[LX]+?')

# Agrupar y referenciar cada grupo
str_replace_all(dias.semana, "(.u.)[:alpha:]*", "\\1rnes")

fruit
str_view(fruit, "(..)\\1", match = TRUE)
str_view(fruit, "(..)\\1")

str_replace_all(fruit, "(..)\\1", "\\1--")
