library(stringr)

# http://r4ds.had.co.nz/strings.html#basic-matches

# Explain why each of these strings don’t match a \: "\", "\\", "\\\".

# If \ is used as an escape character in regular expressions, how do you match a literal \? Well you 
# need to escape it, creating the regular expression \\. To create that regular expression, you need to 
# use a string, which also needs to escape \. That means to match a literal \ you need to write "\\\\" 
# — you need four backslashes to match one!

# How would you match the sequence "'\?
x <- "\"'\\?"
writeLines(x)
str_detect(x, "^\"'\\\\\\?$")

# What patterns will the regular expression \..\..\.. match? How would you represent it as a string?
x <- ".a.b.c"
str_detect(x, "\\..\\..\\..")

x <- "a.b.c."
str_detect(x, "\\..\\..\\..")

# How would you match the literal string "$^$"?
x <- "$^$"
str_detect(x, "^\\$\\^\\$$")

# Given the corpus of common words in stringr::words, create regular expressions that find all words that:
#   - Start with “y”.
#   - End with “x”
#   - Are exactly three letters long. (Don’t cheat by using str_length()!)
#   - Have seven letters or more.
# Since this list is long, you might want to use the match argument to str_view() to show only the matching or non-matching words.
words
str_view(words, "^y", match = T)

str_view(words, "x$", match = T)

str_view(words, "^...$", match = T)
str_view(words, "^.{3}$", match = T)

str_view(words, "^.{7,}$", match = T)

# Create regular expressions to find all words that:
#   - Start with a vowel.
#   - That only contain consonants. (Hint: thinking about matching “not”-vowels.)
#   - End with ed, but not with eed.
#   - End with ing or ise.
# Is “q” always followed by a “u”?
# Create a regular expression that will match telephone numbers as commonly written in your country.
str_view(words, "^[aeiou]", match = T)

str_view(words, "^[^aeiou]*$", match = T)

str_view(words, "[^e]ed$", match = T)

str_view(words, "ing$|ise$", match = T)
str_view(words, "(ing|ise)$", match = T)

str_view(words, "qu", match = T)
str_view(words, "q[^u]", match = T)

str_view("915 200 150", "\\d{3}[:blank:]?\\d{3}[:blank:]?\\d{3}")
str_view("915200150", "\\d{3}[:blank:]?\\d{3}[:blank:]?\\d{3}")
str_view("91 520 01 50", "(\\d+[:blank:]*){9}")
str_view("91 5200150", "(\\d+[:blank:]*){9}")
str_view("915200150", "(\\d+[:blank:]*){9}")
str_view("91520015", "(\\d+[:blank:]*){9}")

# Describe the equivalents of ?, +, * in {m,n} form.
# ? ~ {0, 1}
# + ~ {1,}
# * ~ {0,}
# Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)
#   - ^.*$ ~ Cadena ".*"
#   - "\\{.+\\}" ~ Cualquier cadena entre {}
#   - \d{4}-\d{2}-\d{2} ~ 4 digitos + "-" + 2 dígitos + "-" + 2 dígitos
#   - "\\\\{4}" ~ Cadena "\\\\"
# Create regular expressions to find all words that:
#   - Start with three consonants.
#   - Have three or more vowels in a row.
#   - Have two or more vowel-consonant pairs in a row.
# Solve the beginner regexp crosswords at https://regexcrossword.com/challenges/beginner.
str_view(words, "^[^aeiou]{3}", match = T)

str_view(words, "[aeiou]{3}", match = T)

str_view(words, "([aeiou][^aeiou]){2,}", match = T)

# Describe, in words, what these expressions will match:
#   - (.)\1\1 ~ Carácter que se repite a continuación dos veces
#   - "(.)(.)\\2\\1" ~ Dos caracteres que se repiten pero en orden inverso
#   - (..)\1 ~ Par de caracteres que se repite dos veces
#   - "(.).\\1.\\1" ~ Caracter seguido de punto seguido del mismo carácter seguido de punto seguido del mismo carácter
#   - "(.)(.)(.).*\\3\\2\\1" ~ Tres caracteres que se repiten pero en orden inverso y entre medias puede existir otra cadena
# Construct regular expressions to match words that:
#   - Start and end with the same character.
#   - Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)
#   - Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)

str_view(words, "^(.).*\\1$", match = T)
str_view(words, "(.)(.).*\\1\\2", match = T)
str_view(words, "(.)(.*\\1){2,}", match = T)
