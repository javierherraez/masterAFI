###################################
# tidyr                           #
###################################
library(tidyr)
library(reshape2)

# Arregla el siguiente data frame
people_1 <- data.frame(name = c("Jake", "Alice", "Tim", "Denise"), 
                       age = c(34, 55, 76, 19), 
                       brown = c(0, 0, 1, 0), 
                       blue = c(0, 1, 0, 0),
                       other = c(1, 0, 0, 1),
                       height = c(6.1, 5.9, 5.7, 5.1))

people_1_long_1 <- gather(people_1, eye_color, eye_color_value, brown, blue, other)
people_1_long_1 <- people_1_long_1[people_1_long_1$eye_color_value == 1, c("name", "age", "eye_color", "height")]

people_1_long_2 <- pivot_longer(people_1, cols = c("brown", "blue", "other"), names_to = "eye_color", values_to = "eye_color_value")
people_1_long_2 <- people_1_long_2[people_1_long_2$eye_color_value == 1, c("name", "age", "eye_color", "height")]

people_1_long_3 <- melt(people_1, id.vars = c("name", "age", "height"), variable.name = "eye_color", value.name = "eye_color_value")
people_1_long_3 <- people_1_long_3[people_1_long_3$eye_color_value == 1, c("name", "age", "eye_color", "height")]


# Arregla el siguiente data frame
people_2 <- data.frame(name = c("Jake", "Jake", "Jake", "Alice", "Alice", "Alice"), 
                       measurement = c("n_dog", "n_cats", "n_birds", "n_dog", "n_cats", "n_birds"), 
                       value = c(1, 0, 1, 1, 2, 0))

people_2_wide_1 <- spread(people_2, measurement, value)

people_2_wide_2 <- pivot_wider(people_2, names_from = measurement, values_from = value)

people_2_wide_3 <- dcast(people_2, name ~ measurement, value.var = "value")


# Arregla el siguiente data frame
people_3 <- data.frame(name = c("Jake", "Alice", "Tim", "Denise"), 
                       sex_age = c("M:34", "F:55", "M:76", "F:19"), 
                       eye_color = c("other", "blue", "brown", "other"), 
                       height = c(6.1, 5.9, 5.7, 5.1))

people_3 <- separate(people_3, sex_age, c("sex", "age"), sep=":")


# Consigue el data frame original del último arreglo
people_3 <- unite(people_3, sex_age, sex, age, sep=":")
