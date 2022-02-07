#######################################
# Importación: fichero de texto plano #
#######################################

# Con read.table
mun_csv_1 <- read.table("dat/municipios1.csv",
                   header = TRUE, 
                   sep = ",", 
                   stringsAsFactors = FALSE,
                   colClasses = c("character","numeric"))
mun_csv_1 <- read.table("dat/municipios1.csv",
                        header = TRUE, 
                        sep = ",", 
                        stringsAsFactors = FALSE,
                        colClasses = c(id_municipio = "character"))
# Con read.csv
mun_csv_2 <- read.csv("dat/municipios1.csv", stringsAsFactors = FALSE)

# Con read.table
mun_tsv_1 <- read.table("dat/municipios2.tsv",
                   header = TRUE, 
                   sep = "\t", 
                   stringsAsFactors = FALSE,
                   dec = ",",
                   quote = "")

# Con read.delim2
mun_tsv_2 <- read.delim2("dat/municipios2.tsv", stringsAsFactors = FALSE)


# Con readr
library(readr)
mun_csv_3 <- read_delim("dat/municipios1.csv",
                     delim = ",",
                     col_types = "ci") #c: characters, d: double, i: integer, l: logical, _: ignore

mun_tsv_3 <- read_tsv("dat/municipios2.tsv", col_types = "cccccciiddi", locale = locale("es", decimal_mark = ","))


# Con fread
library(data.table)
mun_csv_4 <- fread("dat/municipios1.csv")


#######################################
# Importación: fichero Excel          #
#######################################
library(readxl)
excel_sheets("dat/municipios.xlsx")

mun_xlsx <- read_excel("dat/municipios.xlsx")
