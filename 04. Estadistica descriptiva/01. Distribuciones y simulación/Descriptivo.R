# https://librovivodecienciadedatos.ai/analisis-exploratorio-de-datos.html
# install.packages("tidyverse")
# install.packages("funModeling") 
# install.packages("Hmisc")

library(funModeling) 
library(tidyverse) 
library(Hmisc)

aed_basico <- function(data)
{
  glimpse(data)
  status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

##########
#EJEMPLO1#
##########

#Datos heart_disease del paquete funModeling
data1=heart_disease %>% select(age, max_heart_rate, thal, has_heart_disease)
aed_basico(data1) 

#Exporta gráficos al directorio como .jpeg
freq(data1, path_out = ".")

# Analizar los datos 
df_status(heart_disease)
my_data_status=df_status(heart_disease, print_results = F)

# Quitar las variables que tienen un 60% de valores cero
vars_to_remove=filter(my_data_status, p_zeros > 60)  %>% .$variable
vars_to_remove
# Conservar todas las columnas excepto aquellas presentes en el vector 'vars_to_remove'
heart_disease_2=select(heart_disease, -one_of(vars_to_remove))

# Ordenar datos según el porcentaje de ceros
arrange(my_data_status, -p_zeros) %>% select(variable, q_zeros, p_zeros)
# Cantidad total de filas
nrow(heart_disease)
# Cantidad total de columnas
ncol(heart_disease)
# Nombres de columnas
colnames(heart_disease)

# Analisis de variables categóricas
# Distribución de dos variables:
freq(data=heart_disease, input = c('thal','chest_pain'))
freq(data=heart_disease) #Se ejecuta para todas las variables
freq(data=heart_disease$thal, plot = FALSE, na.rm = TRUE) #???xcluye NA
freq(data=heart_disease, path_out='my_folder') #Exporta los gráficos a una carpeta my_folder

# Solo las dos variables que utilizaremos en este ejemplo
heart_disease_3=select(heart_disease, thal, chest_pain)

# Analizar los datos
describe(heart_disease_3)

# Correlacion
correlation_table(data=heart_disease, target="has_heart_disease")

##########
#EJEMPLO2#
##########

library(Hmisc)

# Cargar datos desde el repositorio sin alterar el formato
data_world=read.csv(file = "https://goo.gl/2TrDgN", header = T, stringsAsFactors = F, na.strings = "..")

# Excluir los valores faltantes en Series.Code. Los datos descargados de la página web contienen cuatro líneas con "free-text" en la parte inferior del archivo.
data_world=filter(data_world, Series.Code!="")

# Función que conserva los valores más recientes de cada métrica (para NA). Pensar si es lo adecuado
max_ix<-function(d) 
{
  ix=which(!is.na(d))
  res=ifelse(length(ix)==0, NA, d[max(ix)])
  return(res)
}
data_world$newest_value=apply(data_world[,5:ncol(data_world)], 1, FUN=max_ix)

# Visualizar las primeras tres filas
head(data_world, 3)

# Obtener la lista de descripciones de indicadores.
names=unique(select(data_world, Series.Name, Series.Code))
head(names, 5)
# Convertir algunos indicadores con nombres mas explicativos
df_conv_world=data.frame(
  new_name=c("urban_poverty_headcount", 
             "rural_poverty_headcount", 
             "gini_index", 
             "pop_living_slums",
             "poverty_headcount_1.9"), 
  Series.Code=c("SI.POV.URHC", 
                "SI.POV.RUHC",
                "SI.POV.GINI",
                "EN.POP.SLUM.UR.ZS",
                "SI.POV.DDAY"), 
  stringsAsFactors = F)
# Agregar el nuevo valor del indicador
data_world_2 = left_join(data_world, 
                         df_conv_world, 
                         by="Series.Code", 
                         all.x=T)
data_world_2 = 
  mutate(data_world_2, Series.Code_2=
           ifelse(!is.na(new_name), 
                  as.character(data_world_2$new_name), 
                  data_world_2$Series.Code)
  )
# Para ver que es la variable http://data.worldbank.org/indicator/EN.POP.SLUM.UR.ZS

# El paquete 'reshape2' contiene tanto la función 'dcast' como 'melt'
library(reshape2)
# Explicacion reshape https://seananderson.ca/2013/10/19/reshape/

data_world_wide=dcast(data_world_2, Country.Name  ~ Series.Code_2, value.var = "newest_value")

# Visualizar las primeras tres filas
head(data_world_wide, 3)

#library(Hmisc) # contiene la función `describe`

vars_to_profile=c("gini_index", "poverty_headcount_1.9")
data_subset=select(data_world_wide, one_of(vars_to_profile))

# Utilizar la función `describe` en conjunto de datos completo. 
# Puede ejecutarse con una variable; por ejemplo, describe(data_subset$poverty_headcount_1.9)

describe(data_subset)

#library(funModeling)

# El análisis numérico completo de una función automáticamente excluye las variables no numéricas
profiling_num(data_world_wide)
my_profiling_table=profiling_num(data_world_wide) %>% select(variable, mean, p_01, p_99, range_80)

# Visualizar sólo las primeras tres filas
head(my_profiling_table, 3)
#Analisis de variables numericas con graficos
plot_num(data_world_wide)

##########
#EJEMPLO3#
##########

# Cargar las bibliotecas necesarias
library(funModeling) # contiene datos de heart_disease
library(minerva) # contiene estadístico MIC 
library(ggplot2)
library(dplyr)
library(reshape2) 
library(gridExtra) # nos permite realizar dos gráficos en una fila
options(scipen=999) # desactiva la notación científica
correlation_table(data=heart_disease, target="has_heart_disease")

##########
#EJEMPLO4#
##########

# Leer los datos de Anscombe
# Correlacion y coeficiente de informacion maxima, MIC
# http://www.exploredata.net/
# http://openaccess.uoc.edu/webapps/o2/bitstream/10609/81845/7/apazosrTFM0618memoria.pdf

anscombe_data = 
  read.delim(file="https://goo.gl/mVLz5L", header = T)

# Calcular la correlación (R cuadrado o R2) para 
#cada par, los valores son el mismo: 0.86.
cor_1 = cor(anscombe_data$x1, anscombe_data$y1)
cor_2 = cor(anscombe_data$x2, anscombe_data$y2)
cor_3 = cor(anscombe_data$x3, anscombe_data$y3)
cor_4 = cor(anscombe_data$x4, anscombe_data$y4)

# Definir la función
plot_anscombe <- function(x, y, value, type)
{
  # 'anscombe_data' es una variable global, una mala práctica de programación en este caso
  p=ggplot(anscombe_data, aes_string(x,y))  + 
    geom_smooth(method='lm', fill=NA) + 
    geom_point(aes(colour=factor(1), 
                   fill = factor(1)), 
               shape=21, size = 2
    ) + 
    ylim(2, 13) + 
    xlim(4, 19) + 
    theme_minimal() + 
    theme(legend.position="none") + 
    annotate("text", 
             x = 12, 
             y =4.5, 
             label = 
               sprintf("%s: %s", 
                       type, 
                       round(value,2)
               )
    )  
  
  return(p)
}
library(grid)
library(gridExtra)
# Graficar en una cuadrícula de 2x2
grid.arrange(plot_anscombe("x1", "y1", cor_1, "R2"), 
             plot_anscombe("x2", "y2", cor_2, "R2"), 
             plot_anscombe("x3", "y3", cor_3, "R2"), 
             plot_anscombe("x4", "y4", cor_4, "R2"), 
             ncol=2, 
             nrow=2)

library(minerva)

# Calcular el MIC para cada par de Anscombe
mic_1=mine(anscombe_data$x1, anscombe_data$y1, alpha=0.8)$MIC
mic_2=mine(anscombe_data$x2, anscombe_data$y2, alpha=0.8)$MIC
mic_3=mine(anscombe_data$x3, anscombe_data$y3, alpha=0.8)$MIC
mic_4=mine(anscombe_data$x4, anscombe_data$y4, alpha=0.8)$MIC

# Graficar el MIC en una cuadrícula 2x2 
grid.arrange(plot_anscombe("x1", "y1", mic_1, "MIC"), plot_anscombe("x2", "y2", mic_2,"MIC"), plot_anscombe("x3", "y3", mic_3,"MIC"), plot_anscombe("x4", "y4", mic_4,"MIC"), ncol=2, nrow=2)

# Calcular el MIC para cada par, notese que el objeto "MIC-R2" lleva guion cuando los datos son dos vectores, a diferencia de las ocasiones en las que toma un data frame, que es "MICR2".
mic_r2_1=mine(anscombe_data$x1, anscombe_data$y1, alpha = 0.8)$`MIC-R2`
mic_r2_2=mine(anscombe_data$x2, anscombe_data$y2, alpha = 0.8)$`MIC-R2`
mic_r2_3=mine(anscombe_data$x3, anscombe_data$y3, alpha = 0.8)$`MIC-R2`
mic_r2_4=mine(anscombe_data$x4, anscombe_data$y4, alpha = 0.8)$`MIC-R2`

# Ordenar por mic_r2
df_mic_r2=data.frame(pair=c(1,2,3,4), mic_r2=c(mic_r2_1,mic_r2_2,mic_r2_3,mic_r2_4)) %>% arrange(-mic_r2)
df_mic_r2

##########
#EJEMPLO5#
##########

#Relacion no lineal
x=seq(0, 20, length.out=500)
df_exp=data.frame(x=x, y=dexp(x, rate=0.65))
ggplot(df_exp, aes(x=x, y=y)) + geom_line(color='steelblue') + theme_minimal()

#La posición [1,2] contiene la correlación de ambas variables, excluyendo la medida de correlación de cada variable con respecto a sí misma.

# Calcular la correlación lineal
res_cor_R2=cor(df_exp)[1,2]^2
sprintf("R2: %s", round(res_cor_R2,2))

# Calculamos la métrica MIC
res_mine=mine(df_exp)
sprintf("MIC: %s", res_mine$MIC[1,2])

#Se agrega ruido
df_exp$y_noise_1=jitter(df_exp$y, factor = 1000, amount = NULL)
ggplot(df_exp, aes(x=x, y=y_noise_1)) + 
  geom_line(color='steelblue') + theme_minimal()

# Calcular R cuadrado
res_R2=cor(df_exp)^2
res_R2

library(minerva)
# Calcular MINE
res_mine_2=mine(df_exp)

# Visualizar MIC 
res_mine_2$MIC

# MIC r2: métrica de no linealidad
round(res_mine_2$MICR2, 3)
# Calcular MIC r2 manualmente
round(res_mine_2$MIC-res_R2, 3)

# Crear un ejemplo con datos
df_example=data.frame(x=df_exp$x, 
                      y_exp=df_exp$y, 
                      y_linear=3*df_exp$x+2)

# Obtener métricas de mine 
res_mine_3=mine(df_example)

# Generar etiquetas para visualizar los resultados
results_linear = 
  sprintf("MIC: %s \n MIC-R2 (non-linearity): %s",
          res_mine_3$MIC[1,3],
          round(res_mine_3$MICR2[1,3],2)
  )

results_exp = 
  sprintf("MIC: %s \n MIC-R2 (non-linearity): %s", 
          res_mine_3$MIC[1,2],
          round(res_mine_3$MICR2[1,2],4)
  )

# Graficar los resultados 
# Crear el gráfico de la variable exponencial
p_exp=ggplot(df_example, aes(x=x, y=y_exp)) + 
  geom_line(color='steelblue') + 
  annotate("text", x = 11, y =0.4, label = results_exp) + 
  theme_minimal()

# Crear el gráfico de la variable lineal
p_linear=ggplot(df_example, aes(x=x, y=y_linear)) + 
  geom_line(color='steelblue') + 
  annotate("text", x = 8, y = 55, 
           label = results_linear) + 
  theme_minimal()

grid.arrange(p_exp,p_linear,ncol=2)

##########
#EJEMPLO6#
##########
# No-monotonicidad. MAS

# Crear datos de muestra (simulando una serie temporal)
time_x=sort(runif(n=1000, min=0, max=1))
y_1=4*(time_x-0.5)^2
y_2=4*(time_x-0.5)^3

# Calcular el MAS para ambas series
mas_y1=round(mine(time_x,y_1)$MAS,2)
mas_y2=mine(time_x,y_2)$MAS

# Unir todo
df_mono=data.frame(time_x=time_x, y_1=y_1, y_2=y_2)

# Graficar
label_p_y_1 = 
  sprintf("MAS=%s (goes down \n and up => not-monotonic)", 
          mas_y1)

p_y_1=ggplot(df_mono, aes(x=time_x, y=y_1)) + 
  geom_line(color='steelblue') + 
  theme_minimal()  + 
  annotate("text", x = 0.45, y =0.75, 
           label = label_p_y_1)

label_p_y_2=
  sprintf("MAS=%s (goes up => monotonic)", mas_y2)

p_y_2=ggplot(df_mono, aes(x=time_x, y=y_2)) + 
  geom_line(color='steelblue') + 
  theme_minimal() + 
  annotate("text", x = 0.43, y =0.35, 
           label = label_p_y_2)

grid.arrange(p_y_1,p_y_2,ncol=2)


##########
#EJEMPLO7#
##########

# Leer los datos
df_time_series = 
  read.delim(file="https://goo.gl/QDUjfd")

# Convertirlos a formato largo para poder graficarlos
df_time_series_long=melt(df_time_series, id="time")

# Graficar
plot_time_series = 
  ggplot(data=df_time_series_long,
         aes(x=time, y=value, colour=variable)) +
  geom_line() + 
  theme_minimal() + 
  scale_color_brewer(palette="Set2")

plot_time_series

# Calcular y visualizar los valores de MAS para datos de series temporales
mine_ts=mine(df_time_series)
mine_ts$MAS 

# Visualizar otra vez las 3 series temporales
plot_time_series
# Visualizar los valores de MIC
mine_ts$MIC

# Deformacion dinamica del tiempo
# https://en.wikipedia.org/wiki/Dynamic_time_warping
# http://dtw.r-forge.r-project.org/
# https://izbicki.me/blog/converting-images-into-time-series-for-data-mining.html

##########
#EJEMPLO8#
##########

# Correlacion en variables categoricas
library(caret)

# Seleccionar sólo algunas variables
heart_disease_2 = 
  select(heart_disease, max_heart_rate, oldpeak, 
         thal, chest_pain,exer_angina, has_heart_disease)

# Esta conversión de categórica a numérica es simplemente para tener un gráfico más limpio
heart_disease_2$has_heart_disease=
  ifelse(heart_disease_2$has_heart_disease=="yes", 1, 0)

# Convierte todas las variables categóricas (factor y 
# carácter para R) en variables numéricas

dmy = dummyVars(" ~ .", data = heart_disease_2)

heart_disease_3 = 
  data.frame(predict(dmy, newdata = heart_disease_2))

# Cuidado con missing values 
# Valorar si merece la pena usar = 'pairwise.complete.obs'.` 
# Antes de omitir valores NA hay que analizar su impacto antes, en este caso no es importante. 
heart_disease_4=na.omit(heart_disease_3)

# Calculo del MIC
mine_res_hd=mine(heart_disease_4)

mine_res_hd$MIC[1:5,1:5]


# Biblioteca para graficar esta matriz
library(corrplot) 
# Para usar la paleta de color brewer.pal
library(RColorBrewer) 

# Truco para visualizar el valor máximo de la escala excluyendo la diagonal (var. con respecto a sí misma)
diag(mine_res_hd$MIC)=0

# Gráfico de correlación con círculos. 
corrplot(mine_res_hd$MIC, 
         method="circle",
         col=brewer.pal(n=10, name="PuOr"),
         # Mostrar sólo la diagonal superior
         type="lower", 
         #color, tamaño y rotación de las etiquetas
         tl.col="red",
         tl.cex = 0.9, 
         tl.srt=90, 
         # no visualizar la diagonal,
         # (var con respecto a sí misma)
         diag=FALSE, 
         # aceptar cualquier matriz, mic en este caso
         #(no es un elemento de correlación)
         is.corr = F 
         
)
# Gráfico de correlación con color y correlación MIC
corrplot(mine_res_hd$MIC, 
         method="color",
         type="lower", 
         number.cex=0.7,
         # Agregar coeficiente de correlación
         addCoef.col = "black", 
         tl.col="red", 
         tl.srt=90, 
         tl.cex = 0.9,
         diag=FALSE, 
         is.corr = F 
)

cross_plot(heart_disease, input = "chest_pain", target = "has_heart_disease", plot_type = "percentual")

# Obtener el índice de la variable a predecir: has_heart_disease
target="has_heart_disease"
index_target=grep(target, colnames(heart_disease_4))

# El argumento master toma el número de la columna índice para
# calcular todas las correlaciones
mic_predictive=mine(heart_disease_4, 
                    master = index_target)$MIC

# Crear el data frame que contiene los resultados,
# ordenándolos por correlación decreciente y excluyendo
# la correlación del objetivo con respecto a sí mismo
df_predictive = 
  data.frame(variable=rownames(mic_predictive), 
             mic=mic_predictive[,1], 
             stringsAsFactors = F) %>% 
  arrange(-mic) %>% 
  filter(variable!=target)

# Crear un gráfico que muestre la importancia de las variables basándonos en
# la medición del MIC
ggplot(df_predictive, 
       aes(x=reorder(variable, mic),y=mic, fill=variable)
) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  theme_bw() + 
  xlab("") + 
  ylab("Variable Importance (based on MIC)") + 
  guides(fill=FALSE)


library(infotheo)
library(entropy)

# Discretizar cada variable
heart_disease_4_disc=discretize(heart_disease_4) 

# Calcular la "correlación" basándonos en información mutua
heart_info=mutinformation(heart_disease_4_disc, method= "emp")

# Truco para visualizar el valor máximo de la escala
# excluyendo la diagonal (variable con respecto a sí misma)
diag(heart_info)=0

# Gráfico de correlación con color y correlación con información mutua del paquete Infotheo
corrplot(heart_info, method="color",type="lower", number.cex=0.6,addCoef.col = "black", tl.col="red", tl.srt=90, tl.cex = 0.9, diag=FALSE, is.corr = F)

# Teoria de la informacion
# https://www.youtube.com/watch?v=2s3aJfRr9gE
# http://alex.smola.org/teaching/cmu2013-10-701x/slides/R8-information_theory.pdf
# http://www.scholarpedia.org/article/Mutual_information

          