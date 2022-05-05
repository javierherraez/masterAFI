#' ---
#' title: "Reglas de Asociación"
#' author: "Jorge Ayuso"
#' date: "`r stringi::stri_trans_totitle(format(Sys.Date(),'%B %Y'))`"
#' output: 
#'  html_document:
#'    theme: cosmo
#'    highlight: tango
#'    css: ../img/base.css
#' ---

#+ include=FALSE
library(magrittr)
knitr::opts_chunk$set(cache = FALSE,message = FALSE,warning = FALSE,echo = TRUE,fig.align = "center")

#' 
#' ### El carrito de la compra
#' 
#' El "carrito de la compra" se conoce como un ejemplo típico de **reglas de asociación**.
#' 
#' Las [Reglas de asociación](https://en.wikipedia.org/wiki/Association_rule_learning) se utilizan para 
#' encontrar patrones habituales en ciertos datos. Por ejemplo qué tipos de productos se suelen 
#' comprar juntos: 
#' 
#' * Si compra $A$ suele comprar $B$ ($A\to B$).
#' * El que suele comprar $A$ y $C$ suele comprar $D$ ($\{A,C\}\to D$).
#' * ...
#' 
#' Esta técnica es usada en los lineales del supermercado para saber qué tipo de productos poner juntos,
#' por ejemplo los frutos secos cerca de las bebidas.
#' 
#' Queremos que las reglas que encontremos sean significativas, para ello se suele
#'  utilizar como medida el
#' **soporte** y la **confianza**:
#' 
#' $$
#' {\large
#' \begin{eqnarray*}
#' supp(A) &=& \frac{|A|}{|D|} \\[1.5em]
#' conf(A\to B) &=& \frac{supp(A \cup B)}{supp(A)} 
#' \end{eqnarray*}
#' }
#' $$
#' 
#' Donde $D$ es el número total de transacciones.
#' 
#' De manera intuitiva, el soporte es la proporción de veces que
#' aparece el conjunto $A$ en el total de transacciones. Y la confianza es la proporción de veces
#' que aparece $A$ y $B$ cuando aparece $A$.
#'  
#' Con R usaremos el paquete `arules` para realizar este tipo de recomendaciones.
#' 
#' Introducimos un ejemplo sencillo (para más detalle consultar http://www.salemmarafi.com/code/market-basket-analysis-with-r/):
#' 
#' 

library(arules)
library(arulesViz)
library(datasets)

data(Groceries)
help("Groceries")

#' Vamos a usar los datos de `Groceries`, estos datos contiene las transacciones reales
#' de un supermercado durante un mes 
Groceries
class(Groceries)
str(Groceries)

#' Los datos están en un objeto de tipo `transactions`. Dentro de este objeto hay una matriz con
#' las transacciones:
#' 
Groceries@data[1:10,1:20]

#' El paquete `arules` nos proporciona algunas funciones para explorar los datos:

itemFrequencyPlot(Groceries,topN = 20,type = "absolute",col = "#A7DBD8")

#' Usamos la función `apriori` para calcular las reglas. Normalmente se fija a
#' mano el soporte y la confianza que queremos, para ello se suele ser exigentes
#' (para conseguir pocas reglas) y poco a poco relajamos las condiciones hasta conseguir un número
#' adecuado de reglas en función de nuestros propósitos.
#' 

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
rules

#' Para ver las 5 primeras reglas:
inspect(rules[1:5])

#' También podemos hacer un resumen del objeto:
summary(rules)


#' Podemos realizar muchas más cosas con la función `apriori`, alguna de ellas

rules <- apriori(
                data = Groceries, parameter = list(supp = 0.001,conf = 0.15,minlen = 2), 
                appearance = list(default = "rhs",lhs = "whole milk")
               )

inspect(rules)

#' Suele ser conveniente ordenar las reglas por algún parámetro, por ejemplo por la confianza:

rules <- sort(rules, decreasing = TRUE,by = "confidence")
inspect(rules)

#' Para terminar, con el paquete `arulesViz` podemos dibujar las reglas de varias maneras

plot(rules)
plot(rules,method = "graph")




#' ### **Ejercicio**
#' Vamos a familiarizarnos con la técnica de reglas de asociación usando un dataset "clásico".
#' Usaremos los datos del Tinanic `help("Titanic")`, que tenemos preparados en `datos/titanic_raw.csv`.
#' 
#' 
#' Usar dicha técnica para estudiar qué características tenian los supervivientes (lado de derecho `rhs`).
#'
#' &nbsp;
#' 
#'   

