#' ---
#' title: "Sistemas de Recomendación"
#' author: "Jorge Ayuso"
#' date: "`r stringi::stri_trans_totitle(format(Sys.Date(),'%B %Y'))`"
#' output: 
#'  html_document:
#'    theme: cosmo
#'    highlight: tango
#'    css: img/base.css
#' ---

#+ include=FALSE
rm(list = ls());gc()
library(magrittr)
knitr::opts_chunk$set(cache = TRUE,message = FALSE,warning = FALSE,echo = TRUE,fig.align = "center")

#' ## Introducción
#' De manera general un sistema de recomendación es un algoritmo para recomendar a usuarios/clientes ciertos items 
#' (películas, productos de nuestra tienda, anuncios...). Con ello se consigue que el usuario tenga una mejor experiencia
#' y tarde menos en encontrar lo que necesita ... Y por otro lado hay más posibilidades que acabe
#' consumiendo/comprando nuestros productos ofertados.
#' 
#' ## Sistemas de recomendación clásicos
#' 
#' Existen multitud de sistemas de recomendaciones: estanterías en librerías, lineales de supermercado,
#' los mejores diez restaurantes de Madrid... 
#' 
#' Todos estos son sistemas no personalizados, es decir las recomendaciones son iguales para
#' todos los usuarios, y no necesitamos ninguna información de los usuarios. Este tipo de
#' recomendaciones suele ser simples pero de manera rápida nos ayuda a ordenar los items de una 
#' manera
#' eficiente.
#' 
#' Algunos ejemplos de estos sistemas son:
#' 
#' * Los más comprados/vistos.
#' * Los mejor puntuados.
#' * Valoración de expertos: Críticas de cines.
#' * Puntuaciones normalizadas: Muchas veces las métricas clásicas como "el mejor puntuado" puede tener grandes
#' sesgos, ya que puede haberlo puntuado poca gente o tener puntuaciones extremas.
#' * Reglas de asociación
#' 
#' ### Rating de IMDb y la media bayesiana
#' 
#' <center>
#' <img width="200" src="img/imdb.jpg"></img>   
#' </center> 
#' <br>
#' 
#' En algún momento todos hemos acabado consultando IMDb. En esta web además de una gran
#' base de datos de películas, actores, directores... encontramos un raking de las películas
#' mejor valoradas. En la entrada de la Wikipedia encontramos cómo realizan este ranking:
#' https://en.wikipedia.org/wiki/IMDb#Rankings.
#' 
#' Veamos con detalle cómo realizan el ranking y qué tiene de particular:
#' 
#' $$
#' {\large
#' W = \frac{Rv + Cm}{v+m}
#' }
#' $$
#' donde,
#' 
#' * $W$:&nbsp; Weighted rating.
#' * $R$:&nbsp; Average for the movie as a number from 0 to 10 (mean) = (Rating).
#' * $v$:&nbsp; Number of votes for the movie = (votes).
#' * $m$:&nbsp; Minimum votes required to be listed in the Top 250 (currently 25,000).
#' * $C$:&nbsp; The mean vote across the whole report (currently 7.0).
#' 
#' Con esta fórmula se intenta mitigar el problema de puntuaciones muy altas para películas con pocos votos.
#' Es claro que es más fácil tener un 10 si solo te ha votado una persona que tener un 10 si te han votado 1.000.
#' 
#' De esta manera se intenta reducir la credibilidad que le damos a una puntuación si la gente que ha votado ha sido poca,
#' esto es la base de la [estadística bayesiana](https://en.wikipedia.org/wiki/Bayesian_inference) donde si
#' tenemos poca muestra la credibilidad es baja y usamos una
#' distribución "aprior" que en el caso de IMDb sería $C$ la media de los votos.
#' 
#' 

#' 
#' ## Sistemas de recomendación personalizados
#' 
#' Es claro que nuestros clientes/usuarios son diferentes. Cada uno tenemos unos gustos y unas 
#' preferencias, si podemos utilizar toda la información que tengamos de nuestros usuarios 
#' podemos construir recomendaciones personalizadas a cada uno y de este modo conseguir
#' mayor calidad en estas.
#' 
#' Una de las empresas que más ha investigado en los sistemas de recomendación es Netflix y el concurso que
#' protagonizo: 
#' 
#' 
#' <center>
#' <img style="background-color:#B9090B;" width="220" src="img/netflix.gif"></img>   
#' http://www.netflixprize.com
#' </center> 
#' <br>
#' 
#' Aunque poco a poco todas las industrizan han implementado sistemas de recomendación: Amazón, Google, El Corte Inglés o 
#' Movistar+:
#'
#' <center>
#' https://comunidad.movistar.es/t5/Blog-Movisfera/Funcionalidad-Movistar-Para-m%C3%AD-sabe-qu%C3%A9-series-y-pel%C3%ADculas-te/ba-p/3202033
#' </center>
#' 
#' Existen varios tipos de estrategias y algoritmos en los sistemas de recomendación
#' unos de los más usados son los **filtros colaborativos**.
#' 
#' En los [**filtrados colaborativos**](https://en.wikipedia.org/wiki/Collaborative_filtering) usamos la
#' información que nos da nuestros propios usuarios para inferir y recomendar a todos ellos. Una de las
#' ventajas de estos algoritmos es que solo necesitamos la relación de nuestros usuarios y
#' los items a recomendar.
#' 
#' Normalmente se suele usar una notación matricial:    
#' 
#' 
#' $$
#' {\large 
#' M_{(n,p)} =
#' \begin{pmatrix}
#' m_{11} &  m_{12}  & \ldots & m_{1p}\\
#' m_{21}  &  m_{22} & \ldots & m_{2p}\\
#' \vdots & \vdots & \ddots & \vdots\\
#' m_{n1}  &   m_{n2}       &\ldots & m_{np}
#' \end{pmatrix}
#' }
#' $$
#' 
#' Donde,
#' 
#' * $n$: Número total de usuarios.
#' * $p$: Número total de items.
#' * $m_{ij}$: Relación entre el usuario $i$ y el item $j$.
#' A esta relación se le suele denotar como el rating usuario/item (cuantas veces lo ha consumido, valoración dada por el usuario,...)
#' 
#' En el siguiente gráfico podemos ver un ejemplo intuitivo de filtro
#'  colaborativo y la matriz definida:
#' 
#' <center>
#' <img  src="img/450px-Collaborative_filtering.gif"></img>   
#' </center> 
#' 
#' Es claro que no todos los usuarios han tenido contacto con todos los items y por lo tanto la 
#' matriz $M$ tiene muchas entradas vacías (tenemos una matriz *sparse*).
#' Así que podemos entender el problema de recomendación
#' como estimar esos huecos vacíos y predecir de algún  modo qué rating tendrá cada
#' usuario en todos los items y de este modo recomendar los items que 
#' predecimos van a ser más afines.
#' 
#' Para hacer esto existen varias estrategias y algortimos. Uno de ellos es usar la estrategia
#' del vecino más próximo (o *K-nearest neighbors* o *KNN*) donde la idea es
#' encontrar en nuestra base de datos (la matriz $M$) gente "parecida"
#' y recomendar items que gustan a esas "almas gemelas".
#' 
#' Otro de los algoritmos y que ha demostrado tener mejores resultados (ver [Matrix Factorization Techniques for Recommender Systems
#' ](http://dl.acm.org/citation.cfm?id=1608614)) que el *KNN*
#' son los basados en factorización de matrices.
#' 
#' 
#' <br>
#' 
#' #### El proyecto MovieLens
#' 
#' <center>
#' <img style="background-color:#f06624;" src="img/movielens-logo-white.svg"></img>   
#' https://movielens.org
#' </center> <br>
#' 
#' El proyecto [MovieLens](https://movielens.org/) es una base de datos 
#' de películas y puntuaciones de usuarios. Su fin es puramente académico y es mantenida por
#' "GroupLens, a research lab at the University of Minnesota". Toda la base de datos está
#' disponible para uso no comercial y la podemos descargar desde
#' [aquí](http://grouplens.org/datasets/movielens/).
#' 
#' Vamos a trabajar con estos datos y el algoritmo de factorización *ALS* 
#' (*Alternating Least Squares*) que viene implementado en Spark.  
#' 
#' <br>
#' 
#' #### Notas para Google Colab
#' 
#' Si se quiere ejecutar los notebooks de la sesión desde Google Colab, habrá
#' que ejecutar los siguientes comandos para configurarlo.
#' 
#' 
#' ```bash
#' !curl -L -o configurar_colab.ipynb 'https://docs.google.com/uc?export=download&id=1a5QHNtrjfbUwBtPuM6Qz5Kx8BRPDHhz0'
#' run configurar_colab.ipynb
#' ```
#' 