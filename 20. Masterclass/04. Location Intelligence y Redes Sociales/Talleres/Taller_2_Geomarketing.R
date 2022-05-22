#' ---	
#' title: 'MDS/F 2021-22 - Geomarketing'	
#' author: "Esteban Moro"	
#' date: "5 de Mayo de 2022"	
#' output:	
#'   ioslides_presentation:	
#'     logo: ./img/afi.png	
#'     smaller: yes	
#'     widescreen: yes	
#'     toc: true	
#'     toc_depth: 2	
#'   beamer_presentation: default	
#' ---	
#' 	
#' 	
knitr::opts_chunk$set(cache=T,fig.align='center',message=FALSE,warning=FALSE)	
rm(list = ls());gc()	
setwd("~/Programas/CursoLocationIntelligence/2021/")	
library(ggmap)	
library(leaflet)	
library(lubridate)	
library(dplyr)	
library(raster)	
library(sf)	
#' 	
#' 	
#' ## Objetivo	
#' 	
#' Entender cómo usar los datos geolocalizados para problemas de marketing	
#' 	
#' En especial	
#' 	
#' - Explicar la demografía de los visitantes de cada centro comercial	
#' 	
#' - Usar los datos de movimientos de la gente para caracterizar la atracción de cada centro comercial	
#' 	
#' - Estudiar problemas de logística/cierre centros comerciales	
#' 	
#' 	
#' ## Qué es geomarketing	
#' 	
#' Recordamos: geomarketing es un tipo de "location intelligence" que, utilizando datos geolocalizados de tiendas, clientes y competencia permite:	
#' 	
#' - Optimizar la inversión en acciones de marketing	
#' - Mayor conocimiento del mercado y de su segmentación geográfica	
#' - Optimización de la gestión espacial de recursos (tiendas, rutas, etc.)	
#' - Determinación de áreas de influencia de tiendas/empresas	
#' - Ayudar a las decisiones de inversión, compra o cierre de tiendas/oficias, etc.	
#' 	
#' ## Taller	
#' 	
#' El objetivo de este Taller es encontrar las zonas de atracción en los barrios de Madrid para cada uno de los Ikeas (u otros centros comerciales)	
#' 	
#' Para ello:	
#' 	
#' - Vamos a utilizar datos geolocalizados de Twitter para determinar donde viven los usuarios que van a cada uno de los centros de Ikea de Madrid	
#' 	
#' - Hacemos un modelo (fit) para determinar mediante el modelo de Huff cual es la probabilidad $p_{ij}$ de que un usuario en un barrio $i$ visite el centro de Ikea $j$	
#' 	
#' - Con ese modelo determinamos las zonas de atracción de cada Ikea, asignando cada barrio $i$ a un Ikea $j$ si la probabilidad $p_{ij}$ es mayor de las $p_{i*}$	
#' 	
#' 	
#' ## Taller: Cargamos los datos geolocalizados	
#' 	
#' Vamos a utilizar una base de datos de tweets geolocalizados durante 2 años	
#' 	
#' 	
load("./data/tweets_madrid.RData")	
head(tweets_madrid)	
#' 	
#' 	
#' 	
#' ## Taller: y el mapa de los barrios de Madrid	
#' 	
#' Cargamos los shapefiles de los barrios de Madrid	
#' 	
#' 	
map_barrios <- read_sf("./data/Barrios Madrid/200001469.shp")	
map_barrios$DESBDT <- iconv(map_barrios$DESBDT,from="Windows-1252", to="UTF-8")	
#' 	
#' 	
#' Los transformamos a la proyección WSG84	
#' 	
#' 	
mapb <- st_transform(map_barrios,4326)	
#' 	
#' 	
#' ## Taller: y el mapa de los barrios de Madrid	
#' 	
#' Los visualizamos	
#' 	
leaflet(mapb) %>% addTiles() %>% addPolygons(weight=1)	
#' 	
#' 	
#' 	
#' 	
#' ## Taller: poblacion de los barrios	
#' 	
#' También necesitaremos la población de los barrios de Madrid <br> http://www-2.munimadrid.es/TSE6/control/seleccionDatosBarrio	
#' 	
pob_barrios <- read.csv("./data/Madrid-Poblacion_Barrios.csv",	
                      colClasses=c("character","character","integer",	
                                   "character","character"))	
#' 	
#' Añadimos a la tabla del mapa una columna que sea la población (uniendo por el GEOCODIGO)	
#' 	
mapb <- merge(mapb,pob_barrios,by="GEOCODIGO")	
head(mapb)	
#' 	
#' 	
#' ## Taller: y los datos de los centros comerciales 	
#' 	
#' En concreto los centros comerciales en la comunidad de Madrid (http://www.madrid.org/nomecalles/) 	
#' 	
#' 	
centros <- read.table("./data/centros_comerciales.csv",header=T,stringsAsFactors = F)	
head(centros,2)	
#' 	
#' 	
#' ## Taller: determinación home usuarios	
#' 	
#' Estimamos donde está el domicilio de cada usuario a nivel de barrio: barrio más frecuentado para hacer los tweets.	
#' 	
#' Para ello añadimos una columna `GEOCODIGO` en la tabla de tweets de cada barrio:	
#' 	
#' 	
points_tweets <- st_as_sf(tweets_madrid,coords=c("lon","lat"),crs=4326)	
id_polygon <- st_within(points_tweets,mapb)	
tweets_madrid$GEOCODIGO <- mapb$GEOCODIGO[as.numeric(id_polygon)]	
head(tweets_madrid)	
	
#' 	
#' 	
#' 	
#' ## Taller: determinación home usuarios	
#' 	
#' Si va muy lento podemos utilizar también el paquete `sp`	
#' 	
#' 	
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")	
points_tweets <- cbind(tweets_madrid$lon,tweets_madrid$lat)	
pps1 <- SpatialPoints(points_tweets,proj4str=WGS84)	
oo1 <- over(pps1,as(as_Spatial(mapb),"SpatialPolygons"))	
tweets_madrid$GEOCODIGO <- mapb$GEOCODIGO[oo1]	
head(tweets_madrid)	
#' 	
#' 	
#' O podemos utilizar los tweets que ya están procesados en 	
#' 	
#' 	
load("./data/tweets_madrid_geo.RData")	
#' 	
#' 	
#' 	
#' ## Taller: determinación home usuarios	
#' 	
#' Nos quedamos solo con los tweets que suceden en alguno de los barrios	
#' 	
tws <- tweets_madrid[!is.na(tweets_madrid$GEOCODIGO),]	
#' 	
#' 	
#' Es posible que haya usuarios que han venido a Madrid de vacaciones y hayan hecho muchos tweets. Nos quedamos solo con los tweets de aquellos usuarios que al menos haya estado en Madrid más de 5 meses.	
#' También nos quedamos sólo con aquellos que hayan hecho más de 10 tweets y menos de 10000.	
#' 	
tws$month <- month(tws$time)	
users_table <- tws %>% 	
                group_by(user) %>%	
                summarize(ntot=length(user),nmonths=length(unique(month)))	
users_sel <- users_table$user[users_table$nmonths > 5 & users_table$ntot > 10 & users_table$ntot < 10000]	
tws <- tws[tws$user %in% users_sel,]	
#' 	
#' 	
#' 	
#' ## Taller: determinación home usuarios	
#' 	
#' Finalmente calculamos cual ha sido el barrio más frecuentado y cuánto ha sido frecuentado. Para cada usuario obtenemos cuantos tweets tiene en cada barrio	
#' 	
user_barrio_tweets <- tws %>% group_by(user,GEOCODIGO) %>% summarize(nt = length(user))	
#' 	
#' Ordenamos y calculamos el barrio más visitado	
#' 	
user_barrio_tweets <- user_barrio_tweets[order(user_barrio_tweets$user,user_barrio_tweets$nt,decreasing=T),]	
user_barrio_most <- user_barrio_tweets %>% 	
  group_by(user) %>% 	
  summarize(ntot = sum(nt),	
            nmax=head(nt,1),	
            bmax=head(GEOCODIGO,1),	
            nbarrios=length(unique(GEOCODIGO)))	
#' 	
#' 	
#' ## Taller: determinación home usuarios	
#' 	
#' Solo consideramos aquellos en los que más del 30% de los tweets suceden en el barrio más visitado	
#' 	
user_barrio_most <- filter(user_barrio_most,ntot < 10000 & ntot > 10 & nmax/ntot > 0.3 & nbarrios > 3)	
#' 	
#' ## Taller: determinación home usuarios	
#' 	
#' Vamos a ver cómo se comparan el número de homes con la población de cada barrio	
#' 	
#' 	
par(mar=c(3,3,1,1))	
aa <- user_barrio_most %>% group_by(bmax) %>% summarise(nusers=length(user)) %>%	
  ungroup()	
merge(aa,pob_barrios,by.x="bmax",by.y="GEOCODIGO") %>%	
  ggplot(aes(x=nusers,y=Población)) + geom_point() +	
  xlab("Twitter users") + ylab("Población")	
#' 	
#' 	
#' ## Taller: Estimación de áreas de atracción	
#' 	
#' Estimamos cuantos usuarios han ido a un centro comercial, desde que barrio y a qué distancia. 	
#' 	
#' Vamos a probar con los de Ikea. Con el de Vallecas	
#' 	
centros_sel <- centros[centros$ETIQUETA=="Ikea",]	
points_IKEA <- st_as_sf(centros_sel,coords=c("lon","lat"),crs=4326)	
#' 	
#' Vemos qué usuarios han geolocalizado un tweet a menos de 500 metros del centro de IKEA	
#' 	
points_tweets <- st_as_sf(tws,coords=c("lon","lat"),crs=4326)	
distancia_a_Ikea <- st_distance(points_tweets,points_IKEA)	
users_Ikea <- unique(tws$user[as.numeric(distancia_a_Ikea[,3]) < 500])	
#' 	
#' 	
#' ## Taller: Estimación de áreas de atracción	
#' 	
#' Hacemos una tabla de los barrios de donde vienen esos usuarios y la distancia (al centro del barrio)	
#' 	
users_barrio_Ikea <- user_barrio_most[user_barrio_most$user %in% users_Ikea,]	
agg_barrio_Ikea <- users_barrio_Ikea %>% group_by(bmax) %>% summarise(nn=length(user))	
#' 	
#' 	
#' Juntamos estos datos con el mapa	
#' 	
mapb_Ikea <- mapb	
mapb_Ikea <- merge(mapb_Ikea,agg_barrio_Ikea,by.x="GEOCODIGO",by.y="bmax")	
#' 	
#' 	
#' Pintamos un choropleth con estos datos	
#' 	
#' 	
mapp <- mapb_Ikea[!is.na(mapb_Ikea$nn),]	
pal <- colorNumeric("magma",domain=mapp$nn)	
m <- leaflet(mapp) %>% addTiles() %>%	
  addPolygons(fillColor= ~ pal(nn),	
              weight=1,fillOpacity = 0.8,	
              popup=~paste(DESBDT,"<br> nusers = ",nn,sep=""))	
#' 	
#' ## Taller: Estimación de áreas de atracción	
#' 	
#' Veamos como es este choropleth	
#' 	
m	
#' 	
#' 	
#' 	
#' ## Ejercicio	
#' 	
#' Ejercicio 1: 	
#' 	
#' - ¿Desde qué barrio van más personas al centro de Ikea del norte?	
#' 	
#' - ¿Cuál es la distancia media que recorren los clientes para ir a cada uno de los Ikeas?	
#' 	
#' ## Taller: ley de probabilidad de Huff	
#' 	
#' Finalmente, vamos a modelizar la atracción a cada uno de los centros comerciales (Ikea). Para ello vamos a utilizar la ley de Huff que dice que la probabilidad de que una persona que vive en $i$ visite un centro comercial en $j$ viene dada por 	
#' 	
#' $$	
#' p_{ij} \sim \frac{A_j}{d_{ij}^\beta}	
#' $$	
#' 	
#' donde $A_j$ es lo atractivo que es el centro comercial $j$ (normalmente se toma el tamaño), mientras que $d_{ij}$ es la distancia del cliente $i$ al centro $j$.	
#' 	
#' ## Taller: ley de probabilidad de Huff	
#' 	
#' En nuestro caso vamos a empezar por ver cómo la distancia determina el número de usuarios que va a un centro comercial, mediante la ley	
#' 	
#' $$N_{ij} \sim \frac{1}{d_{ij}^\beta}$$	
#' 	
#' Para ello calculamos primero la distancia del Ikea al centro de cada barrio	
#' 	
cc <- st_centroid(mapp)	
dd <- as.numeric(st_distance(cc,points_IKEA)[,3])	
#' 	
#' 	
#' ## Taller: ley de probabilidad de Huff	
#' 	
#' Hacemos el fit (en logaritmos ya que es una función no lineal)	
#' 	
fit <- lm(log(nn) ~  log(dd), data=mapp)	
summary(fit)	
#' 	
#' 	
#' donde vemos que $\beta \approx$ `r abs(round(coefficients(fit)[2],2))`	
#' 	
#' ## Taller: ley de probabilidad de Huff	
#' 	
#' Pero el numero de personas también depende de la población del barrio, asi que vamos a suponer una ley de la forma 	
#' 	
#' $$N_{ij} \sim \frac{P_i^\alpha}{d_{ij}^\beta}$$	
#' 	
#' con lo que el modelo sería	
#' 	
fit1 <- lm(log(nn) ~  log(Población) + log(dd), data=mapp)	
summary(fit1)	
#' 	
#' 	
#' ## Taller: ley de probabilidad de Huff	
#' 	
#' Dado que el fit es a un número de usuarios (variable discreta), probamos un GLM poissoniano con link logaritmico	
#' 	
#' $$\log (\lambda_{ij})  \sim \alpha log(P_i) - \beta log(d_{ij})$$	
#' 	
fit2 <- glm(nn ~ log(Población) + log(dd),data=mapp,family=poisson(link="log"))	
summary(fit2)	
#' 	
#' 	
#' ## Taller: Estimación de las zonas de atracción	
#' 	
#' Los resultados anteriores nos dan una forma funcional para el número de usuarios del barrio $i$ que visitan el centro comercial $j$: 	
#' 	
#' $$N_{ij} \sim \frac{P_i^\alpha}{d_{ij}^\beta}$$.	
#' 	
#' Vamos a utilizarla para determinar las zonas de atracción suponiendo que esa ley vale para todos los centros comerciales	
#' 	
#' - Definimos una función para el modelo de gravedad de Huff	
#' 	
nij_Huff_model <- function(pts1,pts2,pob,alpha,beta){	
  dd <- st_distance(pts1,pts2)	
  prob_Huff_model <- pob^alpha/as.numeric(dd)^beta 	
  prob_Huff_model	
}	
#' 	
#' 	
#' ## Taller: Estimación de las zonas de atracción	
#' 	
#' - Calculamos $N_{ij}$ para cada barrio y centro de Ikea	
#' 	
coord_barrios <- st_centroid(mapb)	
nij <- data.frame()	
for(i in 1:nrow(points_IKEA)){	
  nij <- rbind(nij,nij_Huff_model(coord_barrios,points_IKEA[i,],	
                                   mapb$Población,0.31,1.44))	
}	
colnames(nij) <- mapb$GEOCODIGO	
head(nij[,1:8])	
#' 	
#' 	
#' ## Taller: Estimación de las zonas de atracción	
#' 	
#' Para cada barrio, nos quedamos con el centro que da más número de visitantes. Eso define la zona de atracción. Y la visualizamos	
#' 	
#' 	
centro_most <- apply(nij,2,which.max)	
mapb$centro_most <- centro_most	
factpal <- colorFactor(topo.colors(3), as.factor(mapb$centro_most))	
leaflet(mapb) %>% addTiles() %>% addPolygons(weight=2,color = ~factpal(as.factor(centro_most)))	
#' 	
#' 	
#' 	
#' ## Ejercicio	
#' 	
#' - Repetir el análisis de las zonas de atracción suponiendo que el IKEA de Alcorcón cierra.	
#' 	
#' - Repetir el análisis para los centros de Leroy Merlin	
