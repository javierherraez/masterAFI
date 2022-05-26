library(data.table)
library(igraph)

dd <- fread("masterAFI/18. Analisis de Grafos/01. Redes sociales/ejercicio01/soc-Epinions1.txt")
head(dd)

# a objeto grafo
gg <- graph.data.frame(dd, directed = TRUE) # dataframe a objeto grafo

summary(gg) # identificador DN (dirigido y con nombres) nº nodos y enlaces

vcount(gg) # nº vértices
ecount(gg)


# componentes conexas (clusters) y tamaño componentes
is.connected(gg, mode = 'strong') # es conexo?
is.connected(gg, mode = 'weak') # es conexo?

ccs <- clusters(gg, mode='strong') #componentes conexas

id_compmayor <- which.max(ccs$csize) # mayor componente conexa

vids <- ccs$membership == id_compmayor
vids <- which(ccs$membership == id_compmayor) #nodos de la comp conexa mayor

g2 <- induced_subgraph(gg, vids = vids ) #subbgrafo solamente con estos nodos

summary(g2)


## dibujitos
# grados vertices

# degs <- degree(g2)
# 
# hist(degs, breaks = 20)
# plot(density(degs))
# plot(density(degs), log = 'xy')
# plot(density(degs), log = 'xy', xlim=c(5,600), ylim=c(1e-6, 1e-1))
# 
# ggrandom <- erdos.renyi.game(vcount(g2), ecount(g2) ,type = 'gnm')
# 
# plot(density(degree(ggrandom)))
# 
# ll <- layout.lgl(g2)
# png("grafo.png", width = 1200, height = 1200, res=120)
# plot(g2, layout = ll)
# dev.off()
##

# probar un par de algoritmos de deteccion de comunidades
#### infomap, fastgreedy, label propagation, walktrap

# estos dos no suelen ser los mejores; mejores son walktrap o infomap
comms <- fastgreedy.community(as.undirected(g2))
comms2 <- label.propagation.community(as.undirected(g2))

comms$membership

# ver la modularidad de los resultados para elegir el mejor
modularity(comms)
modularity(comms2) #tiene tan poca modularidad porque ha metido 
                   # en una comunidad ha casi todos los nodos
                   # ver con el table

table(comms2$membership)

# ver cuanto solapan los resultados de los algoritmos

compare(comms, comms2, method='nmi') # es parecido a verlo como un porcentaje

# pintar grafo

ll <- layout_with_lgl(g2, maxiter=30)
palcol <- rainbow(n=max(comms$membership))
png("grafo.png", width = 1200, height = 1200, res=120)
plot(g2, layout = ll, vertex.label='', edge.arrow.size=0,
     vertex.size=log(degree(g2)), vertex.color=palcol[comms$membership])
dev.off()


# crossing(comms, g2) sirve para saber si el enlace esta dentro de una comunidad
# para darle más pesoa a estas aristas en un plot:
# wws <- ifelese(crossing(comms, g2), 1, 500)
# layout_with_fr(g2, maxiter=30, weights=wws)


