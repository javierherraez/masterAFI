library(data.table)
library(igraph)

dd <- fread("masterAFI/18. Analisis de Grafos/01. Redes sociales/ejercicio01/soc-Epinions1.txt")
head(dd)

# a objeto grafo
gg <- graph.data.frame(dd, directed = TRUE)

summary(gg) # identificador DN (dirigido y con nombres) nº nodos y enlaces

vcount(gg) # nº vértices


# componentes conexas (clusters) y tamaño componentes
is.connected(gg, mode = 'strong') # es conexo?
is.connected(gg, mode = 'weak') # es conexo?

ccs <- clusters(gg, mode='strong')

id_compmayor <- order(table(ccs$membership), decreasing = TRUE)[1]
id_compmayor <- which.max(ccs$csize)

vids <- ccs$membership == id_compmayor
vids <- which(ccs$membership == id_compmayor)

g2 <- induced_subgraph(gg, vids = vids)

summary(g2)

# grados vertices

degs <- degree(g2)

hist(degs, breaks = 20)
plot(density(degs))
plot(density(degs), log = 'xy')
plot(density(degs), log = 'xy', xlim=c(5,600), ylim=c(1e-6, 1e-1))

ggrandom <- erdos.renyi.game(vcount(g2), ecount(g2) ,type = 'gnm')

plot(density(degree(ggrandom)))

ll <- layout.lgl(g2)
png("grafo.png", width = 1200, height = 1200, res=120)
plot(g2, layout = ll)
dev.off()



