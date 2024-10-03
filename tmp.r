source("statGraph/R/common.R")
source("statGraph/R/anogva.R")
library(igraph)
library(ks)

directed=TRUE

p <- c()

for(i in 1:100){
K <- list()
H <- list()
for(i in seq(1, 10)){
    k <- as.matrix(igraph::as_adjacency_matrix(igraph::sample_gnp(200, 0.5, directed=directed)))
    h <- as.matrix(igraph::as_adjacency_matrix(igraph::sample_gnp(200, 0.5, directed=directed)))
    K[[i]] <- k
    H[[i]] <- h
}

G <- c(K, H)
labels <- c(rep(1, 10), rep(2, 10))


out <- anogva(G, labels, npoints=64, directed=TRUE, distance="KL")
print(out)
p <- c(p, out$pvalue)
}

hist(p)
