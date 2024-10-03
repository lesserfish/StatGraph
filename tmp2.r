library("igraph")
library("ks")
source("statGraph/R/common.R")
source("statGraph/R/anogva.R")

#anogva

ER <- function(n, p)
{
    M <- as.matrix(igraph::as_adj(igraph::sample_gnp(n, p, directed = FALSE)))
    return(M)
}

p_values <- c()
for(i in 1:100){
    ngraphs <- 10
    MatricesA <- list()
    MatricesB <- list()
    MatricesC <- list()

    for(i in 1:ngraphs){
        G <- ER(100, 0.1)
        H <- ER(100, 0.1)
        I <- ER(100, 0.1)
        MatricesA[[i]] <- G
        MatricesB[[i]] <- H
        MatricesC[[i]] <- I
    }

    G <- c(MatricesA, MatricesB, MatricesC)
    label <- c(rep(1,10),rep(2,10),rep(3,10))
    result <- anogva(G, label, maxBoot=100, directed=FALSE, distance = "KL", npoints=64)
    print(result)
    p_values <- c(p_values, result$p.value)
}

hist(p_values)

