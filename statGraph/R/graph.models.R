# Barabasi-Albert graph
BA <- function(n, ps, M=1, as_matrix=TRUE){
  if(as_matrix == TRUE){
    return (as.matrix(igraph::get.adjacency(igraph::sample_pa(n, power=ps, m=M, directed=FALSE))))
  } else{
    return (igraph::sample_pa(n, power=ps, m=M, directed=FALSE))
  }
}


# Watts-Strogatz graph
WS <- function(n, pr, K=8, as_matrix=TRUE){
  if(as_matrix == TRUE){
    return (as.matrix(igraph::get.adjacency(igraph::sample_smallworld(1, n, K, pr))))
  } else{
    return (igraph::sample_smallworld(1, n, K, pr))
  }
}


# Watts-Strogatz small-world graph
WSfun <- function(K){
  f <- function(n, pr, as_matrix=TRUE){
    WS(n, pr, K=K, as_matrix=as_matrix)
  }
  return(f)
}


# Barabasi-Albert scale-free graph
BAfun <- function(M){
  f <- function(n, ps, as_matrix = TRUE){
    BA(n, ps, M=M, as_matrix = as_matrix)
  }
  return(f)
}

ER <- function(n, p, as_matrix = TRUE){
  if(as_matrix == TRUE){
    return(as.matrix(igraph::get.adjacency(igraph::sample_gnp(n, p))))
  } else {
    return(igraph::sample_gnp(n, p))
  }
}


# Geometric graph
GRG <- function(n, r, as_matrix=TRUE){
  if(as_matrix == TRUE){
    return (as.matrix(igraph::get.adjacency(igraph::sample_grg(n, r))))
  } else{
    return (igraph::sample_grg(n, r))
  }
}


# K-regular graph
KR <- function(n, k){
  return(as.matrix(igraph::get.adjacency(igraph::sample_k_regular(n, k))))
}

