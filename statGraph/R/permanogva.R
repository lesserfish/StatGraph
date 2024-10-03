# References:
# An Multivariate Distance-Based Analytic Framework for Connectome-Wide Association Studies
# Link: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4138049/
# A new method for non-parametric multivariate analysis of variance
# Link: https://ichthyology.usm.edu/courses/multivariate/1786.pdf

RowColumnPermutation <- function(M, i, j){
  N <- M
  N[i, ] <- M[j, ]
  N[j, ] <- M[i, ]
  M <- N
  N[,i] <- M[,j]
  N[,j] <- M[,i]
  return(N)
}

permanova.variability <- function(D, predictor)
{
  N <- nrow(predictor)
  M <- max(ncol(predictor), 2) # M - 1 can't be equal to 0
  
  C <- diag(N) - matrix(1/N, nrow=N, ncol=N) # Centering Matrix
  G <- C %*% D %*% t(C) # Centered Matrix

  X <- predictor 
  H <- X %*% solve(t(X) %*% X) %*% t(X) # Hat Matrix

  F <- (tr(H %*% G) / (M - 1))/(tr((diag(N) - H) %*% G)/(N - M)) # F statistic

  variability <- tr(H %*% G %*% H)/tr(G)
  return(variability)
}

permanogva.variability <- function(G, predictor, npoints = 500, mode = "L2"){
  N <- nrow(predictor)
  M <- ncol(predictor)
  D <- matrix(NA, nrow=N, ncol=N) # Distance Matrix
  SpecDensities <- nSpectralDensities(G, npoints, as.list=TRUE)
  
  for(r in 1:N){
    for(c in 1:N){
      S1 <- list()
      S2 <- list()
      S1$y <- SpecDensities$densities[[r]]
      S2$y <- SpecDensities$densities[[c]]

      d <- Dist(S1, S2, mode)
      D[r, c] <- d
    }
  }

  out <- permanova.variability(D, predictor)
  return(out)
}

permanova <- function(D, predictor, maxBoot = 300, permutations = NA){
  
  N <- nrow(predictor)
  M <- max(ncol(predictor), 2) # M - 1 can't be equal to 0
  
  if(is.na(permutations)){
    permutations = 5*N
  }

  C <- diag(N) - matrix(1/N, nrow=N, ncol=N) # Centering Matrix
  G <- C %*% D %*% t(C) # Centered Matrix

  X <- predictor 
  H <- X %*% solve(t(X) %*% X) %*% t(X) # Hat Matrix

  F <- (tr(H %*% G) / (M - 1))/(tr((diag(N) - H) %*% G)/(N - M)) # F statistic

  PI <- array(0, maxBoot) # Permutation Vector

  for(iter in 1:maxBoot){
    pG <- G
    for(p in 1:permutations){
      shuffle <- sample(1:N, 2, replace = FALSE)
      pG <- RowColumnPermutation(pG, shuffle[1], shuffle[2])
    }
    pF <- (tr(H %*% pG) / (M - 1))/(tr((diag(N) - H) %*% pG)/(N - M)) # F statistic

    PI[iter] <- pF
  }

  pvalue <- sum(PI >= F)/maxBoot

  return(list("F"=F,"pvalue"=pvalue))
}

# Input:
#   G is a list of N elements, such that G[[i]] is a adjacency matrix of a graph
#   predictor is a matrix of N x k elements, with k variables for the N samples of graphs
#   
permanogva <- function(G, predictor, maxBoot = 300, npoints = 500, mode = "L2", permutations = NA, directed=FALSE, trick = FALSE){
  N <- nrow(predictor)
  M <- ncol(predictor)
  D <- matrix(NA, nrow=N, ncol=N) # Distance Matrix
  SpecDensities <- c()
  if(trick){
    SpecDensities <- nSpectralDensitiesP(G, npoints = npoints, directed = directed)
  }
  else
  {
    SpecDensities <- nSpectralDensities(G, npoints = npoints, directed = directed)
  }
  
  for(r in 1:N){
    for(c in 1:N){
      S1 <- list()
      S2 <- list()
      S1$y <- SpecDensities$densities[r, , ]
      S2$y <- SpecDensities$densities[c, , ]

      d <- Dist(S1, S2, mode)
      D[r, c] <- d
    }
  }


  out <- permanova(D, predictor, maxBoot, permutations)
  return(out)
}
tr <- function(M){
  t <- sum(diag(M), na.rm = TRUE)
  return(t)
}
