library("ks")
library("igraph")
library("pracma")
library("parallel")
source("~/Documents/Code/privatestat/statGraph/R/common.R")
source("~/Documents/Code/privatestat/statGraph/R/permanogva.R")


#permanogva

n <- 100 # Number of Samples
niter <- 10
ncores <- 30


ER <- function(n, p, q)
{
  M <- as.matrix(igraph::as_adj(igraph::sample_gnp(n, p)))
  N <- as.matrix(igraph::as_adj(igraph::sample_gnp(n, q)))
  M[upper.tri(M)] <- 0
  N[lower.tri(N)] <- 0
  O <- M + N
  return(O)
}

Thread <- function(thread){
  cat("Starting thread ", thread, "\n")
  p_values <- c()
  
  for(i in 1:niter){
    V1 <- rnorm(n, 0.0, 1.0)
    V2<- rnorm(n, 0.0, 1.0)
    V3 <- rnorm(n, 0.0, 1.0)
    N <- rnorm(n, 0.0, 1.0)
    
    #weight <-  sigmoid(rnorm(n, 0.5, 0.1)) # Covariance does not exist
    Y <- 0.5 * V1 - 0.5 * V2 + N
    P <- sigmoid(Y, a = 1, b = 0)# Non-Linear Covariance exists
    Graphs <- list()
    for(i in 1:n){
      p <- P[i]
      G <- as.matrix(igraph::as_adj(igraph::sample_gnp(1000, p, directed = TRUE)))
      Graphs[[i]] <- G
    }
    Data <- matrix(NA, nrow = n, ncol = 3)
    Data[,1] <- V1
    Data[,2] <- V2
    Data[,3] <- V3
    
    predictor = as.matrix(Data[,c(1, 2)])
    result <- permanogva(Graphs, predictor, maxBoot = 1000, directed=TRUE, npoints = 500, trick = TRUE)
    print(result) 
    p <- result$pvalue
    p_values <- c(p_values, p)
    
    filename = paste("permanogva-er-progress-", thread, ".data", sep="")
    f <- file(filename, "a")
    writeLines(paste("Iteration: ", i, sep=""), f)
    writeLines(paste(p, sep="-"), f)
    close(f)
    
  }
  filename = paste("permanogva-er-core-p-", thread, ".data", sep="")
  write(x=p_values, file=filename)
}

