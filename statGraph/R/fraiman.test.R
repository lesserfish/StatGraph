#' Fraiman’s test
#'
#' Given a list of graphs, the test verifies if all the subpopulations have the
#' same mean network, under the alternative that at least one subpopulation has
#' a different mean network.
#'
#' @param G the undirected graphs to be compared. Must be a list of lists of
#' igraph objects or a list of lists of adjacency matrices.
#'
#' @param maxBoot integer indicating the number of bootstrap resamples
#' (default is 300).
#'
#' @return A list with class "htest" containing the following components:
#' \item{statistic}{the T-value of the test.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a string indicating the used method.}
#' \item{data.name}{a string with the data's name(s).}
#'
#' @references
#' Fraiman, Daniel, and Ricardo Fraiman. "An ANOVA approach for statistical
#' comparisons of brain networks",
#' https://www.nature.com/articles/s41598-018-23152-5
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#'
#' ## test under H0
#' a <- b <- G <- list()
#' for(i in 1:10){
#'   a[[i]] <- igraph::sample_gnp(50, 0.5)
#'   b[[i]] <- igraph::sample_gnp(50, 0.5)
#' }
#' G <- list(a,b)
#' k1 <- fraiman.test(G)
#' k1
#'
#' ## test under H1
#' a <- b <- G <- list()
#' for(i in 1:10){
#'   a[[i]] <- igraph::sample_gnp(50, 0.5)
#'   b[[i]] <- igraph::sample_gnp(50, 0.6)
#' }
#' G <- list(a,b)
#' k2 <- fraiman.test(G)
#' k2
#' }
#'
#' @export
fraiman.test <- function(G, maxBoot = 300){

  data.name <- deparse(substitute(G))
  if(methods::is(G, "list") && methods::is(G[[1]], "list") && methods::is(G[[1]][[1]], "igraph"))
    G <- f.transform(G)
  if(!methods::is(G, "list") || !methods::is(G[[1]], "list") || !methods::is(G[[1]][[1]], "matrix"))
    stop(paste("You must pass a list of lists of igraphs or a list of lists of matrices."))

  D <- f.test(G)
  test_distribution <- f.sampling.distribution(G, maxBoot)
  p_val <- mean(test_distribution <= D)

  #htest method
  statistic <- D
  names(statistic) <- "T"
  method <- "Test for network differences between groups with an analysis of variance test (ANOVA)"
  rval <- list(statistic=statistic, p.value=p_val, method=method, data.name=data.name)
  class(rval) <- "htest"
  return(rval)
}

f.test <- function(g){
  a <- 1
  m <- length(g)
  l <- unlist(lapply(g, length))
  g <- lapply(g, f.upper)

  M <- f.calcM(g)

  G <- list()
  for(i in 1:length(g)) G <- append(G, g[[i]])

  sumDG <- rep(0, length(M))
  for(i in 1:length(M)){
    for(j in 1:length(G)){
      sumDG[i] <- sumDG[i] + sum(abs(G[[j]] - M[[i]]))
    }
    sumDG[i] <- sumDG[i] / length(G)
  }

  sumDGi <- rep(0, length(M))

  for(i in 1:length(M)){
    for(j in 1:length(g[[i]])){
      sumDGi[i] <- sumDGi[i] + sum(abs(g[[i]][[j]] - M[[i]]))
    }
    sumDGi[i] <- sumDGi[i] / length(g[[i]])
  }

  t1 <- (l / (l - 1)) * sumDGi
  t2 <- (sum(l) / (sum(l) - 1)) * sumDG
  t <- (sqrt(m) / a) * sum(sqrt(l) * (t1 - t2))

  return(t)
}


# Boostrap for the test.
f.sampling.distribution <- function(g, maxBoot = 300)
{
  G <- list()
  n <- length(g)
  for(i in 1:n) G <- append(G, g[[i]])
  m <- length(G)

  dist.boot = c()
  for (i_per in 1:maxBoot){
    G1 <- sample(G, m, replace=F)
    if(n == 2){
      l <- list(G1[1:floor(m / 2)], G1[(floor(m / 2) + 1):m])
    }
    else{
      l <- list(G1[1:floor(m / 3)], G1[(floor(m / 3) + 1):(2 * floor(m / 3))], G1[((2 * floor(m / 3)) + 1):m])
    }
    dist.boot[i_per] <- f.test(l)
  }
  return(dist.boot)
}


# Functions to speed calculations using R builtins.
f.upper <- function(x) lapply(x, function(s){
  s2 <- s
  s2[lower.tri(s2)] <- 0
  eval.parent(substitute(s <- s2))
})


f.add <- function(x){ list(Reduce("+", x), length(x)) }


f.div <- function(x){ x[[1]] / x[[2]] }


f.calcM <- function(x){ mapply(f.div, mapply(f.add, x, SIMPLIFY=F), SIMPLIFY=F)}


