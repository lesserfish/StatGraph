#' Test for Association / Correlation Between Paired Samples of Graphs
#'
#' \code{graph.cor.test} tests for association between paired samples of graphs,
#' using Spearman's rho correlation coefficient.
#'
#' @param G1 a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @param G2 a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @return A list with class "htest" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a string indicating the used method.}
#' \item{data.name}{a string with the data's name(s).}
#' \item{estimates}{the estimated measure of association 'rho'.}
#'
#' @keywords correlation_coefficient
#'
#' @references
#' Fujita, A., Takahashi, D. Y., Balardin, J. B., Vidal, M. C. and Sato, J. R.
#' (2017) Correlation between graphs with an application to brain network
#' analysis. _Computational Statistics & Data Analysis_ *109*, 76-92.
#'
#' @examples
#' set.seed(1)
#' G1 <- G2 <- list()
#'
#' p <- MASS::mvrnorm(50, mu=c(0,0), Sigma=matrix(c(1, 0.5, 0.5, 1), 2, 2))
#'
#' ma <- max(p)
#' mi <- min(p)
#' p[,1] <- (p[,1] - mi)/(ma - mi)
#' p[,2] <- (p[,2] - mi)/(ma - mi)
#'
#' for (i in 1:50){
#'   G1[[i]] <- igraph::sample_gnp(50, p[i,1])
#'   G2[[i]] <- igraph::sample_gnp(50, p[i,2])
#' }
#' graph.cor.test(G1, G2)
#'
#' @import stats
#' @import MASS
#' @export
graph.cor.test <- function(G1, G2){
  data.name <- paste(deparse(substitute(G1)), "and", deparse(substitute(G2)))
  if(methods::is(G1, "list") && methods::is(G1[[1]], "igraph")){
    G1 <- f.transform(G1)
  }
  if(methods::is(G2, "list") && methods::is(G2[[1]], "igraph")){
    G2 <- f.transform(G2)
  }

  G1.radius <- array(0, length(G1))
  G2.radius <- array(0, length(G2))

  for (i in 1:length(G1)){
    G1.radius[i] <- eigen(G1[[i]], only.values=TRUE, symmetric=TRUE)$values[1]
    G2.radius[i] <- eigen(G2[[i]], only.values=TRUE, symmetric=TRUE)$values[1]
  }

  res <- cor.test(G1.radius, G2.radius, method="spearman")

  statistic         <- res$statistic
  names(statistic)  <- "statistic"
  estimate          <- res$estimate
  names(estimate)   <- "rho"
  method            <- "Association between paired samples of graphs, using Spearman's rho correlation coefficient"
  rval              <- list(statistic=statistic,
                            p.value=res$p.value, method=method,
                            data.name=data.name, estimate=estimate)
  class(rval)       <- "htest"
  return(rval)
}
