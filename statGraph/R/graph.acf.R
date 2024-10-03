#' Auto Correlation Function Estimation for Graphs
#'
#' The function \code{graph.acf} computes estimates of the autocorrelation
#' function for graphs.
#'
#' @param G a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @param plot logical. If TRUE (default) the graph.acf is plotted.
#'
#' @return An object of class acf.
#'
#' @keywords autocorrelation
#'
#' @references
#' Fujita, A., Takahashi, D. Y., Balardin, J. B., Vidal, M. C. and Sato, J. R.
#' (2017) Correlation between graphs with an application to brain network
#' analysis. _Computational Statistics & Data Analysis_ *109*, 76-92.
#'
#' @examples
#' set.seed(1)
#' G <- list()
#' p <- array(0, 100)
#' p[1:3] <- rnorm(3)
#' for (t in 4:100){
#'   p[t] <- 0.5*p[t-3] + rnorm(1)
#' }
#' ma <- max(p)
#' mi <- min(p)
#' p <- (p - mi)/(ma-mi)
#' for (t in 1:100){
#'   G[[t]] <- igraph::sample_gnp(100, p[t])
#' }
#' graph.acf(G, plot=TRUE)
#'
#' @import stats
#' @export
graph.acf <- function(G, plot=TRUE){
  if(methods::is(G, "list") && methods::is(G[[1]], "igraph")){
    G <- f.transform(G)
  }
  G.radius <- array(0, length(G))
  for (t in 1:length(G)){
    G.radius[t] <- eigen(G[[t]], only.values=TRUE, symmetric=TRUE)$values[1]
  }
  res <- acf(G.radius, plot=plot)
  return(res)
}

