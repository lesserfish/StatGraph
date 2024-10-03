#' Hierarchical cluster analysis on a list of graphs.
#'
#' Given a list of graphs, \code{graph.hclust} builds a hierarchy of clusters
#' according to the Jensen-Shannon divergence between graphs.
#'
#' @param G a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @param k the number of clusters.
#'
#' @param method the agglomeration method to be used. This should be (an
#' unambiguous abbreviation of) one of '"ward.D"', '"ward.D2"', '"single"',
#' '"complete"', '"average"' (= UPGMA), '"mcquitty"' (= WPGMA), '"median"'
#' (= WPGMC) or '"centroid"' (= UPGMC).
#'
#' @param bandwidth string showing which criterion is used to choose the
#' bandwidth during the spectral density estimation. Choose between the
#' following criteria: "Silverman" (default), "Sturges", "bcv", "ucv" and "SJ".
#' "bcv" is an abbreviation of biased cross-validation, while "ucv" means
#' unbiased cross-validation. "SJ"  implements the methods of Sheather & Jones
#' (1991) to select the bandwidth using pilot estimation of derivatives.
#'
#' @return A list containing:
#' \item{hclust}{an object of class *hclust* which describes the tree produced
#' by the clustering process.}
#' \item{cluster}{the clustering labels for each graph.}
#'
#' @keywords clustering
#'
#' @references
#' Takahashi, D. Y., Sato, J. R., Ferreira, C. E. and Fujita A. (2012)
#' Discriminating Different Classes of Biological Networks by Analyzing the
#' Graph Spectra  Distribution. _PLoS ONE_, *7*, e49949.
#' doi:10.1371/journal.pone.0049949.
#'
#' Silverman, B. W. (1986) _Density Estimation_.  London: Chapman and Hall.
#'
#' Sturges, H. A. The Choice of a Class Interval. _J. Am. Statist. Assoc._,
#' *21*, 65-66.
#'
#' Sheather, S. J. and Jones, M. C. (1991). A reliable data-based bandwidth
#' selection method for kernel density estimation.
#' _Journal of the Royal Statistical Society series B_, 53, 683-690.
#' http://www.jstor.org/stable/2345597.
#'
#' @examples
#' set.seed(1)
#' G <- list()
#' for (i in 1:5){
#'   G[[i]] <- igraph::sample_gnp(50, 0.5)
#' }
#' for (i in 6:10){
#'   G[[i]] <- igraph::sample_smallworld(1, 50, 8, 0.2)
#' }
#' for (i in 11:15){
#'   G[[i]] <- igraph::sample_pa(50, power = 1, directed = FALSE)
#' }
#' graph.hclust(G, 3)
#'
#' @import stats
#'
#' @export
graph.hclust <- function(G, k, method="complete", bandwidth="Silverman"){

  if(methods::is(G, "list") && methods::is(G[[1]], "igraph")){
    G <- f.transform(G)
  }
  f <- nSpectralDensities(G, bandwidth=bandwidth)

  d <- matrix(0, length(G), length(G))
  for (i in 1:(length(G)-1)){
    f1 <- list("G"=f$G, "y"=f$densities[,i])
    for (j in (i+1) : length(G)){
      f2 <- list("G"=f$G, "y"=f$densities[,j])
      d[i,j] <- d[j,i] <- sqrt(JS(f1, f2))
    }
  }
  tmp <- hclust(as.dist(d), method=method)

  res <- list()
  res$hclust <- tmp
  res$cluster <- cutree(tmp, k)
  return(res)
}

