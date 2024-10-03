#' Multidimensional scaling of graphs
#'
#' \code{graph.mult.scaling} performs multidimensional scaling of graphs. It
#' takes the Jensen-Shannon divergence between graphs (JS) and uses the
#' 'cmdscale' function from the 'stats' package to obtain a set of points such
#' that the distances between the points are similar to JS.
#'
#' @param G a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @param plot logical. If TRUE (default) the points chosen to represent the
#' Jensen-Shannon divergence between graphs are plotted.
#'
#' @param bandwidth string showing which criterion is used to choose the
#' bandwidth during the spectral density estimation. Choose between the
#' following criteria: "Silverman" (default), "Sturges", "bcv", "ucv" and "SJ".
#' "bcv" is an abbreviation of biased cross-validation, while "ucv" means
#' unbiased cross-validation. "SJ"  implements the methods of Sheather & Jones
#' (1991) to select the bandwidth using pilot estimation of derivatives.
#'
#' @param type what type of plot should be drawn. The defaut value is '"n"',
#' which indicates that the points will not be plotted (i. e. only the labels
#' of the graphs will be plotted).
#'
#' @param main title of the plot (default value is "").
#'
#' @param ... additional plotting parameters. See 'plot' function from the
#' 'graphics' package for the complete list.
#'
#' @return A list with class "statGraph" containing the following components:
#' \item{method}{a string indicating the used method.}
#' \item{info}{a string showing details about the method.}
#' \item{data.name}{a string with the data's name(s).}
#' \item{values}{A matrix in which each column corresponds to a coordinate and each
#' row corresponds to a graph (point). Then, each row gives the coordinates of
#' the points chosen to represent the Jensen-Shannon divergence between graphs.}
#'
#' @keywords multidimensional_scaling
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
#' graph.mult.scaling(G)
#'
#' @import graphics
#' @export
graph.mult.scaling <- function(G, plot=TRUE, bandwidth="Silverman", type="n", main="", ...){
  if(methods::is(G, "list") && methods::is(G[[1]], "igraph")){
    G <- f.transform(G)
  }

  f <- nSpectralDensities(G, bandwidth=bandwidth)

  d <- matrix(0, length(G), length(G))
  for (i in 1:(length(G)-1)){
    f1 <- list("G"=f$G, "y"=f$densities[,i])
    for (j in (i+1):length(G)){
      f2 <- list("G"=f$G, "y"=f$densities[,j])
      d[i,j] <- d[j,i] <- sqrt(JS(f1, f2))
    }
  }

  if(is.null(names(G)))
    names <- as.character(1:length(G))
  else
    names <- names(G)
  colnames(d) <- rownames(d) <- names
  fit <- cmdscale(as.dist(d), k=2)

  G <- fit[,1]
  y <- fit[,2]
  names(G) <- names
  names(y) <- names
  if(plot){
    plot(G, y, xlab="G", ylab="y", main=main, type=type, ...)
    text(G, y, labels=names, cex=1)
  }

  method <- "Multidimensional scaling of graphs"
  info <- paste("Using ", bandwidth, "'s criterion to estimate the bandwidth", sep='')
  data.name <- deparse(substitute(G))
  output <- list(method=method, info=info, values=fit)
  attr(output, "class") <- "statGraph"
  return(output)
}


