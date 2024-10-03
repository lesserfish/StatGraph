#' Graph spectral entropy
#'
#' \code{graph.entropy} returns the spectral entropy of an undirected graph.
#'
#' @param G the undirected graph (igraph type) or its adjacency matrix. The
#' adjacency matrix of an unweighted graph contains only 0s and 1s, while the
#' weighted graph may have nonnegative real values that correspond to the
#' weights of the edges.
#'
#' @param bandwidth string showing which criterion is used to choose the
#' bandwidth during the spectral density estimation. Choose between the
#' following criteria: "Silverman" (default), "Sturges", "bcv", "ucv" and "SJ".
#' "bcv" is an abbreviation of biased cross-validation, while "ucv" means
#' unbiased cross-validation. "SJ"  implements the methods of Sheather & Jones
#' (1991) to select the bandwidth using pilot estimation of derivatives.
#'
#' @param eigenvalues optional parameter. It contains the eigenvalues of matrix
#' G. Then, if the eigenvalues of matrix G have already been computed, this
#' parameter can be used instead of G. If no value is passed, then the
#' eigenvalues of G will be computed by 'graph.entropy'.
#'
#' @return A list with class "statGraph" containing the following components:
#' \item{method}{a string indicating the used method.}
#' \item{info}{a string showing details about the method.}
#' \item{data.name}{a string with the data's name(s).}
#' \item{entropy}{a real number corresponding to the graph spectral entropy.}
#'
#' @keywords spectral_entropy
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
#' G <- igraph::sample_gnp(n=100, p=0.5)
#' entropy <- graph.entropy(G)
#' entropy
#'
#' @export
graph.entropy <- function(G=NULL, bandwidth="Silverman", eigenvalues=NULL){  
  A <- get.adjacency.matrix(G)

  if(is.null(eigenvalues)){
    f <- spectralDensity(A, bandwidth=bandwidth)
  } else f <- gaussianDensity(eigenvalues, bandwidth=bandwidth)

  if(sum(is.na(f)) > 0) return(NA)
  y <- f$y
  i <- which(y != 0)
  y[i] <- y[i]*log(y[i])

  entropy <- -trapezoidSum(f$x, y)

  method <- "Spectral entropy of a graph"
  info <- "Using a Gaussian kernel to estimate the spectral density"
  data.name <- deparse(substitute(G))
  value <- list(method=method, info=info, data.name=data.name, entropy=entropy)
  attr(value, "class") <- "statGraph"
  return(value)
}

