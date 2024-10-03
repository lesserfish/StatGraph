#' Tang hypothesis testing for random graphs.
#'
#' Given two independent finite-dimensional random dot product graphs,
#' \code{tang.test} tests if they have generating latent positions that are drawn
#' from the same distribution.
#'
#' @param G1 the first undirected graph to be compared. Must be an igraph
#' object.
#'
#' @param G2 the second undirected graph to be compared. Must be an igraph
#' object.
#'
#' @param dim dimension of the adjacency spectral embedding.
#'
#' @param sigma a real value indicating the kernel bandwidth. If NULL (default)
#' the bandwidth is calculated by the method.
#'
#' @param maxBoot integer indicating the number of bootstrap resamples
#' (default is 200).
#'
#' @return A list with class "htest" containing the following components:
#' \item{statistic}{the T-value of the test.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a string indicating the used method.}
#' \item{data.name}{a string with the data's name(s).}
#'
#' @references
#' Tang, Minh, et al. "A nonparametric two-sample hypothesis testing problem for
#' random graphs." Bernoulli 23.3 (2017): 1599-1630.
#'
#' Tang, Minh, et al. "A semiparametric two-sample hypothesis testing problem
#' for random graphs." Journal of Computational and Graphical Statistics 26.2
#' (2017): 344-354.
#'
#' @examples
#' set.seed(42)
#'
#' ## test under H0
#' lpvs <- matrix(rnorm(200), 20, 10)
#' lpvs <- apply(lpvs, 2, function(x){ return (abs(x)/sqrt(sum(x^2))) })
#' G1 <- igraph::sample_dot_product(lpvs)
#' G2 <- igraph::sample_dot_product(lpvs)
#' D1 <- tang.test(G1, G2, 5)
#' D1
#'
#' ## test under H1
#' lpvs2 <- matrix(pnorm(200), 20, 10)
#' lpvs2 <- apply(lpvs2, 2, function(x){ return (abs(x)/sqrt(sum(x^2))) })
#' G2 <- suppressWarnings(igraph::sample_dot_product(lpvs2))
#' D2 <- tang.test(G1, G2, 5)
#' D2
#'
#' @export
tang.test <- function(G1, G2, dim, sigma = NULL, maxBoot=200){

  data.name <- paste(deparse(substitute(G1)), "and", deparse(substitute(G2)))

  t.validateInput(G1, G2, dim, maxBoot)
  Xhat1 = t.embed.graph(G1, dim)
  Xhat2 = t.embed.graph(G2, dim)
  if(is.null(sigma)){
    sigma = t.get.sigma(Xhat1, Xhat2)
  }
  test_stat = t.test.stat(Xhat1, Xhat2, sigma)
  test_distribution = t.sampling.distribution(G1, dim, maxBoot)
  p_val = t.p_value(test_stat, test_distribution)

  statistic <- test_stat
  names(statistic) <- "T"
  method <- "Tang hypothesis testing for random graphs"
  rval <- list(statistic=statistic, p.value=p_val, method=method, data.name=data.name)
  class(rval)       <- "htest"
  return(rval)
}

t.test.stat <- function(X, Y, sigma){
  n <- nrow(X)
  m <- nrow(Y)
  tmpXX <- sum(exp(-(as.matrix(stats::dist(X))^2)/(2*sigma^2)))
  tmpYY <- sum(exp(-(as.matrix(stats::dist(Y))^2)/(2*sigma^2)))
  tmpXY <- sum(exp(-(t.rect.dist(X,Y))/(2*sigma^2)))
  tmp <- tmpXX/(n*(n-1)) + tmpYY/(m*(m-1)) - 2*tmpXY/(m*n)
  return((m+n)*tmp)
}


t.embed.graph <- function(g, dim){
  defaults = igraph::arpack_defaults
  defaults$maxiter = .Machine$integer.max
  lpv = igraph::embed_adjacency_matrix(g,dim, options = defaults)$X

  # Fix signs of eigenvectors issue
  for (i in 1:dim){
    if(sign(lpv[1, i]) != 1){
      lpv[, i] = -lpv[, i]
    }
  }
  return(lpv)
}


t.rect.dist <- function(X,Y){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  n <- nrow(X)
  m <- nrow(Y)
  tmp1 <- X%*%t(Y)
  tmp2 <- outer(rep(1, n), rowSums(Y^2))
  tmp3 <- outer(rowSums(X^2), rep(1,m))

  D <- tmp2 - 2*tmp1 + tmp3
  return(D)
}


t.get.sigma <- function(X1, X2){
  v1 = as.vector(stats::dist(X1))
  v2 = as.vector(stats::dist(X2))
  v = base::append(v1, v2)
  sigma = stats::median(v)
  return(sigma)
}


t.sampling.distribution <- function(G1, dim, bootstrap_sample_size){
  Xhat1 = t.embed.graph(G1,dim)
  P = t(Xhat1)
  test_distribution = c()
  i = 1
  while (i <= bootstrap_sample_size){
    tryCatch({
      G_a = suppressWarnings(igraph::sample_dot_product(P))
      G_b = suppressWarnings(igraph::sample_dot_product(P))
      Xhat_a = suppressWarnings(t.embed.graph(G_a, dim))
      Xhat_b = suppressWarnings(t.embed.graph(G_b, dim))
      sigma = t.get.sigma(Xhat_a, Xhat_b)
      ts = t.test.stat(Xhat_a, Xhat_b, sigma)
      test_distribution[i] = ts
      i = i + 1
    }, error=function(e){stop(print(e))})
  }
  test_distribution
}


t.p_value <- function(ts, test_distribution){
  area = sum(test_distribution >= ts) / length(test_distribution)
  return(area)
}


t.validateInput <- function(G1, G2, dim, maxBoot){
  !methods::is(G2, "igraph")
  if(methods::is(G1, "dgCMatrix")){ G1 = igraph::graph_from_adjacency_matrix(G1) }
  if(methods::is(G1, "matrix")){ G1 = igraph::graph_from_adjacency_matrix(G1) }
  if(!methods::is(G1, "igraph")){ stop("Input object 'G1' is not an igraph object.") }
  if(methods::is(G2, "dgCMatrix")){ G2 = igraph::graph_from_adjacency_matrix(G2) }
  if(methods::is(G2, "matrix")){ G2 = igraph::graph_from_adjacency_matrix(G2) }
  if(!methods::is(G2, "igraph")){ stop("Input object 'G2' is not an igraph object.") }
  if(!is.null(dim)){
    if(!methods::is(dim, "numeric") && !is.integer(dim)){ stop("Input 'dim' is not a number.") }
    if(dim%%1 != 0){ stop("Input 'dim' must be an integer.") }
    if(length(dim) > 1){ stop("Input 'dim' has length > 1.") }
    if(dim < 1){ stop("Number of dimensions 'dim' is less than 1.") }
    if(dim >= igraph::gorder(G1) || dim >= igraph::gorder(G2)){
      stop("Num. Embedded dimensions 'dim' is greater or equal than number of vertices.")
    }
  }

  if(!methods::is(maxBoot, "numeric")){
    stop("Input object 'maxBoot' is not a numeric value.")
  } else if(length(maxBoot) != 1){
    stop("Input object 'maxBoot' is not a numeric value.")
  } else {
    if(maxBoot <= 20){
      stop("The size of bootstrap sample is too small. Pick a larger value.")
    }
  }
}

