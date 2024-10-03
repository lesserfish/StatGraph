#' Degree-based spectral density
#'
#' \code{get.spectral.density} returns the degree-based spectral density in
#' the interval <\code{from},\code{to}> by using npoints discretization points.
#'
#' @param G The undirected unweighted graph (igraph type) whose spectral
#' density we want to obtain.
#'
#' @param from Lower end of the interval that contain the eigenvalues or
#' smallest eigenvalue of the adjacency matrix of the graph. The smallest
#' eigenvalue is used if the value is not given.
#'
#' @param to  Upper end of the interval that contain the eigenvalues or largest
#' eigenvalue of the adjacency matrix of the graph. The largest eigenvalue is
#' used if the value is not given.
#'
#' @param npoints Number of discretization points of the interval <\code{from},\code{to}>.
#'
#' @param numCores Number of cores to use for parallelization.
#'
#' @return A list with class "statGraph" containing the following components:
#' \item{method}{a string indicating the used method.}
#' \item{info}{a string showing details about the method.}
#' \item{data.name}{a string with the data's name(s).}
#' \item{x}{x-value of the the degree-based spectral density of the graph.}
#' \item{y}{y-value of the the degree-based spectral density of the graph.}
#'
#' @keywords eigenvalue_density
#'
#' @references
#' Newman, M. E. J., Zhang, X., & Nadakuditi, R. R. (2019).
#' Spectra of random networks with arbitrary degrees.
#' Physical Review E, 99(4), 042309.
#'
#' @examples
#' set.seed(42)
#' G <- igraph::sample_smallworld(dim=1, size=100, nei=2, p=0.2)
#'
#' # Obtain the degree-based spectral density
#' density_ <- get.spectral.density(G=G, npoints=80, numCores=1)
#' density_
#'
#' @export
get.spectral.density <- function(G, from = NULL, to = NULL, npoints = 2000, numCores = 1){
  graph <- G
  `%dopar%` <- foreach::`%dopar%`
  # Number of vertices
  n <- igraph::vcount(graph)
  # Adjacency matrix
  A <- NULL
  # If 'from' or 'to' are null, then get the adjacency matrix
  if(is.null(from) || is.null(to)) A <- igraph::as_adjacency_matrix(graph, type="both")
  # Obtain the largest eigenvalue
  if(is.null(to)) to <- rARPACK::eigs_sym(A, k=1)$values[1]
  # Obtain the smallest eigenvalue
  if(is.null(from)) from <- rARPACK::eigs_sym(A, k=1, which="SA")$values[1]
  # Discretizise interval <\code{from},\code{to}> in npoints
  bw <- (to - from) / npoints
  x <- seq(from, to, bw)
  y <- rep(0, length(x))
  # Obtain the degree and excess degree distribution
  deg_prob <- c(igraph::degree_distribution(graph=graph, mode="all"), 0.0)
  k_deg <- seq(1, length(deg_prob)) - 1
  c <- sum(k_deg * deg_prob)
  q_prob <- c()

  for(k in 0:(length(deg_prob) - 1)){
    aux_q <- (k + 1) * deg_prob[k + 1] / c
    q_prob <- c(q_prob, aux_q)
  }
  # Obtain sorted unique degrees of the graph
  all_k <- c(1:length(q_prob))
  valid_idx <- q_prob != 0
  q_prob <- q_prob[valid_idx]
  all_k <- all_k[valid_idx]

  # Obtain the eigenvalue density for each discretized points by using numCores cores.
  cl <- parallel::makePSOCKcluster(numCores)
  doParallel::registerDoParallel(cl)
  i <- NULL
  y <- foreach::foreach(i=1:length(x),.combine = c,.export = c("eigenvalue.probability")) %dopar% {
    z <- x[i] + 0.01*1i
    -Im(eigenvalue.probability(deg_prob, q_prob, all_k, z))
  }

  # close cluster
  parallel::stopCluster(cl)

  method <- "Degree-based spectral density"
  info <- paste("Using",npoints,"discretization points")
  data.name <- deparse(substitute(G))
  output <- list(method=method, info=info, x=x, y=y)
  attr(output, "class") <- "statGraph"
  return(output)
}


