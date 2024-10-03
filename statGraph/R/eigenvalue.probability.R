#' Degree-based eigenvalue probability
#'
#' \code{eigenvalue.probability} returns the probability of an eigenvalue
#' given the degree and excess degree probability.
#'
#' @param deg_prob The degree probability of the graph.
#'
#' @param q_prob The excess degree probability of the graph.
#'
#' @param all_k  List of sorted unique degrees greater than 1 of the graph.
#'
#' @param z  Complex number whose real part is the eigenvalue whose probability
#' we want to obtain, the imaginary part is a small value (e.g., 1e-3).
#'
#' @param n_iter The maximum number of iterations to perform.
#'
#' @return A complex number whose imaginary part absolute value corresponds to
#' the probability of the given eigenvalue.
#'
#' @keywords eigenvalue_probability
#'
#' @references
#' Newman, M. E. J., Zhang, X., & Nadakuditi, R. R. (2019).
#' Spectra of random networks with arbitrary degrees.
#' Physical Review E, 99(4), 042309.
#'
#' @examples
#' set.seed(42)
#' G <- igraph::sample_smallworld(dim = 1, size = 10, nei = 2, p = 0.2)
#'
#' # Obtain the degree distribution
#' deg_prob <- c(igraph::degree_distribution(graph=G, mode="all"), 0.0)
#' k_deg <- seq(1, length(deg_prob)) - 1
#'
#' # Obtain the excess degree distribution
#' c <- sum(k_deg * deg_prob)
#' q_prob <- c()
#' for(k in 0:(length(deg_prob) - 1)){
#'   aux_q <- (k + 1) * deg_prob[k + 1] / c
#'   q_prob <- c(q_prob, aux_q)
#' }
#'
#' # Obtain the sorted unique degrees greater than 1
#' all_k <- c(1:length(q_prob))
#' valid_idx <- q_prob != 0
#' q_prob <- q_prob[valid_idx]
#' all_k <- all_k[valid_idx]
#'
#' # Obtain the probability of the eigenvalue 0
#' z <- 0 + 0.01 * 1i
#' eigenval_prob <- -Im(eigenvalue.probability(deg_prob, q_prob, all_k, z))
#' eigenval_prob
#'
#' @export
eigenvalue.probability <- function(deg_prob, q_prob, all_k, z, n_iter = 5000){
  h_z   <- 0 + 0i
  eps <- 1e-7
  all_k_mo <- all_k - 1
  while(n_iter > 0){
    new_h_z <- sum(q_prob / (1 - all_k_mo * h_z))
    new_h_z <- new_h_z / z^2
    # replaces H_z using the new value found
    if(abs(h_z - new_h_z) < eps){
      h_z <- new_h_z
      break
    }
    h_z <- new_h_z
    n_iter <- n_iter - 1
  }

  count_z <- 0
  for(k in 1:length(deg_prob)){
    count_z <- count_z + (deg_prob[k]) / (1 - (k - 1) * h_z)
  }
  count_z <- (count_z / z)
  return(count_z)
}


