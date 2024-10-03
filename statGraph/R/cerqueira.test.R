#' Cerqueira et al.’s test
#'
#' Given two identically independently distributed (idd) samples of graphs G1 and
#' G2, the test verifies if they have the same distribution by calculating the
#' mean distance D from G1 to G2. The test rejects the null hypothesis if D is
#' greater than the (1-alpha)-quantile of the distribution of the test under the
#' null hypothesis.
#'
#' @param G1 the first iid sample of graphs to be compared. Must be a list of
#' igraph objects.
#'
#' @param G2 the second iid sample of graphs to be compared. Must be a list of
#' igraph objects.
#'
#' @param maxBoot integer indicating the number of bootstrap resamples (default
#' is 300).
#'
#' @return A list with class "htest" containing the following components:
#' \item{statistic}{the W-value of the test.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a string indicating the used method.}
#' \item{data.name}{a string with the data's name(s).}
#'
#' @references
#' Andressa Cerqueira, Daniel Fraiman, Claudia D. Vargas and Florencia Leonardi.
#' "A test of hypotheses for random graph distributions built from EEG data",
#' https://ieeexplore.ieee.org/document/7862892
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#'
#' ## test under H0
#' G1 <- G2 <- list()
#' for(i in 1:10){
#'   G1[[i]] <- igraph::sample_gnp(50, 0.5)
#'   G2[[i]] <- igraph::sample_gnp(50, 0.5)
#' }
#' k1 <- cerqueira.test(G1, G2)
#' k1
#'
#' ## test under H1
#' G1 <- G2 <- list()
#' for(i in 1:10){
#'   G1[[i]] <- igraph::sample_gnp(50, 0.5)
#'   G2[[i]] <- igraph::sample_gnp(50, 0.6)
#' }
#' k2 <- cerqueira.test(G1, G2)
#' k2
#' }
#'
#' @export
cerqueira.test <- function(G1, G2, maxBoot = 300){
  data.name <- paste(deparse(substitute(G1)), "and", deparse(substitute(G2)))
  if(methods::is(G1, "list") && methods::is(G1[[1]], "igraph")){
    G1 <- c.transform(G1)
  }
  else{
    stop("Parameter G1 must be a list of igraph objects.")
  }
  if(methods::is(G2, "list") && methods::is(G2[[1]], "igraph")){
    G2 <- c.transform(G2)
  }
  else{
    stop("Parameter G2 must be a list of igraph objects.")
  }

  D <- c.test(G1,G2)
  test_distribution <- c.sampling.distribution(G1,G2,maxBoot)
  p_val <- mean(test_distribution >= D)

  #htest method
  statistic <- D
  names(statistic) <- "W"
  method <- "Verify if two samples of random graphs were originated from the same probability distribution."
  rval <- list(statistic=statistic, p.value=p_val, method=method, data.name=data.name)
  class(rval) <- "htest"
  return(rval)
}

c.sampling.distribution <- function(g, gp, maxBoot = 300)
{
  m <- nrow(g) + nrow(gp)
  test_distribution = c()
  for(i_per in 1:maxBoot){
    total <- rbind(g, gp)
    ind <- sample(1:m, floor(m / 2), replace=F)
    xa <- total[ind,]
    ya <- total[-ind,]
    test_distribution[i_per] <- c.test(xa,ya)
  }
  return(sort(test_distribution))
}


# Fix input format.
c.transform <- function(g, n = igraph::gorder(g[[1]]))
{
  x <- matrix(0, length(g), n * (n - 1) / 2)
  i <- 1
  for(gr in g){
    aux <- as.matrix(igraph::get.adjacency(gr))
    x[i,] <- aux[upper.tri(aux)]
    i <- i + 1
  }
  return(x)
}


# The test itself.
c.test <- function(g, gp)
{
  wstat <- sum(abs(colMeans(g)-colMeans(gp)))
  return(wstat)
}


