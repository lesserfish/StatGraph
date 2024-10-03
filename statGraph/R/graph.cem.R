#' Clustering Expectation-Maximization for Graphs (graph.cem)
#'
#' \code{graph.cem} clusters graphs following an expectation-maximization algorithm based
#' on the Kullback-Leibler divergence between the spectral densities of the
#' graph and of the random graph model.
#'
#' @param g a list containing the graphs or their adjacency matrices to be
#' clustered.
#'
#' @param  model a string that indicates one of the following random graph
#' models: "ER" (Erdos-Renyi random graph), "GRG" (geometric random graph), "KR"
#' (k regular graph), "WS" (Watts-Strogatz model), and "BA" (Barabasi-Albert
#' model).
#'
#' @param k an integer specifying the number of clusters.
#'
#' @param max_iter the maximum number of expectation-maximization steps to execute.
#'
#' @param ncores the number of cores to be used for the parallel processing. The
#' default value is 1.
#'
#' @param bandwidth string showing which criterion is used to choose the
#' bandwidth during the spectral density estimation. Choose between the
#' following criteria: "Silverman" (default), "Sturges", "bcv", "ucv" and "SJ".
#' "bcv" is an abbreviation of biased cross-validation, while "ucv" means
#' unbiased cross-validation. "SJ"  implements the methods of Sheather & Jones
#' (1991) to select the bandwidth using pilot estimation of derivatives.
#'
#' @param parameters a list with the range where the parameters will be estimated. If
#' nothing is passed default values are used for each model.
#'
#' @return A list with class "statGraph" containing the following components:
#' \item{method}{a string indicating the used method.}
#' \item{info}{a string showing details about the method.}
#' \item{data.name}{a string with the data's name(s).}
#' \item{cluster}{a vector of the same length of \code{g} containing the clusterization
#' labels.}
#' \item{parameters}{a vector containing the estimated parameters for the groups.
#' It has the length equals to \code{k}.}
#'
#' @keywords graph.cem
#'
#' @references
#' Celeux, Gilles, and Gerard Govaert. "Gaussian parsimonious clustering
#' models." Pattern recognition 28.5 (1995): 781-793.
#'
#' Sheather, S. J. and Jones, M. C. (1991). A reliable data-based bandwidth
#' selection method for kernel density estimation.
#' _Journal of the Royal Statistical Society series B_, 53, 683-690.
#' http://www.jstor.org/stable/2345597.
#'
#' @examples
#'  set.seed(42)
#'  g <- list()
#'  for(i in 1:2){
#'    g[[i]] <- igraph::sample_gnp(n=10, p=0.5)
#'  }
#'  for(i in 3:4){
#'    g[[i]] <- igraph::sample_gnp(n=10, p=1)
#'  }
#'  res <- graph.cem(g, model="ER", k=2, max_iter=1, ncores=1)
#'  res
#' @export
graph.cem <- function(g, model, k, max_iter = 10, ncores=1, bandwidth="Sturges", parameters=NULL){

  data.name <- deparse(substitute(g))

  if(methods::is(g, "list") && methods::is(g[[1]], "igraph")){
    g <- f.transform(g)
  }
  `%dopar%` <- foreach::`%dopar%`
  `%:%` <- foreach::`%:%`

  cl <- parallel::makePSOCKcluster(ncores)
  doParallel::registerDoParallel(cl)

  tau <- matrix(0, nrow = k, ncol = length(g))
  kl <- matrix(0, nrow = k, ncol = length(g))

  prevlik <- 0
  lik <- 1
  count <- 0
  prevlabels <- array(0, length(g))
  vertices <- nrow(g[[1]])
  labels <- array(0, length(g))
  g_GIC <- array(0, length(g))
  p <- array(0, k)

  if(is.null(parameters)){
    if(model == "ER"){
      parameters <- seq(0.1, 1, 0.01)
    }
    if(model == "GRG"){
      parameters <- seq(0.1, sqrt(2), 0.01)
    }
    if(model == "WS"){
      parameters <- seq(0.01, 1, 0.01)
    }
    if(model == "KR"){
      parameters <- as.integer(seq(2, 10, 1))
    }
    if(model == "BA"){
      parameters <- seq(0.01, 4, 0.01)
    }
  }
  
  # Pre-processing of the graph spectra
  eigenvalues <- list()
  eigenvalues <- foreach::foreach(j = 1:length(g)) %dopar% {
    as.numeric(eigen(g[[j]], only.values = TRUE, symmetric=TRUE)$values) / sqrt(vertices)
  }

  p_graph <- array(0, length(g))
  list_functions = c(
    "GIC", "matchFunction", "ER", "KR", "WS", "BA",
    "GRG", "WSfun", "BAfun", "modelSpectralDensity",
    "nDensities", "gaussianDensity", "kernelBandwidth",
    "trapezoidSum", "KL", "L2", "get.adjacency.matrix",
    "parameter.estimator.erdos.renyi", "GIC.string.or.function"
  )

  # Parameter estimation
  ret <- foreach::foreach(i = 1:length(g), .export=c("graph.param.estimator", list_functions)) %dopar% {
    graph.param.estimator(
      g[[i]], model=model, parameters=parameters, bandwidth=bandwidth,
      eigenvalues=eigenvalues[[i]], eps=0.01
    )
  }

  for(i in 1:length(g)){
    p_graph[i] <- ret[[i]]$p
    g_GIC[i] <- ret[[i]]$KLD
  }

  # Initialize cluster parameters
  p_uniq <- unique(p_graph)
  for(i in 1:k){
    p[i] <- quantile(p_uniq, i / (k + 1))
    # The KR parameter needs to be even
    if(model == "KR") p[i] <- round(p[i])
  }

  converged <- 0
  count <- 0
  while(!converged){
    kl <- foreach::foreach(
      i = 1:k, .combine=cbind, .export=list_functions) %:% foreach::foreach(
        j = 1:length(g), .combine=c,.export=list_functions) %dopar% {
          GIC(g[[j]], model, p[i], bandwidth=bandwidth, eigenvalues=eigenvalues[[j]], dist="KL")$value
    }
    kl <- t(kl)
    kl[which(kl == Inf)] <- max(kl[which(kl < Inf)])
    kl[which(kl == 0)] <- 0.000000001

    for(i in 1:length(g)){
      tau[,i] <- (1 / kl[, i]) / sum(1 / kl[, i])
    }
    for(i in 1:length(g)){
      labels[i] <- which(tau[, i] == max(tau[, i]))
    }

    #Check if there is an empty group
    for(i in 1:k){
      if(length(which(labels == i)) == 0) labels[which(tau[i,] == max(tau[i,]))] <- i
    }

    # Estimates the value of p for the models to maximize tae
    for(i in 1:k){
      p[i] <- sum(p_graph[which(labels == i)]) / length(which(labels == i))
      if(model == "KR") p[i] <- round(p[i])
    }

    prevlik <- lik
    lik <- sum(tau * kl)
    count <- count + 1
    if(count > max_iter) converged = 1

    if((prevlik != 0 && prevlik / lik > 0.99 && prevlik / lik < 1.01)) converged <- 1
    prevlabels <- labels
  }

  ret <- list("cluster"=labels, "parameters"=p)

  # close cluster
  parallel::stopCluster(cl)

  method <- "Clustering Expectation-Maximization for Graphs "
  info <- paste("Using ",bandwidth,"'s criterion to estimate the bandwidth",sep='')
  output <- list(method=method, info=info, data.name=data.name, cluster=ret$cluster, parameters=ret$parameters)
  attr(output, "class") <- "statGraph"
  return(output)
}

