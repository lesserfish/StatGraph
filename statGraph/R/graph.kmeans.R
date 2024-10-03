#' K-means for Graphs
#'
#' \code{graph.kmeans} clusters graphs following a k-means algorithm based on the
#' Jensen-Shannon divergence between the spectral densities of the graphs.
#'
#' @param x a list containing the graphs or their adjacency matrices to be
#' clustered.
#'
#' @param k an integer specifying the number of clusters.
#'
#' @param nstart the number of trials of k-means clusterizations. The algorithm
#' returns the clusterization with the best silhouette.
#'
#'
#' @return A list with class "statGraph" containing the following components:
#' \item{method}{a string indicating the used method.}
#' \item{info}{a string showing details about the method.}
#' \item{data.name}{a string with the data's name(s).}
#' \item{cluster}{a vector of the same length of \code{x} containing the
#' clusterization labels.}
#'
#' @keywords k-means
#'
#' @references
#' MacQueen, James. "Some methods for classification and analysis of
#' multivariate observations." Proceedings of the fifth Berkeley symposium on
#' mathematical statistics and probability. Vol. 1. No. 14. 1967.
#'
#' Lloyd, Stuart. "Least squares quantization in PCM." IEEE transactions on
#' information theory 28.2 (1982): 129-137.
#'
#' @examples
#' set.seed(42)
#' g <- list()
#' for(i in 1:5){
#'   g[[i]] <- igraph::sample_gnp(30, p=0.2)
#' }
#' for(i in 6:10){
#'   g[[i]] <- igraph::sample_gnp(30, p=0.5)
#' }
#' res <- graph.kmeans(g, k=2, nstart=2)
#' res
#'
#' @export
graph.kmeans <- function(x, k, nstart=2){

  data.name <- deparse(substitute(x))
  if(methods::is(x, "list") && methods::is(x[[1]], "igraph")) x <- f.transform(x)

  sil <- -1
  num.graphs <- length(x)
  tmp <- spectral_density(x)
  spectral.density <- tmp$spectral.density

  if(k > nstart) nstart <- k

  for (ns in 1:nstart){
    # Random initialization of the clusters
    label <- sample(seq(1:k), num.graphs, replace=TRUE)
    converged <- FALSE
    while(converged == FALSE){
      centroid <- matrix(0, k, 512)
      for (j in 1:k){
        for (i in 1:512){
          centroid[j, i] <- mean(spectral.density[which(label == j), i])
        }
        centroid[j,] <- centroid[j,] / trapezoidSum(tmp$x, centroid[j,])
      }

      distance <- matrix(0, num.graphs, k)
      for (j in 1:k){
        tmp1 <- list()
        tmp1$y <- centroid[j,]
        tmp1$x <- tmp$x
        for(i in 1:num.graphs){
          tmp2 <- list()
          tmp2$y <- spectral.density[i,]
          tmp2$x <- tmp$x
          distance[i,j] <- sqrt(JS(tmp1, tmp2))
        }
      }

      label.new <- array(0, num.graphs)
      for(i in 1:num.graphs){
        label.new[i] <- which(distance[i,] == min(distance[i,]))[1]
      }
      i <- 1
      while(i<=k){
        if(length(which(label.new == i)) != 0){
          i <- i + 1
        }
        else { # There is an empty cluster
          size.cluster <- array(0, k)
          for(j in 1:k){
            size.cluster[j] <- length(which(label.new == j))
          }
          largest.cluster <- which(size.cluster == max(size.cluster))
          item <- which(
            distance[, largest.cluster] == max(distance[which(label.new == largest.cluster), largest.cluster])
          )
          label.new[item] <- i
          i <- 1
        }
      }

      if(length(which(label == label.new)) == num.graphs){
        converged <- TRUE
        sil.new <- mean(cluster::silhouette(label, distance_matrix(tmp))[, 3])
        if(sil.new > sil){
          sil <- sil.new
          label.final <- label
        }
      }
      label <- label.new
    }
  }

  method <- "K-means for Graphs"
  info <- "Clustering the graphs following a k-means algorithm"
  value <- list(method=method, info=info, data.name=data.name, cluster=label.final)
  attr(value, "class") <- "statGraph"
  return(value)
}

spectral_density <- function(x){
  num.graphs <- length(x)
  spectrum <- list()

  spectrum[[1]] <- eigen(x[[1]])$values
  max.value <- max(spectrum[[1]])
  min.value <- min(spectrum[[1]])
  for(i in 2:num.graphs){
    spectrum[[i]] <- eigen(x[[i]])$values
    if(max(spectrum[[i]]) > max.value){
      max.value <- max(spectrum[[i]])
    }
    if(min(spectrum[[i]]) < min.value){
      min.value <- min(spectrum[[i]])
    }
  }

  spectral.density <- matrix(0, num.graphs, 512)
  for (i in 1:num.graphs){
    bw = SturgesBandwidth(spectrum[[i]])
    tmp <- density(spectrum[[i]], bw=bw, from=min.value, to=max.value)
    area <- trapezoidSum(tmp$x, tmp$y)
    spectral.density[i,] <- tmp$y/area ## normalize to AUC == 1
  }
  res <- list()
  res$spectral.density <- spectral.density
  res$x <- tmp$x
  return(res)

}


distance_matrix <- function(x){
  num.graphs <- nrow(x$spectral.density)
  distance <- matrix(0, num.graphs, num.graphs)
  for (i in 1:(num.graphs-1)){
    for (j in (i+1):num.graphs){
      tmp1 <- list()
      tmp1$y <- x$spectral.density[i,]
      tmp1$x <- x$x
      tmp2 <- list()
      tmp2$y <- x$spectral.density[j,]
      tmp2$x <- x$x
      distance[i,j] <- distance[j,i] <- sqrt(JS(tmp1, tmp2))
    }
  }
  return(distance)
}


SturgesBandwidth <- function(x){
  n <- length(x)
  # Sturges' criterion
  nbins <- ceiling(log2(n) + 1)
  return(abs(max(x) - min(x))/nbins)
}
