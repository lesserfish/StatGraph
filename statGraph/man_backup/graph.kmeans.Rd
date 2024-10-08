% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statGraph.R
\name{graph.kmeans}
\alias{graph.kmeans}
\title{K-means for Graphs}
\usage{
graph.kmeans(x, k, nstart = 2)
}
\arguments{
\item{x}{a list containing the graphs or their adjacency matrices to be
clustered.}

\item{k}{an integer specifying the number of clusters.}

\item{nstart}{the number of trials of k-means clusterizations. The algorithm
returns the clusterization with the best silhouette.}
}
\value{
A list with class "statGraph" containing the following components:
\item{method}{a string indicating the used method.}
\item{info}{a string showing details about the method.}
\item{data.name}{a string with the data's name(s).}
\item{cluster}{a vector of the same length of \code{x} containing the
clusterization labels.}
}
\description{
\code{graph.kmeans} clusters graphs following a k-means algorithm based on the
Jensen-Shannon divergence between the spectral densities of the graphs.
}
\examples{
set.seed(42)
g <- list()
for(i in 1:5){
  g[[i]] <- igraph::sample_gnp(30, p=0.2)
}
for(i in 6:10){
  g[[i]] <- igraph::sample_gnp(30, p=0.5)
}
res <- graph.kmeans(g, k=2, nstart=2)
res

}
\references{
MacQueen, James. "Some methods for classification and analysis of
multivariate observations." Proceedings of the fifth Berkeley symposium on
mathematical statistics and probability. Vol. 1. No. 14. 1967.

Lloyd, Stuart. "Least squares quantization in PCM." IEEE transactions on
information theory 28.2 (1982): 129-137.
}
\keyword{k-means}
