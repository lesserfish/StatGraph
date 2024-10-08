#' Test for the Jensen-Shannon divergence between graphs
#'
#' \code{takahashi.test} tests whether two sets of graphs were generated by the same
#' random graph model.
#' This bootstrap test is based on the Jensen-Shannon (JS) divergence between
#' graphs.
#'
#' Given two lists of graphs, 'G1' and 'G2', 'takahashi.test' tests H0: "JS
#' divergence between 'G1' and 'G2' is 0" against H1: "JS divergence between
#' 'G1' and 'G2' is larger than 0".
#'
#' @param G1 a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @param G2 a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @param maxBoot integer indicating the number of bootstrap resamplings.
#'
#' @param bandwidth string showing which criterion is used to choose the
#' bandwidth during the spectral density estimation. Choose between the
#' following criteria: "Silverman" (default), "Sturges", "bcv", "ucv" and "SJ".
#' "bcv" is an abbreviation of biased cross-validation, while "ucv" means
#' unbiased cross-validation. "SJ"  implements the methods of Sheather & Jones
#' (1991) to select the bandwidth using pilot estimation of derivatives.
#'
#' @return A list with class "htest" containing the following components:
#' \item{statistic}{the value of the Jensen-Shannon divergence (JSD) between 'G1' and 'G2'.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a string indicating the used method.}
#' \item{data.name}{a string with the data's name(s).}
#'
#' @keywords graph_comparison
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
#' G1 <- G2 <- list()
#' for (i in 1:20){
#'   G1[[i]] <- igraph::sample_gnp(n=50, p=0.5)
#'   G2[[i]] <- igraph::sample_gnp(n=50, p=0.51)
#' }
#' result <- takahashi.test(G1, G2, maxBoot=100)
#' result
#'
#' @export
takahashi.test <- function(G1, G2, maxBoot=1000, bandwidth="Silverman", directed=FALSE, distance="KL", from=NULL, to=NULL, npoints=1024){
    data.name <- paste(deparse(substitute(G1)), "and", deparse(substitute(G2)))

    if(methods::is(G1, "list") && methods::is(G1[[1]], "igraph")) G1 <- f.transform(G1)
    if(methods::is(G2, "list") && methods::is(G2[[1]], "igraph")) G2 <- f.transform(G2)

    if(directed){
        adjacencyMatrices <- append(G1, G2)
        labels <- c(rep(0, length(G1)), rep(1, length(G2)))

        f <- nSpectralDensities(adjacencyMatrices, bandwidth=bandwidth, directed=TRUE, from=from, to=to, npoints=npoints)
        densities <- f$densities
        x <- f$x

        # R is stupid and will drop dimensions if one of the groups has only one element
        y1 <- apply(densities[labels == 0, , , drop=FALSE], c(2), mean)
        y2 <- apply(densities[labels == 1, , , drop=FALSE], c(2), mean)

        n1 <- length(which(labels==0))
        n2 <- length(which(labels==1))

        results <- vector(length=maxBoot)
        ngraphs <- length(adjacencyMatrices)
        result <- JS(list("x"=x, "y"=y1), list("x"=x, "y"=y2), distance=distance)
        
        for (i in 1:maxBoot){
            b1 <- sample(1:ngraphs, n1, replace=TRUE)
            b2 <- sample(1:ngraphs, n2, replace=TRUE)
            
            # R is stupid and will drop dimensions if one of the groups has only one element
            y1 <- apply(densities[b1, , , drop=FALSE], c(2), mean)
            y2 <- apply(densities[b2, , , drop=FALSE], c(2), mean)

            results[i] <- JS(list("x"=x, "y"=y1), list("x"=x, "y"=y2), distance=distance)
        }
        pvalue <- (sum(results >= result))/maxBoot

        method <- "Jensen-Shannon divergence between graphs"
        statistic <- result
        names(statistic) <- "JSD"
        rval <- list(statistic=statistic, p.value=pvalue, method=method, data.name=data.name)
        class(rval) <- "htest"
        return(rval)
    }
    else{
        adjacencyMatrices <- append(G1, G2)
        labels <- c(rep(0, length(G1)), rep(1, length(G2)))

        f <- nSpectralDensities(adjacencyMatrices, bandwidth=bandwidth, directed=FALSE, from=from, to=to, npoints=npoints)
        densities <- f$densities
        x <- f$x

        # R is stupid and will drop dimensions if one of the groups has only one element
        y1 <- apply(densities[labels == 0, , drop=FALSE], c(2), mean)
        y2 <- apply(densities[labels == 1, , drop=FALSE], c(2), mean)

        n1 <- length(which(labels==0))
        n2 <- length(which(labels==1))

        results <- vector(length=maxBoot)
        ngraphs <- length(adjacencyMatrices)
        result <- JS(list("x"=x, "y"=y1), list("x"=x, "y"=y2), distance=distance)
        
        for (i in 1:maxBoot){
            b1 <- sample(1:ngraphs, n1, replace=TRUE)
            b2 <- sample(1:ngraphs, n2, replace=TRUE)
            
            # R is stupid and will drop dimensions if one of the groups has only one element
            y1 <- apply(densities[b1, , drop=FALSE], c(2), mean)
            y2 <- apply(densities[b2, , drop=FALSE], c(2), mean)

            results[i] <- JS(list("x"=x, "y"=y1), list("x"=x, "y"=y2), distance=distance)
        }
        pvalue <- (sum(results >= result))/maxBoot

        method <- "Jensen-Shannon divergence between graphs"
        statistic <- result
        names(statistic) <- "JSD"
        rval <- list(statistic=statistic, p.value=pvalue, method=method, data.name=data.name)
        class(rval) <- "htest"
        return(rval)
    }
}


