#' ANOGVA Analysis Of Graph Variability
#'
#' \code{anogva} statistically tests whether two or more sets of graphs are generated
#' by the same random graph model. It is a generalization of the 'takahashi.test'
#' function.
#'
#' @param G a list of undirected graphs (igraph type) or their adjacency
#' matrices. The adjacency matrix of an unweighted graph contains only 0s and
#' 1s, while the weighted graph may have nonnegative real values that correspond
#' to the weights of the edges.
#'
#' @param labels an array of integers indicating the labels of each graph.
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
#' \item{statistic}{the statistic of the test.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a string indicating the used method.}
#' \item{data.name}{a string with the data's name(s).}
#'
#' @keywords analysis_of_graph_variability
#'
#' @examples
#' set.seed(1)
#' g1 <- g2 <- g3 <- list()
#' for (i in 1:20){
#'   g1[[i]] <- igraph::sample_gnp(50, 0.50)
#'   g2[[i]] <- igraph::sample_gnp(50, 0.50)
#'   g3[[i]] <- igraph::sample_gnp(50, 0.52)
#' }
#' G <- c(g1, g2, g3)
#' label <- c(rep(1, 20), rep(2, 20), rep(3, 20))
#' result <- anogva(G, label, maxBoot=50)
#' result
#'
#' @references
#'
#' Fujita, A., Vidal, M. C. and Takahashi, D. Y. (2017) A Statistical Method to
#' Distinguish Functional Brain Networks. _Front. Neurosci._, *11*, 66.
#' doi:10.3389/fnins.2017.00066.
#'
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
#' @export
anogva <- function(G, labels, maxBoot=1000, bandwidth="Silverman", directed=FALSE, npoints=1024, from=NULL, to=NULL, distance="L2"){
    if(directed) {
        data.name <- deparse(substitute(G))
        if(methods::is(G, "list") && methods::is(G[[1]], "igraph")) G <- f.transform(G)

        f <- nSpectralDensities(G, from=from, to=to, bandwidth=bandwidth, directed=directed, npoints=npoints)
        densities <- f$densities
        x <- f$x


        group_qty <- max(labels)
        group_mean <- list()
        overall_mean <- list()

        for(group in 1:group_qty){
            current_group <- densities[which(labels==group),,]
            current_mean <- apply(current_group, MARGIN = c(2, 3), sum) / length(which(labels == group))
            group_mean[[group]] <- list('y'=current_mean,'x'=x)
        }
        overall_mean$y <- apply(densities, MARGIN = c(2, 3), sum) / length(G)
        overall_mean$x <- x

        distOrigin <- 0

        for(group in 1:group_qty){
            distOrigin <- distOrigin + Dist(overall_mean, group_mean[[group]], distance=distance)
        }

        distOrigin <- distOrigin/group_qty

        ## Permutation Testing

        bootDist <- array(0, maxBoot)
        for(i in 1:maxBoot){

            Blabels <- sample(labels, length(labels), replace = FALSE)
            Bgroup_mean <- list()

            for(group in 1:group_qty){
                current_group <- densities[which(Blabels==group),,]
                current_mean <- apply(current_group, MARGIN = c(2, 3), sum) / length(which(Blabels == group))
                Bgroup_mean[[group]] <- list('y'=current_mean,'x'=x)
            }

            currentDist <- 0
            for(group in 1:group_qty){
                currentDist <- currentDist + Dist(overall_mean, Bgroup_mean[[group]], distance=distance)
            }

            currentDist <- currentDist / group_qty
            bootDist[i] <- currentDist 
        }

        pvalue <- length(which(bootDist >= distOrigin)) / (maxBoot+1)
        statistic        <- distOrigin
        names(statistic) <- "statistic"
        method           <- "Analysis of Graph Variability"
        rval             <- list(statistic = statistic, p.value = pvalue, method = method, data.name = data.name)
        class(rval)      <- "htest"
        return(rval)
    } else {
        data.name <- deparse(substitute(G))
        if(methods::is(G, "list") && methods::is(G[[1]], "igraph")) G <- f.transform(G)

        f <- nSpectralDensities(G, from=from, to=to, bandwidth=bandwidth, directed=directed, npoints=npoints)
        densities <- f$densities
        x <- f$x


        group_qty <- max(labels)
        group_mean <- list()
        overall_mean <- list()

        for(group in 1:group_qty){
            current_group <- densities[which(labels==group),]
            current_mean <- apply(current_group, MARGIN = c(2), sum) / length(which(labels == group))
            group_mean[[group]] <- list('y'=current_mean,'x'=x)
        }
        overall_mean$y <- apply(densities, MARGIN = c(2), sum) / length(G)
        overall_mean$x <- x

        distOrigin <- 0

        for(group in 1:group_qty){
            distOrigin <- distOrigin + Dist(overall_mean, group_mean[[group]], distance=distance)
        }

        distOrigin <- distOrigin/group_qty

        ## Permutation Testing

        bootDist <- array(0, maxBoot)
        for(i in 1:maxBoot){

            Blabels <- sample(labels, length(labels), replace = FALSE)
            Bgroup_mean <- list()

            for(group in 1:group_qty){
                current_group <- densities[which(Blabels==group),]
                current_mean <- apply(current_group, MARGIN = c(2), sum) / length(which(Blabels == group))
                Bgroup_mean[[group]] <- list('y'=current_mean,'x'=x)
            }

            currentDist <- 0
            for(group in 1:group_qty){
                currentDist <- currentDist + Dist(overall_mean, Bgroup_mean[[group]], distance=distance)
            }

            currentDist <- currentDist / group_qty
            bootDist[i] <- currentDist 
        }

        pvalue <- length(which(bootDist >= distOrigin)) / (maxBoot+1)
        statistic        <- distOrigin
        names(statistic) <- "statistic"
        method           <- "Analysis of Graph Variability"
        rval             <- list(statistic = statistic, p.value = pvalue, method = method, data.name = data.name)
        class(rval)      <- "htest"
        return(rval)
    }
}

