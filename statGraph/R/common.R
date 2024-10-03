# Returns the adjacency matrix of an igraph object
get.adjacency.matrix <- function(G){
    if(methods::is(G, "igraph")) A <- as.matrix(igraph::get.adjacency(G)) else A <- G
    return(A)
}

# Returns the kernel bandwidth for a sample x based on Sturge's criterion
kernelBandwidth <- function(x){
    n <- length(x)
    nbins <- ceiling(log2(n) + 1)
    return(abs(max(x) - min(x)) / nbins)
}


# For Undirected graphs, accepts two vectors: x, y, such that y[i] = f(x[i]) for some function f.
# For Directed graphs, accepts a list x containing x1 and x and a matrix y, such that y[i, j] = F(x1[i], x2[j]) for some function f
# Returns the trapezoid sum approximation for the integral of f or F 

trapezoidSum <- function(x, y){
    
    if(is.matrix(y)){
        x1 <- x[[1]]
        x2 <- x[[2]]
        
        Lf <- dim(y)
        L1 <- length(x1)
        L2 <- length(x2)
        
        nan <- is.na(y)
        y[nan] <- 0
        y[2:(L1-1), ] = y[2:(L1-1), ] * 2;
        y[, 2:(L2-1)] = y[ , 2:(L2-1)] * 2;
        return(sum(y) * diff(x1)[1] * diff(x2)[1] / 4)

    } else {
        x1 <- x
        
        nonan <- !is.na(y)
        nn <- sum(nonan)
        if(nn < 2L) return(0)
        Y <- y[nonan]
        X <- x1[nonan]
        return(0.5 * sum( (Y[-1] + Y[-nn]) * diff(X)))
    }
}

# Returns the density function for a sample x at n points in the interval [from, to]
gaussianDensity <- function(x, from=NULL, to=NULL, bandwidth="Silverman", npoints=1024, directed=FALSE){

    if(!directed && (dim(x)[2] == 2)){
        directed <- TRUE
        warning("Requested gaussian density of real eigenvalues, but the eigenvalues were complex")
    }

    if(directed){

        if(is.null(from) || is.null(to)){
            f <- kde(x=x, gridsize=c(npoints, npoints));
        } else {
            f <- kde(x=x, gridsize=c(npoints, npoints), xmin=from, xmax = to);
        }
        f$estimate = f$estimate + 1e-12;
        volume <- trapezoidSum(f$eval.points, f$estimate)
        return(list("x" = f$eval.points,
                    "y" = f$estimate/volume,
                    "from" = c( min(f$eval.points[[1]]),
                               min(f$eval.points[[2]])),
                    "to" = c( max(f$eval.points[[1]]),
                             max(f$eval.points[[2]]))
                    ))
    }
    else{
        if(bandwidth == "Sturges") {
            bw <- kernelBandwidth(x)
        } else if(bandwidth == "Silverman"){
            bw <- bw.nrd0(x)
        } else if (bandwidth == "bcv"){
            bw <- suppressWarnings(bw.bcv(x))
        } else if (bandwidth == "ucv"){
            bw <- suppressWarnings(bw.ucv(x))
        } else if (bandwidth == "SJ"){
            bw <- "SJ"
        } else stop("Please, choose a valid bandwidth.")

        if(bw == 0) {
            stop("Bandwidth cannot be zero.")
        }

        if(is.null(from) || is.null(to)){
            f <- density(x, bw=bw, n=npoints)
        } else {
            f <- density(x, bw=bw, from=from, to=to, n=npoints)
        }

        # we do not want the area to be zero, so we add a very small number
        f$y = f$y + 1e-12

        area <- trapezoidSum(f$x, f$y)
        return(list("x" = f$x,
                    "y" = f$y / area,
                    "from"=min(f$x), 
                    "to"=max(f$x)
                    ))
    }
}

# Returns the spectral density for a given adjacency matrix A
spectralDensity <- function(A, from=NULL, to=NULL, bandwidth="Silverman", npoints=1024, directed=FALSE){
    if(!isSymmetric(A) && !directed){
        directed <- TRUE
        warning("The matrix specified is not symmetric, but the parameter 'directed' was set to FALSE")
    }

    if(directed){
        eigenvalues <- eigen(A, only.values=TRUE, symmetric=FALSE)$values
        eigenvalues <- eigenvalues/sqrt(nrow(A))

        eigenmatrix <- matrix(NA, nrow=nrow(A), ncol=2);
        eigenmatrix[,1] = Re(eigenvalues)
        eigenmatrix[,2] = Im(eigenvalues)

        return(gaussianDensity(eigenmatrix, from, to, bandwidth, npoints, directed))
    }
    else {
        eigenvalues <- as.matrix(eigen(A, only.values=TRUE, symmetric=TRUE)$values)
        eigenvalues <- eigenvalues / sqrt(nrow(A))
        return(gaussianDensity(eigenvalues, from, to, bandwidth, npoints, directed))
    }
}

nDensities <- function(spectra, from=NULL, to=NULL, bandwidth="Silverman", npoints=1024){
}
nDensities <- function(spectra, from=NULL, to=NULL, bandwidth="Silverman", npoints=1024){
    ngraphs <- ncol(spectra)
    densities <- matrix(NA, npoints, ngraphs)
    minimum <- min(spectra)
    maximum <- max(spectra)
    if(!is.null(from) && !is.null(to)){
        minimum <- min(minimum, from)
        maximum <- max(maximum, to)
    }
    for(i in 1:ngraphs){
        eigen <- as.matrix(spectra[, i])
        f <- gaussianDensity(eigen, bandwidth=bandwidth, from=minimum, to=maximum, npoints=npoints)
        if(sum(is.na(f)) > 0) return(NA)
        densities[, i] <- f$y
        x <- f$x
    }
    return(list("x" = x, "densities" = densities))
}

# Estimates the spectral density of a graph model
modelSpectralDensity <- function(F, n, p, ngraphs=100, npoints=1024, from=NULL, to=NULL, bandwidth="Silverman", directed=FALSE) {

    # Argument p is supposed to be a vector.
    if(is.numeric(p)){
        p <- c(p)
    }

    if(directed)
    {
        spectra <- array(NA, c(ngraphs, 2, n))
        minRe <- Inf
        maxRe <- -Inf
        minIm <- Inf
        maxIm <- -Inf

        for(i in 1:ngraphs)
        {
            A <- do.call(F, as.list(c(n, p)))
            A <- get.adjacency.matrix(A)

            eigenvalues <- eigen(A, only.values=TRUE, symmetric=FALSE)$values
            eigenvalues <- eigenvalues/sqrt(nrow(A))

            spectra[i, 1, ] = Re(eigenvalues)
            spectra[i, 2, ] = Im(eigenvalues)

            minRe <- min(spectra[i, 1, ], minRe)
            minIm <- min(spectra[i, 2, ], minIm)
            maxRe <- max(spectra[i, 1, ], maxRe)
            maxIm <- max(spectra[i, 2, ], maxIm)

        }

        if(is.null(from)){
            from = c(minRe, minIm)
        }
        if(is.null(to)){
            to = c(maxRe, maxIm)
        }

        densities <- array(NA, c(ngraphs, npoints, npoints))
        for (i in 1:ngraphs) {
            eigenmatrix <- matrix(NA, nrow=nrow(A), ncol=2);
            eigenmatrix[, 1] = spectra[i, 1, ]
            eigenmatrix[, 2] = spectra[i, 2, ]

            f <- gaussianDensity(eigenmatrix, from, to, bandwidth, npoints, directed=TRUE)
            x <- f$x
            densities[i, , ] <- f$y
        }

        density <- apply(densities, c(2, 3), mean)
        return(list("x" = x, "y" = density));
    }
    else
    {
        spectra <- array(NA, c(ngraphs, n))
        minimum <- Inf
        maximum <- -Inf

        for(i in 1:ngraphs)
        {
            A <- do.call(F, as.list(c(n, p)))
            A <- get.adjacency.matrix(A)

            eigenvalues <- (as.numeric(eigen(A, only.values=TRUE, symmetric=TRUE)$values) / sqrt(nrow(A)))
            spectra[i,] <- eigenvalues

            minimum <- min(eigenvalues, minimum)
            maximum <- max(eigenvalues, maximum)
        }

        if(is.null(from)){
            from = minimum
        }
        if(is.null(to)){
            to = maximum
        }

        densities <- array(NA, c(ngraphs, npoints))
        for (i in 1:ngraphs) {
            eigen <- as.matrix(spectra[i, ])
            f <- gaussianDensity(eigen, from=from, to=to, bandwidth=bandwidth, npoints=npoints, directed=FALSE)
            densities[i, ] <- f$y
            x <- f$x
        }

        density <- apply(densities, c(2), mean)
        return(list("x" = x, "y" = density));
    }
}
# Distance Functions

# TODO: Rename this function!
Dist <- function(f1, f2, distance = "L1")
{
    if(distance == "L2")
    {
        Sum <- (f1$y - f2$y)^2
        Sum <- sum(Sum)
        return(Sum)
    }
    else if(distance == "L1")
    {
        Sum <- abs(f1$y - f2$y)
        Sum <- sum(Sum)
        return(Sum)
    }
    else if(distance == "KL")
    {
        return(KL(f1, f2))
    }
    stop("Unsupported distance. Valid distances are 'KL', 'L2' or 'L1'")
}

L2 <- function(f1, f2){
    y <- abs(f1$y - f2$y)
    return (trapezoidSum(f1$x,y))
}

# Returns the Kullback-Leibler divergence between two densities
KL <- function(f1, f2){
    y <- f1$y
    y <- ifelse(y != 0, y*log(y/f2$y), y)
    return(trapezoidSum(f1$x, y))
}


# Returns the Jensen-Shannon divergence between two densities
JS <- function(f1, f2, distance="KL"){
    fm <- f1
    fm$y <- (f1$y + f2$y) / 2
    js <- (Dist(f1, fm, distance) + Dist(f2, fm, distance))/2
    return(js)
}

# Extract a function specified by name
matchFunction <- function(name){
    return(match.fun(name))
}

# Padronize input.
f.transform <- function(g){
    if(methods::is(g, "igraph")) return(as.matrix(igraph::get.adjacency(g)))
    if(methods::is(g, "list") && methods::is(g[[1]],"igraph")){
        d <- lapply(g, f.transform)
        return(d)
    }
    if(methods::is(g, "list") && methods::is(g[[1]], "list")){
        d <- lapply(g, f.transform)
        return(d)
    }
}

#   Returns the spectral densities for a list of adjacency matrices at the same points
#
#   Input: adjacencyMatrices is a list such that each adjacencyMatrices[[i]] is a matrix
#   Output: If directed is set to false, then it outputs a list with two elements:
#   output$densities - an array of size (ngraphs, npoints), such that
#   output$densities[i, ] correspond to the spectral distribution of real eigenvalues.
#   output$x - The points sample in the real line
# Output: If directed is set to true, then it outputs a list with two elements:
#   If directed is set to true, then it outputs an array of size (ngraphs, npoints, npoints), such that
#   outout$densities[i,,] correspond to the spectral distribution of complex eigenvalues.
#   output$x - The points sampled in the real plane
nSpectralDensities <- function(adjacencyMatrices, from=NULL, to=NULL, bandwidth="Silverman", directed=FALSE, npoints=1024){

    ngraphs <- length(adjacencyMatrices)
    ns <- unlist(lapply(adjacencyMatrices, ncol))

    if(directed){

        spectra <- array(NA, c(ngraphs, 2, max(ns)))
        for(i in 1:ngraphs){
            A <- adjacencyMatrices[[i]]
            n <- ncol(A)

            eigenvalues <- eigen(A, only.values=TRUE, symmetric=FALSE)$values
            eigenvalues <- eigenvalues/sqrt(nrow(A))

            spectra[i, 1, ] <- Re(eigenvalues)
            spectra[i, 2, ] <- Im(eigenvalues)
        }

        minRe <- min(spectra[, 1, ], na.rm=TRUE)
        maxRe <- max(spectra[, 1, ], na.rm=TRUE)
        minIm <- min(spectra[, 2, ], na.rm=TRUE)
        maxIm <- max(spectra[, 2, ], na.rm=TRUE)

        if(is.null(from))
        {
            from <- c(minRe, minIm)
        }
        if(is.null(to)){
            to <- c(maxRe, maxIm)
        }

        densities <- array(NA, c(ngraphs, npoints, npoints))
        for(i in 1:ngraphs){
            n <- ns[i]

            eigenmatrix <- matrix(NA, nrow=n, ncol=2);
            eigenmatrix[, 1] = spectra[i, 1, ]
            eigenmatrix[, 2] = spectra[i, 2, ]

            f <- gaussianDensity(eigenmatrix, from=from, to=to, bandwidth=bandwidth, npoints=npoints, directed=TRUE)
            x <- f$x
            densities[i, , ] <- f$y
        }
        return(list("x" = x, "densities" = densities))
    }
    else{
        spectra <- array(NA, c(ngraphs, max(ns)))
        for(i in 1:ngraphs){
            A <- adjacencyMatrices[[i]]
            n <- ncol(A)
            eigenvalues <- (as.numeric(eigen(A, only.values=TRUE, symmetric=TRUE)$values) / sqrt(n))
            spectra[i, 1:n] <- eigenvalues
        }
        minimum <- min(spectra, na.rm=TRUE)
        maximum <- max(spectra, na.rm=TRUE)
        if(is.null(from))
        {
            from <- minimum
        }
        if(is.null(to)){
            to <- maximum
        }

        densities <- array(NA, c(ngraphs, npoints))
        for(i in 1:ngraphs){
            n <- ns[i]
            eigen <- as.matrix(spectra[i, 1:n])
            f <- gaussianDensity(eigen, bandwidth=bandwidth, from=from, to=to, npoints=npoints)
            densities[i,] <- f$y
            x <- f$x
        }
        return(list("x" = x, "densities" = densities))
    }
}
nSpectralDensitiesP <- function(adjacencyMatrices, from=NULL, to=NULL, bandwidth="Silverman", directed=FALSE, npoints=1024){

    ngraphs <- length(adjacencyMatrices)
    ns <- unlist(lapply(adjacencyMatrices, ncol))

    if(directed){
        x <- c()
        densities <- array(NA, c(ngraphs, npoints, npoints))
        for(i in 1:ngraphs){
            G <- adjacencyMatrices[[i]]
            f <- spectralDensity(G, npoints = npoints, from=from, to=to, directed=directed);
            densities[i, , ] <- f$y;
            x <- f$x
        }
        return(list("x" = x, "densities" = densities))
    }
    else
    {
        stop("Not implemented")
    }
}

