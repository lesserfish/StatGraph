graph.param.distance <- function(G, model, parameters = NULL, npoints = 100, ngraphs = 1, numCores = 1, bandwidth="Silverman", search_mode = "ternary_search", distance = "L1", directed=FALSE, from = NULL, to = NULL, log=FALSE){

    A <- get.adjacency.matrix(G)
    n <- ncol(A)

    # Verification that input is Valid:
    search <- grid_distance

    #TODO: Should graphSpec be calculated with the same from, to of modelSpec??
    graphSpec <- spectralDensity(G,from = from, to = to, bandwidth = bandwidth, npoints = npoints, directed=TRUE)
    criteria <- function(x){
        modelSpec <- modelSpectralDensity(model, n, x, from = from, to = to, bandwidth = bandwidth, ngraphs = ngraphs, npoints = npoints, directed=TRUE)
        dist <- Dist(graphSpec, modelSpec, distance = distance)
        return(dist)
    }
    result <- search(criteria, parameters, log=log)
    out = result

    method    <- "Graph parameter estimator"
    info     <- "Estimating the parameter that best approximates the model to the observed graph"
    data.name <- deparse(substitute(G))
    output     <- list(method=method, info=info, data.name=data.name, distance=out)
    attr(output, "class") <- "statGraph"
    return(output)
}
grid_distance <- function(criteria, parameters, log = FALSE)
{
    # If input is vector instead of a list, fix it.
    if(is.atomic(parameters)){
        parameters = list(parameters)
    }

    if(length(parameters) == 1)
    {
        pslice <- parameters[[1]]
        output <- c()

        if(length(pslice) == 0){
            stop("Wrong input for Grid search. Received an empty parameter list. Parameters should be a list such that list[n]] contains a vector of possible parameters for the n-th argument of the criteria function.")
        }

        for(p in pslice)
        {
            val <- criteria(c(p))
            if(log){
                cat(paste("Parameter ", p,":  " , val , "\n", sep=""))
            }
            output <- c(output, val)
        }

        out <- output
        return(out)
    }
}
