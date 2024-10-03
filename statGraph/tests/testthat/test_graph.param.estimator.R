# DON'T RUN MANUALLY!
# THIS SCRIPT IS INVOKED AUTOMATICALLY BY devtools::test()
# Run 'Rscript tests.R' to perform tests or 'Rscript build.r' to build the project.
# For Documentation on how to create tests, please refer to: https://cran.r-project.org/web/packages/testthat/testthat.pdf

UER <- function(n, p) {
        G <- as.matrix(igraph::as_adj(igraph::sample_gnp(n=n, p=p)))
        return(G)
}

UER2 <- function(n, p1, p2) {
        p <- p1 ** 2 + p2 ** 2
        G <- as.matrix(igraph::as_adj(igraph::sample_gnp(n=n, p=p)))
        return(G)
}
DER <- function(n, p1, p2) {
    G1 <- UER(n, p1)
    G2 <- UER(n, p2)
    G1[upper.tri(G1)] <- 0
    G2[lower.tri(G2)] <- 0
    G <- G1 + G2
    return(G)
}

RunTest("graph.param.estimator", {
    eps <- 0.05
    acceptable_error <- 0.1
    G <- UER(500, 0.2)
    parameters <- seq(0.1, 0.5, eps)
    out <- graph.param.estimator(G, parameters = parameters, UER, npoints=100, search_mode = "grid_search")
    test_that("Grid search with undirected graphs (1 argument model)",
              {
                expect_lt(out$param, 0.2 + acceptable_error)
                expect_gt(out$param, 0.2 - acceptable_error)
              }
    )
})

RunTest("graph.param.estimator", {
    eps <- 0.05
    acceptable_error <- 0.1
    G <- UER(500, 0.2)
    parameters <- list(min = c(0.1), max = c(0.5), eps = c(eps))
    out <- graph.param.estimator(G, parameters = parameters, UER, npoints=100, search_mode = "ternary_search")
    test_that("Ternary search with undirected graphs (1 argument model)",
              {
                expect_lt(out$param, 0.2 + acceptable_error)
                expect_gt(out$param, 0.2 - acceptable_error)
              }
    )
})

RunTest("graph.param.estimator", {
    eps <- 0.05
    acceptable_error <- 0.1
    acceptable_error <- 0.1
    G <- UER2(500, 0.2, 0.3)
    parameters <- list(seq(0.1, 0.5, eps), seq(0.1, 0.5, eps))
    out <- graph.param.estimator(G, parameters = parameters, UER2, npoints=100, search_mode = "grid_search")

    p1 <- min(out$param[1], out$param[2])
    p2 <- max(out$param[1], out$param[2])

    test_that("Grid search with undirected graphs (2 argument model)",
              {
                expect_lt(p1, 0.2 + acceptable_error)
                expect_gt(p1, 0.2 - acceptable_error)
                
                expect_lt(p2, 0.3 + acceptable_error)
                expect_gt(p2, 0.3 - acceptable_error)
              }
    )
})

RunTest("graph.param.estimator", {
    eps <- 0.05
    acceptable_error <- 0.1
    G <- UER2(500, 0.2, 0.3)
    parameters <- list(min = c(0.1, 0.1), max = c(0.5, 0.5), eps = c(eps, eps))
    out <- graph.param.estimator(G, parameters = parameters, UER2, npoints=100, search_mode = "ternary_search")
    
    p1 <- min(out$param[1], out$param[2])
    p2 <- max(out$param[1], out$param[2])

    test_that("Ternary search with undirected graphs (2 argument model)",
              {
                expect_lt(p1, 0.2 + acceptable_error)
                expect_gt(p1, 0.2 - acceptable_error)
                
                expect_lt(p2, 0.3 + acceptable_error)
                expect_gt(p2, 0.3 - acceptable_error)
              }
    )
})


RunTest("graph.param.estimator", {
    eps <- 0.1
    acceptable_error <- 0.2001
    G <- DER(200, 0.2, 0.3)
    parameters <- list(seq(0.1, 0.5, eps), seq(0.1, 0.5, eps))
    out <- graph.param.estimator(G, parameters = parameters, DER, npoints=100, search_mode = "grid_search", directed=TRUE, ngraphs=5)

    p1 <- min(out$param[1], out$param[2])
    p2 <- max(out$param[1], out$param[2])

    test_that("Grid search with directed graphs (2 argument model)",
              {
                expect_lt(p1, 0.2 + acceptable_error)
                expect_gt(p1, 0.2 - acceptable_error)
                
                expect_lt(p2, 0.3 + acceptable_error)
                expect_gt(p2, 0.3 - acceptable_error)
              }
    )
})

RunTest("graph.param.estimator", {
    eps <- 0.1
    acceptable_error <- 0.2
    G <- DER(200, 0.2, 0.3)
    parameters <- list(min = c(0.1, 0.1), max = c(0.5, 0.5), eps = c(eps, eps))
    out <- graph.param.estimator(G, parameters = parameters, DER, npoints=100, search_mode = "ternary_search", directed=TRUE, ngraphs=5)
    
    p1 <- min(out$param[1], out$param[2])
    p2 <- max(out$param[1], out$param[2])

    test_that("Ternary search with directed graphs (2 argument model)",
              {
                expect_lt(p1, 0.2 + acceptable_error)
                expect_gt(p1, 0.2 - acceptable_error)
                
                expect_lt(p2, 0.3 + acceptable_error)
                expect_gt(p2, 0.3 - acceptable_error)
              }
    )
})

