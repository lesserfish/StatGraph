gradient <- function(from, to, points){
    colors <- c()
    for(point in points){
        r <-  (1 - point) * from[1] + point * to[1]
        g <-  (1 - point) * from[2] + point * to[2]
        b <-  (1 - point) * from[3] + point * to[3]
        col <- rgb(r=r, g=g, b=b)
        colors <- c(colors, col)
    }
    return(colors)
}

run_plot <- function(model 
                    , epsilons
                    , size = 400
                    , cores = seq(1, 50)
                    , start = 1
                    , xlim=c(0, 1)
                    , ylab = "Acceptance Rate"
                    , xlab = "Acceptance Threshold"
                    #, colors = c("red", "blue", "green", "yellow", "cyan", "magenta", "black", "white", "orange", "purple")
                    , colors = NULL
                    , x = seq(0, 1, 0.01)
                    , main = ""
                    , ...) {

    if(is.null(colors)){
        colors <- gradient(c(0, 0, 0), c(1, 0.2, 0), points = seq(0, 1, length.out = length(epsilons) + 1))
    }
    plot(x=x, y=x, type='l', col="grey", xlab = xlab, ylab = ylab, xlim=c(0,1), ylim=c(0, 1), main=main)

    h <- list()

    h0 <- c()
    for(core in cores){
        fp <- paste0("../Code/"
                    , model, "/"
                    , size, "/"
                    , "simulation.anogva-h0-"
                    , model
                    , "-"
                    , size
                    , "-0-result-"
                    , core, ".data")
        if(!file.exists(fp)){
            msg <- paste("File ", fp, " does not exist", sep="")
            warning(msg)
        }

        part <- scan(fp, what = numeric(), quiet = TRUE)
        h0 <- c(h0, part)
    }

    h[[1]] <- h0
    i <- 2
    for(eps in epsilons){
        he <- c()
        for(core in cores){
            fp <- paste0("../Code/"
                    , model, "/"
                    , size, "/"
                    , "simulation.anogva-h1-"
                    , model
                    , "-"
                    , size, "-"
                    , eps
                    , "-result-"
                    , core, ".data")
            part <- scan(fp, what = numeric(), quiet = TRUE)
            he <- c(he, part)
        }
        h[[i]] <- he
        i <- i + 1
    }

    for(i in 1:(length(epsilons) + 1)){
        eps <- c(0, epsilons)[i]
        data <- h[[i]]
        y <- sapply(x, function(y)(length(which(data < y)) / length(data)))
        lines(x = x, y = y, col = colors[i], lwd=2.0)
    }
    legend_labels = c(bquote(epsilon == .(0)), lapply(epsilons, function(e) bquote(epsilon == .(e))))
    legend("bottomright",                      # Position of the legend
         legend = legend_labels,
         col = colors,                         # Colors for lines
         lwd = 1.0,                            # Line width
     )
}

Cairo::CairoSVG(file = "out/anogva.svg", height = 13,onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(3, 1))
run_plot("ER", main = "Erdős–Rényi", epsilons=c(5e-04, 0.001, 0.0015, 0.002))
run_plot("PA", main = "Barabási–Albert", epsilons=c(0.003, 0.004, 0.005, 0.007, 0.008, 0.01 , 0.015))
run_plot("WS", main = "Watts–Strogatz", epsilons=c(5e-04, 0.001, 0.0015, 0.002, 0.003))
dev.off()
