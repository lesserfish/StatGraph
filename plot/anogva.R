library(ggplot2)
library(patchwork)
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
                    , linewidth=0.8
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
    h <- list()

    h0 <- c()
    for(core in cores){
        fp <- paste0("../MacRun/Code/Anogva/"
                    , "simulation.anogva-h1-"
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
            fp <- paste0("../MacRun/Code/Anogva/"
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

    df <- data.frame(matrix(nrow=length(x), ncol=0))
    df$x <- x
    for(i in 1:(length(epsilons) + 1)){
        eps <- c(0, epsilons)[i]
        data <- h[[i]]
        y <- sapply(x, function(y)(length(which(data < y)) / length(data)))
        df[[i]] <- y
    }

    gg <- ggplot(df, aes(x = x)) +
          geom_abline(intercept=0, color="#BBB") +
          geom_line(aes(y = V2, color = 1), linewidth = linewidth) +
          geom_line(aes(y = V3, color = 2), linewidth = linewidth) +
          geom_line(aes(y = V4, color = 3), linewidth = linewidth) +
          geom_line(aes(y = V5, color = 4), linewidth = linewidth) +
          geom_line(aes(y = V6, color = 5), linewidth = linewidth) +
          geom_line(aes(y = V7, color = 6), linewidth = linewidth) +
          geom_line(aes(y = V8, color = 7), linewidth = linewidth) +
          geom_line(aes(y = V9, color = 8), linewidth = linewidth) +
          geom_line(aes(y = V10, color = 9), linewidth = linewidth) +
          geom_line(aes(y = V11, color = 10), linewidth = linewidth) +
          geom_line(aes(y = V12, color = 11), linewidth = linewidth) +
          geom_line(aes(y = V13, color = 12), linewidth = linewidth) +
          geom_line(aes(y = V14, color = 13), linewidth = linewidth) +
          geom_line(aes(y = V15, color = 14), linewidth = linewidth) +
          geom_line(aes(y = V16, color = 15), linewidth = linewidth) +
          scale_color_gradient(low = rgb(0, 0, 0), high = rgb(1, 0, 0), labels=c(epsilons[1], epsilons[15]), breaks=c(1, 15)) +
          labs(color = "", x = "Acceptance Threshold", y = "Acceptance Rate") +
          theme_minimal() +
          theme(legend.position = "right") +
          theme(
            legend.position = "right",      # Position the legend on the right
            panel.grid.major = element_blank(),  # Remove major grid lines
            panel.grid.minor = element_blank(),  # Remove minor grid lines
            plot.title = element_text(hjust = 0.5),  # Center the title
            panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add a border around the plot area
          ) +
          ggtitle(main)
    return(gg)
}


ERL <- sort(c(0, 0.00142857142857143, 0.00285714285714286, 0.00171428571428571, 0.00314285714285714, 0.000571428571428571, 0.002, 0.00342857142857143, 0.00228571428571429, 0.00371428571428571, 0.00114285714285714, 0.00257142857142857, 0.004, 0.006, 0.008, 0.005, 0.0055))


PAL <- sort(c(0, 0.0107142857142857, 0.0214285714285714, 0.00214285714285714, 0.0128571428571429, 0.0235714285714286, 0.00428571428571429, 0.015, 0.0257142857142857, 0.00642857142857143, 0.0171428571428571, 0.03, 0.00857142857142857, 0.0192857142857143, 0.04))
WSL <- sort(c(0, 0.00214285714285714, 0.00428571428571429, 0.000428571428571429, 0.00257142857142857, 0.00471428571428571, 0.000857142857142857, 0.003, 0.00514285714285714, 0.00128571428571429, 0.00342857142857143, 0.00557142857142857, 0.00171428571428571, 0.00385714285714286, 0.006, 0.007, 0.008))

eps <- list(ER = ERL,
            PA = PAL,
            WS = WSL)

#Cairo::CairoSVG(file = "out/anogva.svg")
svglite::svglite("out/anogva.svg", width=6, height=12)
par(mfrow = c(3, 1))
p1 <- run_plot("ER", main = "Erdős–Rényi", epsilons=eps$ER)
p2 <- run_plot("PA", main = "Barabási–Albert", epsilons=eps$PA)
p3 <- run_plot("WS", main = "Watts–Strogatz", epsilons=eps$WS)
(p1 / p2 / p3)
dev.off()
