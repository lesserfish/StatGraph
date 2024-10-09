ci <- function(row_data){
  if(var(row_data[-1]) == 0) {
    return(c(row_data[2], row_data[2], row_data[2]))
  }
  else {
    
    test <- t.test(row_data[-1])
    m = as.numeric(test$estimate)
    cil <- min(test$conf.int[[1]], 1)
    cih <- test$conf.int[[2]]
    return(c(m, cil, cih))
  }
}

run_plot <- function(model, 
                    x,
                    size, 
                    ylim=c(0, 1),
                    main = NULL,
                    ylab = "ARI Score",
                    xlab = expression(epsilon),
                    ...) {
  if(is.null(main)){
    main <- paste("ARI Score for the ", model, " Model with V = ", size, sep="")
  }
  fp <- paste("../Code/KMedoids/simulation.kmedoids-", 
                model,
                "-", size,
                "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
    }
  load(fp)
  cia <- apply(ari_data, 1, ci)
  means <- cia[1, ]
  lower <- cia[2, ]
  upper <- cia[3, ]
  
  data <- data.frame(x=x, means = means, lower = lower, upper = upper)
  
  
  # Plot the line
  plot(x = x, y = means, type = "l", col = "black", lwd = 2, ylab=ylab, xlab=xlab, main = main, ylim=ylim)
  polygon(c(x, rev(x)), c(lower, rev(upper)), col = "gray", border = NA)
  lines(x=x, y = means, col="black", lwd=2)
}

run_alternative_plot <- function(model, 
                    xpoints,
                    size, 
                    ylim=c(0, 1),
                    main = NULL,
                    ylab = "ARI Score",
                    xlab = expression(epsilon),
                    ...) {
  if(is.null(main)){
    main <- paste("ARI Score for the ", model, " Model with V = ", size, sep="")
  }
  fp <- paste("../Code/KMedoids/simulation.kmedoids-", 
                model,
                "-", size,
                "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
    }
  load(fp)

  plot(1, 1
       , type = "n"
       , xlim = c(0, max(xpoints))
       , ylim = c(0, 1)
       , ylab="ARI Score", xlab=expression(epsilon)
       , main=main)

  m <- c()
  for(i in 1:nrow(ari_data)){
      x <- xpoints[i]
      data_points <- as.numeric(ari_data[i, -1])
      m <- c(m, mean(data_points))
      points(x = rep(x, length(data_points)), y = data_points, col = rgb(1, 0.27, 0.4, alpha = 0.1), pch=19)
  }

  lines(x = xpoints, y = m, col="red", lwd=3)
}

Cairo::CairoSVG(file = "out/k-medoids-er.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_plot("ER", size = 100, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 100 nodes")
run_plot("ER", size = 300, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 300 nodes")
run_plot("ER", size = 500, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 500 nodes")
run_plot("ER", size = 800, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 800 nodes")
dev.off()

Cairo::CairoSVG(file = "out/k-medoids-pa.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_plot("PA", size = 100, x = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 100 nodes")
run_plot("PA", size = 300, x = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 300 nodes")
run_plot("PA", size = 500, x = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 500 nodes")
run_plot("PA", size = 800, x = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 800 nodes")
dev.off()

Cairo::CairoSVG(file = "out/k-medoids-ws.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_plot("PA", size = 100, x = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 100 nodes")
run_plot("PA", size = 300, x = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 300 nodes")
run_plot("PA", size = 500, x = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 500 nodes")
run_plot("PA", size = 800, x = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 800 nodes")
dev.off()

Cairo::CairoSVG(file = "out/k-medoids-er-alternative.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_alternative_plot("ER", size = 100, xpoints = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 100 nodes")
run_alternative_plot("ER", size = 300, xpoints = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 300 nodes")
run_alternative_plot("ER", size = 500, xpoints = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 500 nodes")
run_alternative_plot("ER", size = 800, xpoints = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model - 800 nodes")
dev.off()

Cairo::CairoSVG(file = "out/k-medoids-pa-alternative.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_alternative_plot("PA", size = 100, xpoints = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 100 nodes")
run_alternative_plot("PA", size = 300, xpoints = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 300 nodes")
run_alternative_plot("PA", size = 500, xpoints = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 500 nodes")
run_alternative_plot("PA", size = 800, xpoints = seq(0, 0.2, length.out=30), main = "Barabási–Albert Model  - 800 nodes")
dev.off()

Cairo::CairoSVG(file = "out/k-medoids-ws-alternative.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_alternative_plot("PA", size = 100, xpoints = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 100 nodes")
run_alternative_plot("PA", size = 300, xpoints = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 300 nodes")
run_alternative_plot("PA", size = 500, xpoints = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 500 nodes")
run_alternative_plot("PA", size = 800, xpoints = seq(0, 0.03, length.out=30), main = "Watts–Strogatz Model  - 800 nodes")
dev.off()
