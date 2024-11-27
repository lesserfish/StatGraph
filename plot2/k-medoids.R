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
                    append = FALSE,
                    lcol = "black",
                    pcol = "black",
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
  if(!append){
    plot(x = NA, y = NA, type = "l", ylab=ylab, xlab=xlab, main = main, ylim=ylim, xlim=c(min(x), max(x)))
  } 
  #polygon(c(x, rev(x)), c(lower, rev(upper)), col = pcol, border = NA)
  lines(x=x, y = means, col=lcol, lwd=2)
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

add_legend <- function(group_sizes, colors, ...) {
  legend("bottomright",                      # Position of the legend
         legend = group_sizes,  # Legend labels
         col = colors,                     # Colors for lines
         lwd = 0.5,                          # Line width
         ...
         )
}

Cairo::CairoSVG(file = "out/k-medoids-all.svg", width = 8, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(3, 1), mar=c(5.1, 4.1, 4.1, 8.7), xpd=TRUE)
run_plot("ER", size = 100, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model", lcol = "red",    pcol=rgb(1, 0, 0, alpha=0.3))
run_plot("ER", size = 300, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model", lcol = "blue",   pcol=rgb(0, 0, 1, alpha=0.3), append = TRUE)
run_plot("ER", size = 500, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model", lcol = "purple", pcol=rgb(1, 0, 1, alpha=0.3), append = TRUE)
run_plot("ER", size = 800, x = seq(0, 0.03, length.out=30), main = "Erdős–Rényi Model", lcol = "cyan",   pcol=rgb(0, 1, 1, alpha=0.3), append = TRUE)
add_legend(c("100 nodes", "300 nodes", "500 nodes", "800 nodes"), c("red", "blue", "purple", "cyan"), inset=c(-0.17,0))


run_plot("PA", size = 100, x = seq(0, 0.2, length.out=30), main = "Barabási-Albert Model", lcol = "red",    pcol=rgb(1, 0, 0, alpha=0.3))
run_plot("PA", size = 300, x = seq(0, 0.2, length.out=30), main = "Barabási-Albert Model", lcol = "blue",   pcol=rgb(0, 0, 1, alpha=0.3), append = TRUE)
run_plot("PA", size = 500, x = seq(0, 0.2, length.out=30), main = "Barabási-Albert Model", lcol = "purple", pcol=rgb(1, 0, 1, alpha=0.3), append = TRUE)
run_plot("PA", size = 800, x = seq(0, 0.2, length.out=30), main = "Barabási-Albert Model", lcol = "cyan",   pcol=rgb(0, 1, 1, alpha=0.3), append = TRUE)
add_legend(c("100 nodes", "300 nodes", "500 nodes", "800 nodes"), c("red", "blue", "purple", "cyan"), inset=c(-0.17,0))


run_plot("WS", size = 100, x = seq(0, 0.03, length.out=30), main = "Watts-Strogatz Model", lcol = "red",    pcol=rgb(1, 0, 0, alpha=0.3))
run_plot("WS", size = 300, x = seq(0, 0.03, length.out=30), main = "Watts-Strogatz Model", lcol = "blue",   pcol=rgb(0, 0, 1, alpha=0.3), append = TRUE)
run_plot("WS", size = 500, x = seq(0, 0.03, length.out=30), main = "Watts-Strogatz Model", lcol = "purple", pcol=rgb(1, 0, 1, alpha=0.3), append = TRUE)
run_plot("WS", size = 800, x = seq(0, 0.03, length.out=30), main = "Watts-Strogatz Model", lcol = "cyan",   pcol=rgb(0, 1, 1, alpha=0.3), append = TRUE)
add_legend(c("100 nodes", "300 nodes", "500 nodes", "800 nodes"), c("red", "blue", "purple", "cyan"), inset=c(-0.17,0))
dev.off()
