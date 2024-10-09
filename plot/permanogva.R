run_plot <- function(model, 
                     count, 
                     start = 1, 
                     xlim=c(0, 1),
                     main = NULL,
                     ylab = "Acceptance Rate",
                     xlab = "Acceptance Threshold",
                     group_sizes = c(10, 30, 50, 70),
                     colors = c("red", "green", "blue", "cyan"),
                     size = 400,
                     x = seq(0, 1, 0.01),
                     h = "h0",
                     ...) {
  if(is.null(main)){
    hh <- "H0"
    if(h == "h1"){hh <- "H1"}
    main <- paste(model, " Model - ", hh, sep="")
  }
  plot(x=x, y=x, type='l', col="grey", main = main, xlab = xlab, ylab = ylab)
  len <- length(group_sizes)
  for(j in seq(1, len)){
    group_size <- group_sizes[j]
    color <- colors[j]
    data <- c()  
    for(i in seq(start, count)){
      fp <- paste("../Code/", 
                  model, "/", 
                  size, "/", 
                  "simulation.permanogva-", h, 
                  "-", model, 
                  "-", size,
                  "-", group_size,
                  "-result-", i,
                  ".data", sep = "")
      if(!file.exists(fp)){
        msg <- paste("File ", fp, " does not exist", sep="")
        warning(msg)
      }
      
      part <- scan(fp, what = numeric(), quiet = TRUE)
      data <- c(data, part)
    }
    y <- sapply(x, function(y)(length(which(data < y)) / length(data)))
    lines(x = x, y = y, col = color, lwd=1.5)
  }
  
  legend("bottomright",                      # Position of the legend
         legend = sprintf("m = %d", group_sizes),  # Legend labels
         col = colors,                     # Colors for lines
         lty = 1,                          # Line type (solid)
         lwd = 2,                          # Line width
         )
}

Cairo::CairoSVG(file = "out/permanogva.svg", height = 9, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(3, 2))
run_plot("ER", count=50, h = "h0", main = "Erdős–Rényi - H0") 
run_plot("ER", count=50, h = "h1", main = "Erdős–Rényi - H1") 
run_plot("PA", count=50, h = "h0", main = "Barabási–Albert - H0")
run_plot("PA", count=50, h = "h1", main = "Barabási–Albert - H1")
run_plot("WS", count=50, h = "h0", main = "Watts–Strogatz - H0")
run_plot("WS", count=50, h = "h1", main = "Watts–Strogatz - H1")
dev.off()
