run_plot <- function(model, 
                     size = 600, 
                     count = 50, 
                     start = 1, 
                     rline = c(0.25, 0.35), 
                     breaks=10, 
                     xlim=c(0, 1),
                     main = NULL,
                     ylab = "Frequency",
                     xlab = "Estimated parameter",
                     ...) {
  if(is.null(main)){
    main <- paste(model, " Model with V = ", size, sep="")
  }
  data_x <- c()  
  data_y <- c()  
  for(i in seq(start, count)){
    fp <- paste("../Code/", 
                model, "/", 
                size, "/", 
                "simulation.param.estimator2-", model, 
                "2-", size,
                "-result-", i,
                ".data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
    }
    
    part <- read.csv(fp, header=TRUE)
    px <- apply(part, 1, min)
    py <- apply(part, 1, max)
    data_x <- c(data_x, px)
    data_y <- c(data_y, py)
  }
  hist(data_x, 
       col = "white", 
       border = "black", 
       main = main, 
       xlab = xlab, 
       ylab=ylab, 
       breaks=breaks, 
       xlim=xlim,
       lwd = 1.0,
       ...)
  abline(v = rline[1], col = "red", lwd = 1.5)
  hist(data_y, 
       col = "white", 
       border = "black", 
       main = main, 
       xlab = xlab, 
       ylab=ylab, 
       breaks=breaks, 
       xlim=xlim,
       lwd = 1.0,
       ...)
  abline(v = rline[2], col = "red", lwd = 1.5)
}


Cairo::CairoSVG(file = "out/param-estimator-2d.svg", height = 9, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(3, 2))
run_plot("ER", breaks= 10, main = "Erdős–Rényi - 600 nodes")
run_plot("PA", breaks= 20, main = "Barabási–Albert - 600 nodes")
run_plot("WS", breaks= 8, main = "Watts–Strogatz - 600 nodes")
dev.off()


