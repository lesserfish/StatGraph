run_plot <- function(model, 
                     size, 
                     count = 50, 
                     start = 1, 
                     rline = 0.35, 
                     breaks=10, 
                     xlim=c(0, 1),
                     main = NULL,
                     ylab = "Frequency",
                     xlab = "Estimated parameter",
                     ...) {
  if(is.null(main)){
    main <- paste(model, " Model with V = ", size, sep="")
  }
  data <- c()  
  for(i in seq(start, count)){
        fp <- paste("../Code/", 
                    model, "/", 
                    size, "/", 
                    "simulation.param.estimator-", model, 
                    "-", size,
                    "-result-", i,
                    ".data", sep = "")
        if(!file.exists(fp)){
            msg <- paste("File ", fp, " does not exist", sep="")
            warning(msg)
        }

        part <- scan(fp, what = numeric(), quiet = TRUE)
        data <- c(data, part)
  }
  hist(data, 
       col = "white", 
       border = "black", 
       main = main, 
       xlab = xlab, 
       ylab=ylab, 
       breaks=breaks, 
       xlim=xlim,
       lwd = 1.0,
       ...)
  abline(v = rline, col = "red", lwd = 1.5)
}


Cairo::CairoSVG(file = "out/param-estimator-er.svg", width = 15, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_plot("ER", "100", breaks= 12, main = "Erdős–Rényi - 100 nodes")
run_plot("ER", "300", breaks = 6, main = "Erdős–Rényi - 300 nodes")
run_plot("ER", "500", breaks = 4, main = "Erdős–Rényi - 500 nodes")
run_plot("ER", "800", breaks = 3, main = "Erdős–Rényi - 800 nodes")
dev.off()

Cairo::CairoSVG(file = "out/param-estimator-pa.svg", width = 15, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_plot("PA", "100", breaks= 26, main = "Barabási–Albert - 100 nodes")
run_plot("PA", "300", breaks = 18, main = "Barabási–Albert - 300 nodes")
run_plot("PA", "500", breaks = 12, main = "Barabási–Albert - 500 nodes")
run_plot("PA", "800", breaks = 9, main = "Barabási–Albert - 800 nodes")
dev.off()


Cairo::CairoSVG(file = "out/param-estimator-ws.svg", width = 15, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 2))
run_plot("WS", "100", breaks= 12, main = "Watts–Strogatz - 100 nodes")
run_plot("WS", "300", breaks = 6, main = "Watts–Strogatz - 300 nodes")
run_plot("WS", "500", breaks = 4, main = "Watts–Strogatz - 500 nodes")
run_plot("WS", "800", breaks = 3, main = "Watts–Strogatz - 800 nodes")
dev.off()

