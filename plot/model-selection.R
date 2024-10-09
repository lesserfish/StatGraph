run_plot <- function(model, 
                     sizes = seq(20, 120), 
                     start = 1, 
                     ylim=c(0, 1),
                     main = NULL,
                     ylab = "Selection Rate",
                     xlab = "Number of nodes",
                     colors = c("black", "red", "green"),
                     size = 400,
                     x = seq(0, 1, 0.01),
                     h = "h0",
                     lwd=1.3,
                     ...) {
  if(is.null(main)){
    hh <- "H0"
    if(h == "h1"){hh <- "H1"}
    main <- paste(model, " Model - ", hh, sep="")
  }
  er_line <- c()
  pa_line <- c()
  ws_line <- c()
  for(size in sizes){
      fp <- paste("../Code/ModelSelection/simulation.model.selection-", 
                  model,
                  "-", size,
                  "-result.data", sep = "")
      if(!file.exists(fp)){
        msg <- paste("File ", fp, " does not exist", sep="")
        warning(msg)
      }
      
      part <- read.csv(fp, header=TRUE)
      min_positions <- apply(part, 1, function(row) which.min(row))
      er_rate <- sum(min_positions == 1) / nrow(part)
      pa_rate <- sum(min_positions == 2) / nrow(part)
      ws_rate <- sum(min_positions == 3) / nrow(part)
      
      er_line <- c(er_line, er_rate)
      pa_line <- c(pa_line, pa_rate)
      ws_line <- c(ws_line, ws_rate)
  }
  
  par(mar = c(5, 4, 4, 8), xpd = TRUE)  # Increases the right margin
  plot(x=sizes, y = er_line, type="l", col=colors[1], main = main, xlab = xlab, ylab = ylab, ylim=ylim, lwd=lwd)
  lines(x = sizes, y = pa_line, col=colors[2], lwd=lwd)
  lines(x = sizes, y = ws_line, col=colors[3], lwd=lwd)
  
  legend("topright",  inset = c(-0.15, 0), # Position of the legend
         legend = c("ER", "BA", "WS"),  # Legend labels
         col = colors,                     # Colors for lines
         lty = 1,                          # Line type (solid)
         lwd = 2,                          # Line width
  )
}


Cairo::CairoSVG(file = "out/model-selection.svg",height = 9, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(3, 1))
run_plot("ERP", main = "Erdős–Rényi Model")
run_plot("PAP", main = "Barabási–Albert Model")
run_plot("WSP", main = "Watts–Strogatz Model")
dev.off()
