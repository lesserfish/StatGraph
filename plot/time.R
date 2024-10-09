cil <- function(row_data){
  if(var(row_data[-1]) == 0) {
    return(c(row_data[2], row_data[2], row_data[2]))
  }
  else {
    test <- t.test(row_data[-1])
    cil <- test$conf.int[[1]]
    return(cil)
  }
}
cih <- function(row_data){
  if(var(row_data[-1]) == 0) {
    return(c(row_data[2], row_data[2], row_data[2]))
  }
  else {
    test <- t.test(row_data[-1])
    cih <- test$conf.int[[2]]
    return(cih)
  }
}


remove_outliers <- function(column) {
  # Calculate Q1 (25th percentile) and Q3 (75th percentile)
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  
  # Compute the IQR
  IQR <- Q3 - Q1
  
  # Define lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Return the column without outliers
  return(column[column >= lower_bound & column <= upper_bound])
}

run_plot <- function(method, 
                     sizes, 
                     main = NULL,
                     ...) {
  if(is.null(main)){
    main <- paste("")
  }

  directed_y <- list()
  undirected_y <- list()
  i <- 1
  for(size in sizes){
    fp <- paste("../Code/Benchmark/", 
                  "directed-time/bench.", method,
                  "-", size,
                  "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
    }
    data <- remove_outliers(read.csv(fp, header = TRUE, sep = " ")[,3])
    directed_y[[i]] <- data
    
    fp <- paste("../Code/Benchmark/", 
                "undirected-time/bench.", method,
                "-", size,
                "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
    }
    data <- remove_outliers(read.csv(fp, header = TRUE, sep = " ")[,3])
    undirected_y[[i]] <- data

    i <- i + 1
  } 
  directed_m   <- sapply(directed_y, mean)
  directed_cil <- sapply(directed_y, cil)
  directed_cih <- sapply(directed_y, cih)

  undirected_m <- sapply(undirected_y, mean)
  undirected_cil <- sapply(undirected_y, cil)
  undirected_cih <- sapply(undirected_y, cih)
  

  ymin <- min(c(undirected_m, directed_m))
  ymax <- max(c(undirected_m, directed_m))
  
  plot(x = sizes, y = directed_m, type="p", col="blue", 
       ylim=c(ymin, ymax), 
       ylab="Mean time in seconds", xlab="Graph size",
       main = main)
  points(x = sizes, y = undirected_m, type="p", col="red")
  polygon(c(sizes, rev(sizes)), c(directed_cil  , rev(directed_cih))  , col = "blue", border = NA, density = 35)
  polygon(c(sizes, rev(sizes)), c(undirected_cil, rev(undirected_cih)), col = "red" , border = NA, density = 35, angle = 10)
  
  legend("topleft",                             # Position of the legend
         legend = c("Directed", "Undirected"),  # Legend labels
         col = c("blue", "red"),                # Colors for lines
         lty = 1,                               # Line type (solid)
         lwd = 2,                               # Line width
  )
}

run_plot_separate <- function(method, 
                     sizes, 
                     main1 = "",
                     main2 = "",
                     ...) {
  directed_y <- list()
  undirected_y <- list()
  i <- 1
  for(size in sizes){
    fp <- paste("../Code/Benchmark/", 
                  "directed-time/bench.", method,
                  "-", size,
                  "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
    }
    data <- remove_outliers(read.csv(fp, header = TRUE, sep = " ")[,3])
    directed_y[[i]] <- data
    
    fp <- paste("../Code/Benchmark/", 
                "undirected-time/bench.", method,
                "-", size,
                "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
    }
    data <- remove_outliers(read.csv(fp, header = TRUE, sep = " ")[,3])
    undirected_y[[i]] <- data

    i <- i + 1
  } 
  directed_m <- sapply(directed_y, mean)
  directed_cil <- sapply(directed_y, cil)
  directed_cih <- sapply(directed_y, cih)

  undirected_m <- sapply(undirected_y, mean)
  undirected_cil <- sapply(undirected_y, cil)
  undirected_cih <- sapply(undirected_y, cih)
  

  ymin <- 0
  ymax <- max(c(directed_m))
  
  plot(x = sizes, y = directed_m, type="p", col="blue", 
       ylim=c(ymin, ymax), 
       ylab="Mean time in seconds", xlab="Graph size",
       main = main1)
  polygon(c(sizes, rev(sizes)), c(directed_cil, rev(directed_cih)), col = "blue", border = NA, density = 35)


  ymax <- max(c(undirected_m))

  plot(x = sizes, y = undirected_m, type="p", col="red",
       ylim=c(ymin, ymax), 
       ylab="Mean time in seconds", xlab="Graph size",
       main = main2)
  polygon(c(sizes, rev(sizes)), c(undirected_cil, rev(undirected_cih)), col = "red", border = NA, density = 35, angle = 10)
}

run_ball_plot <- function(method, 
                     sizes, 
                     main = NULL,
                     ...) {
  if(is.null(main)){
    main <- paste("")
  }

  directed_y <- list()
  undirected_y <- list()

  for(size in sizes){
    fp <- paste("../Benchmark/Code/Benchmark/", 
                  "directed-time/bench.", method,
                  "-", size,
                  "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
      directed_y[[size]] <- c()
    }
    else {
        data <- read.csv(fp, header = TRUE, sep = " ")[,3]
        directed_y[[size]] <- data
    }
    fp <- paste("../Benchmark/Code/Benchmark/", 
                "undirected-time/bench.", method,
                "-", size,
                "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
      undirected_y[[size]] <- c()
    }
    else {
        data <- read.csv(fp, header = TRUE, sep = " ")[,3]
        undirected_y[[size]] <- data
    }
  } 

  directed_m   <- sapply( Filter(Negate(is.null), directed_y)  , identity)
  undirected_m <- sapply( Filter(Negate(is.null), undirected_y), identity)

  ymin <- min(c(undirected_m, directed_m))
  ymax <- max(c(undirected_m, directed_m))

  xmin <- min(sizes)
  xmax <- max(sizes)

  plot(1, 1
       , type = "n"
       , ylim = c(ymin, ymax)
       , xlim = c(xmin, xmax)
       , ylab="Mean time in seconds", xlab="Graph size"
       , main=main)

  for(size in sizes){
      uys <- undirected_y[[size]]
      dys <- directed_y[[size]]

      points(x = rep(size, length(uys)), y = uys, col = rgb(1, 0, 0, alpha = 0.1), pch=15)
      points(x = rep(size, length(dys)), y = dys, col = rgb(0, 0, 1, alpha = 0.1), pch=15)
  }

  legend("topleft",                             # Position of the legend
         legend = c("Directed", "Undirected"),  # Legend labels
         col = c("blue", "red"),                # Colors for lines
         lty = 1,                               # Line type (solid)
         lwd = 2,                               # Line width
  )
}

run_ball_plot_separate <- function(method, 
                     sizes, 
                     main1 = NULL,
                     main2 = NULL,
                     ...) {

  directed_y <- list()
  undirected_y <- list()

  for(size in sizes){
    fp <- paste("../Benchmark/Code/Benchmark/", 
                  "directed-time/bench.", method,
                  "-", size,
                  "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
      directed_y[[size]] <- rep(0, 60)
    }
    else {
        data <- read.csv(fp, header = TRUE, sep = " ")[,3]
        directed_y[[size]] <- data
    }
    
    fp <- paste("../Benchmark/Code/Benchmark/", 
                "undirected-time/bench.", method,
                "-", size,
                "-result.data", sep = "")
    if(!file.exists(fp)){
      msg <- paste("File ", fp, " does not exist", sep="")
      warning(msg)
      undirected_y[[size]] <- rep(0, 60)
    }
    else {
        data <- read.csv(fp, header = TRUE, sep = " ")[,3]
        undirected_y[[size]] <- data
    }
  } 

  directed_m   <- sapply( Filter(Negate(is.null), directed_y)  , mean)
  undirected_m <- sapply( Filter(Negate(is.null), undirected_y), mean)

  ymin <- min(c(undirected_m))
  ymax <- max(c(undirected_m))
  xmin <- min(sizes)
  xmax <- max(sizes)
  plot(1, 1
       , type = "n"
       , xlim = c(xmin, xmax)
       , ylim = c(ymin, ymax)
       , ylab="Mean time in seconds", xlab="Graph size"
       , main=main1)

  for(size in sizes){
      uys <- undirected_y[[size]]
      dys <- directed_y[[size]]

      points(x = rep(size, length(uys)), y = uys, col = rgb(1, 0, 0, alpha = 0.1), pch=15)
  }


  ymin <- min(c(directed_m))
  ymax <- max(c(directed_m))

  plot(1, 1
       , type = "n"
       , ylim = c(ymin, ymax)
       , xlim = c(xmin, xmax)
       , ylab="Mean time in seconds", xlab="Graph size"
       , main=main2)

  for(size in sizes){
      uys <- undirected_y[[size]]
      dys <- directed_y[[size]]

      points(x = rep(size, length(dys)), y = dys, col = rgb(0, 0, 1, alpha = 0.1), pch=15)
  }

}

# Cairo::CairoSVG(file = "out/time.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
# par(mfrow = c(1, 2))
# run_plot("param.estimator", sizes = seq(100, 500, 5), "Parameter estimator")
# run_plot("model.selection", sizes = seq(100, 500, 5), "Model Selection")
# dev.off()
#
# Cairo::CairoSVG(file = "out/time_model_selection.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
# par(mfrow = c(1, 2))
# run_plot_separate("model.selection", sizes = seq(100, 500, 5), "Directed Graphs", "Undirected Graphs")
# dev.off()
#
# Cairo::CairoSVG(file = "out/time_pe.svg", height = 6,  width = 8, onefile = TRUE, bg = "transparent", pointsize = 12)
# par(mfrow = c(1, 1))
# run_ball_plot("param.estimator", sizes = seq(100, 500, 5), "Parameter Estimator")
# dev.off()
#
# Cairo::CairoSVG(file = "out/time_ms.svg", height = 6,  width = 8, onefile = TRUE, bg = "transparent", pointsize = 12)
# par(mfrow = c(1, 1))
# run_ball_plot("model.selection", sizes = seq(100, 500, 5), "Model Selection")
# dev.off()
#
# Cairo::CairoSVG(file = "out/time_ball.svg", height = 12,  width = 8, onefile = TRUE, bg = "transparent", pointsize = 12)
# par(mfrow = c(2, 1))
# run_ball_plot("param.estimator", sizes = seq(100, 500, 5), "Parameter Estimator")
# run_ball_plot("model.selection", sizes = seq(100, 500, 5), "Model Selection")
# dev.off()
#
# Cairo::CairoSVG(file = "out/time_model_selection.svg", width = 10, onefile = TRUE, bg = "transparent", pointsize = 12)
# par(mfrow = c(1, 2))
# run_ball_plot_separate("model.selection", sizes = seq(100, 500, 5), "Undirected Graphs", "Directed Graphs")
# dev.off()
#
Cairo::CairoSVG(file = "out/time_spectrum_ball.svg", width = 12, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(1, 1))
run_ball_plot("spectrum", sizes = seq(100, 500, 5), "")
dev.off()

Cairo::CairoSVG(file = "out/time_spectrum_ball-separate.svg", width = 10, height=12, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(2, 1))
run_ball_plot_separate("spectrum", sizes = seq(100, 500, 5), "Undirected Graphs", "Directed Graphs")
dev.off()
