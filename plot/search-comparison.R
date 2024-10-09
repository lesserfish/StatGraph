ternary_search <- function(criteria, parameters)
{
  if(is.null(parameters$min) || is.null(parameters$max) || is.null(parameters$eps)){
    stop("Wrong input for Ternary search. Parameters should be a list containing elements $min, $max and $eps, of type vector. The ternary search will then search parameter i in the interval [min[i], max[i]] using an epsilon of eps[i]. Read the documentation!")
  }
  
  min <- parameters$min
  max <- parameters$max
  eps <- parameters$eps
  points <- c()
  if(length(min) == 1)
  {
    lo <- min[1]
    hi <- max[1]
    leps <- eps[1]
    
    val1 <- 0
    val2 <- 0
    while(abs(hi - lo) > leps && lo < hi)
    {
      mid1 <- (2*lo + hi) / 3
      mid2 <- (2*hi + lo) / 3
      
      val1 <- criteria(c(mid1))
      val2 <- criteria(c(mid2))
      
      points <- c(points, mid1)
      points <- c(points, mid2)

      
      if(val1 < val2) {
        hi <- mid2
      }
      else {
        lo <- mid1
      }
    }
    return(points)
  }
}


criteria <- function(x){
  return(4*(x - 0.5)**2 + 0.7 * x + 3)
}

gpoints <- seq(0, 1, 0.01)
tpoints <- ternary_search(criteria, list(min=c(0), max=c(1), eps=c(0.01)))


Cairo::CairoSVG(file = "out/search-comparison.svg", width = 12, onefile = TRUE, bg = "transparent", pointsize = 12)
par(mfrow = c(1, 2))
x <- seq(0, 1, 0.01)
y <- criteria(x)
plot(x = x, y = y, t = "l", col="black", lwd=3, main = "Points evaluated during a grid search", xlab = "", ylab="")
points(gpoints, criteria(gpoints), col="red", pch = 19, cex=0.5)

plot(x = x, y = y, t = "l", col="black", lwd=3, main = "Points evaluated during a ternary search", xlab = "", ylab="")
points(tpoints, criteria(tpoints), col="red", pch = 19, cex=0.5)

dev.off()
