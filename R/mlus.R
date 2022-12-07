## ML estimation of uniform data, typically with a fixed 
## right censoring time point.

## Application: Infant mortality on a log-cube scale.
## BP and BB.

mlus <- function(x, d){
   n <- length(x)
   if (length(d) != n) stop("Length mismatch")
   theta <- max(x)  ## Start value
   ivl <- c(theta, n * theta)
   
   loglik <- function(theta){
      sum((1 - d) * log(theta - x)) - n * log(theta)
   }
   
   dloglik <- function(theta){
      sum(d / (theta - x)) - n / theta
   }
   
   res <- optimize(loglik, interval = ivl, maximum = TRUE)
   p <- ivl[1] / res$maximum
   p
}