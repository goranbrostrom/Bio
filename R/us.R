## Uniform survival distribution functions.

## Density

dus <- function(x, from = 0, to = 1, p = 1 / (to - from)){
   if ((p < 0) | (p > 1)) return(NA)
   if (any(x <= from) | any(x > to)) return(NA)
   
   rep(p / (to - from), length(x))    
}

## CDF

pus <- function(x, from = 0, to = 1, p = 1 / (to - from),
                lower.tail = TRUE){
   if ((p < 0) | (p > 1)) return(NA)
   if (any(x <= from) | any(x > to)) return(NA)
   
    res <- p * x / (to - from)
    if (!lower.tail) res <- 1 - res
    res
}

## hazard function

hus <- function(x, from = 0, to = 1, p = 1 / (to - from)){
    dus(x, from, to, p) / pus(x, from, to, p, lower.tail = FALSE)  
}