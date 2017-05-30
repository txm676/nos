

###Internal functions (part 1)


#' @importFrom gmp factorialZ

###converts the user's net and pot_net objects to numeric data frames in which each columns starts at 1;
##can deal with factors.

form <- function(x){
  if (ncol(x) > 2) warning("More than 2 columns in a network matrix; only first 2 will be evaluated")
  colnames(x) <- c("X1", "X2")
  if (is.factor(x$X1)) x$X1 <- as.character(x$X1)
  if (is.factor(x$X2)) x$X2 <- as.character(x$X2)
  Dvin <- sort(unique(x[, 2]))
  Dvout <- sort(unique(x[, 1]))
  vall <- sort(union(Dvin, Dvout))
  for (i in 1:nrow(x)){
    w <- which(vall == x[i, 1])
    x[i, 1] <- w
    w <- which(vall == x[i, 2])
    x[i, 2] <- w
  }
  if (!is.numeric(x[, 1])) x <- data.frame(sapply(x, as.numeric))
  return(x)
}



#function needed within z2p function for the conversion from Z score to p value

erf <- function(x){
  sign <- ifelse(x >= 0, 1, -1)
  x <- abs(x)
  a1 <-  0.254829592
  a2 <- -0.284496736
  a3 <-  1.421413741
  a4 <- -1.453152027
  a5 <-  1.061405429
  p  <-  0.3275911
  t <- 1.0 / (1.0 + p * x)
  y <- 1.0 - ( ( ( ( (a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-x * x)
  return(sign * y)
}


#convert Z to p value

z2p <- function(z){
  z <- abs(z)
  p <- 0.5 * (1 + erf(z / sqrt(2)))
  return(2 * (1 - p))
}


#return number of combination of size k of n items. In n or k are too large (> roughly 170) factorial(n || k) produces
#Inf. In these cases gmp::factorialZ is used instead.

comb <- function(n, k){
  if (k > n) return(0)
  if (suppressWarnings(is.infinite(factorial(n))) || suppressWarnings(is.infinite(factorial(k)))){
    x <- gmp::factorialZ(n) %/% gmp::factorialZ(k) %/% gmp::factorialZ(n - k) ##uses gmp and integer division when n is > 170
    x <- as.numeric(x)
    if(is.infinite(x)) stop("n and/or k is too large for the internal comb function; there are likely too many species", "\n",
                            "in the dataset for calculations to be possible")
  } else{
    x <- factorial(n) / factorial (k) / factorial(n - k)
  }
  return(x)
}

#compute the probability that exactly j nodes are shared by the two focal nodes, with N1 and N2 being
#the actual numbers of neighbors for N1 and N2, and N the total number of possible neighbors.

cooc <- function(j, N, N1, N2){
  a <- comb(N1, j)
  b <- comb( (N - N1), (N2 - j))
  d <- comb(N, N2)
  return( (a * b) / d)
}



