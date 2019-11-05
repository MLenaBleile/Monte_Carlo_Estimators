# Estimators
# R trim estimator
rtrim <- function(x, r){
  n = length(x)
  est <- sum(sort(x)[(n*r + 1) : (n - n*r)])/(n - 2*(n*r))
  return(est)
}

#r- winsorized mean
rwinsor<- function(x, r){
  n = length(x)
  x = sort(x)
  est <- (sum(x[(n*r + 2) : (n - n*r -1)]) + (n*r+1) * x[n*r + 1] + (n*r+1) * x[n - n*r] )/n
  return(est)
}

#r - linearly weighted mean
rlin <- function(x, r){
  n = length(x)
  x = sort(x)
  vec <- seq(1, (n/2 - n*r))
  total = 0
  for (i in 1:length(vec)){
    y = (2*i - 1) * (x[n*r+i] + x[n-n*r-i+1])
    total = total + y
  }
  est = (1/(2*(n/2 - n*r)^2)) * total
  return(est)
}

#test cases
xx<- c(12,14,23,16,24,30,1,2,4,5)
rwinsor(xx,0.0) == mean(xx)
rtrim(xx,0.0) == mean(xx)
