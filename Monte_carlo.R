# Estimators
# R trim estimator
rtrim <- function(x, r){
  n = length(x)
  est <- sum(sort(x)[(n*r + 1) : (n - n*r)])/(n - 2*(n*r))
  return(est)
}