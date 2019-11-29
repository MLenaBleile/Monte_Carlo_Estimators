library(psych)
library(stats)
library(SimDesign)

###Generation of estimators based on a sample
###re-write these all manually
###

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


trim.1 = function(x){return(rtrim(x, 0.1))}
trim.2 = function(x){return(rtrim(x, 0.2))}
w.1 = function(x){return(rwinsor(x, 0.1))}
w.2 = function(x){return(rwinsor(x,0.2))}
lin.1 = function(x){return(rlin(x, 0.1))}
lin.2 = function(x){return(rlin(x,0.2))}


#Generate data samples from models

n_cont.1 = function(n){return(as.numeric(rbinom(1,n, prob = 0.1)))}
n_cont.2 = function(n){return(as.numeric(rbinom(1,n,prob = 0.2)))}

R_O.1 = function(n){
  a = n_cont.1(n)
  return(c(rnorm(n-a), rnorm(a, sd=4)))
}
R_O.2 = function(n){
  a = n_cont.2(n)
  return(c(rnorm(n - a), rnorm(a, sd=4)))
}

F_O_1 = function(n){return(c(rnorm(n-1), rnorm(1, sd=4)))}
F_O_2 = function(n){return(c(rnorm(n-2), rnorm(2,sd=4)))}

#Generating Logistic variate using the inversion method
Logistic_e = function(n){return((-log((1-runif(n))/runif(n)))/(pi/sqrt(3)))}
laplace_es = function(n){return((rexp(n) - rexp(n))/sqrt(2))}


#function to calculate performances (Bias, Variance and MSE)
biass = mean(x)
MSE = function(x){
  return(biass^2+Varr(x))
}
Varr = function(x){
  return(sum((x - mean(x))^2)/(length(x) - 1))
}
computePerf = function(x){
  return(c(biass(x), Varr(x), MSE(x)))
}

