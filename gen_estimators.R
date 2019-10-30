
library(psych)

n = 10


###Generation of estimators based on a sample

trim.1 = function(x){return(mean(x, trim = 0.1))}
trim.2 = function(x){return(mean(x, trim = 0.2))}
w.1 = function(x){return(winsor.mean(x, trim = 0.1))}
winsor.mean(x)


#Generate data samples from models

  n_cont.1 = function(n){return(rbinom(1,n, prob = 0.1))}
  n_cont.2 = function(n){return(rbinom(1,n,prob = 0.2))}
  R_O.1 = function(n){return(c(rnorm(n-n_cont.1), rnorm(n_cont.1, sd=4)))}
  R_O.2 = function(n){return(c(rnorm(n - n_cont.2), rnorm(n_cont.2, sd=4)))}
  F_O_1 = function(n){return(c(rnorm(n-1), rnorm(1, sd=4)))}
  F_O_2 = function(n){return(c(rnorm(n-2), rnorm(2,sd=4)))}
  
  
get_estimators = function(n,model){
  data = model(n)
  estimators = c(mean(data), median(data), trim.1(data), trim.2(data),w.1(data),winsor.mean(data))
  return(estimators)
}

get_n_ests = function(model){
  N_estimators = c()
  ns = c(10,20,30,50)
  for(n in ns){
    N_estimators = c(N_estimators, get_estimators(n, model))
    
  }
  return(N_estimators)
}

M_estimates = function(M, model, estimator_names){
  M_estimators = matrix(rep(1:M, length(estimator_names)), c(M, length(estimator_names)))
  for(i in 1:M){
    M_estimators[i,] = get_n_ests(model)
  }
  M_estimators = as.data.frame(M_estimators)
  names(M_estimators) = estimator_names
  return(M_estimators)
}

