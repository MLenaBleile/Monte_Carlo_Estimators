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
  
  
get_estimators = function(n,model){
  data = model(n)

  estimators = c(mean(data), median(data), trim.1(data), trim.2(data),w.1(data),w.2(data), lin.1(data), lin.2(data))
  return(estimators)
}

n_helper = function(n){
  N_estimators <<- c(N_estimators,get_estimators(as.numeric(n), model))
}

get_n_ests = function(model){
  model<<-model
  N_estimators <<- c()
  ns = c(10,20,30,50)
  for(kk in ns){
    N_estimators = c(N_estimators,get_estimators(kk, model))
  }
  return(N_estimators)
}

M_helper = function(i){
  #print(length(get_n_ests(model)))
  M_estimators[i,] <<- get_n_ests(model)
}

M_estimates = function(M, model, estimator_names){
  model<<-model
  M_estimators <<- matrix(rep(1:M, length(estimator_names)), c(M, length(estimator_names)))
  MM = 1:M
  lapply(MM, M_helper)
  M_estimators = as.data.frame(M_estimators)
  names(M_estimators) = estimator_names
  return(M_estimators)
}


###Write funcs to generate Bias and MSE###
###Need to write user defined fnc for Variance##
###Bias is just the mean right???####
MSE = function(x){
  return(mean(x)^2+sd(x)^2)
}
Var = function(x){
  return(sd(x)^2)
}

Bias_Var_MSE = function(data){
  out = data.frame(Bias = 1:length(data), Var = 1:length(data), MSE = 1:length(data))
  row.names(out) = colnames(data)
  out[,1] = apply(data,2,mean)
  out[,2] = apply(data,2,Var)
  out[,3] = apply(data,2,MSE)
  return(out)
}

get_estimates = function(d){
  ests_by_n = Bias_Var_MSE(d)
  ests_by_n$N = c(rep(10, length(estimate)), rep(20, length(estimate)), rep(30, length(estimate)), rep(50, length(estimate)))
  
  return(ests_by_n)
}

best_estimates = function(ests_by_n){
  ests_sub10 = ests_by_n[1:8,]
  best = ests_sub10[ests_sub10$MSE==min(ests_sub10$MSE),]
  ests_sub20 = ests_by_n[9:16,]
  best = rbind(best,ests_sub20[ests_sub20$MSE==min(ests_sub20$MSE),])
  ests_sub30 = ests_by_n[17:24,]
  #print(ests_sub30)
  best = rbind(best,ests_sub30[ests_sub30$MSE==min(ests_sub30$MSE),])
  ests_sub50 = ests_by_n[25:32,]
  #print(ests_sub50)
  best = rbind(best,ests_sub50[ests_sub50$MSE==min(ests_sub50$MSE),])
  return(best)
}
