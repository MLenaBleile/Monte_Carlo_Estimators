#a Simulation function that returns a dataframe of the perfomances(bias, variance, MSE) for M run, for a model of sample size n
est <- list( 'mean'= mean , 'median'=median , 'trim.1'=trim.1 , 
             'trim.2'=trim.2 , 'w.1'=w.1, 'w.2'=w.2 , 'lin.1'=lin.1 , 'lin.2'=lin.2, 'harmonic mean' = hm)
simu = function(m, n, modelf){
  datalist = list()
  for (j in 1:9){
    lst = numeric()
    for (i in 1: m){
      samp = modelf(n)
      estim = est[[j]](samp)
        lst[i] <- estim
    }
    datalist[[j]] = computePerf(lst)
  }
  data_df = do.call(rbind, datalist)
  #dimnames(data_df)[[1]] = names(est)
  dimnames(data_df)[[2]] = c(paste('Bias', n), paste('Variance', n), paste('MSE',n))
  return(data_df)
}

#return simulation for the different sample sizes
df2 = data.frame(Estimator = names(est))
sampSimu = function(m,modelf){
  lst = list()
  for (i in c(10,20,30,50)){
    df = simu(m,i,modelf)
    lst[[i]] = df
  }
  df1 = do.call(cbind, lst)
  df1 = as.data.frame(df1)
  df1 = cbind(df2,df1)
  df1 = df1[order(df1$'MSE 50'),]
  return(df1)
}
#Return Simulation results for different models
normal = sampSimu(100,rnorm)
R01 = sampSimu(10000,R_O.1)
R02 = sampSimu(10000,R_O.2)
f01= sampSimu(10000,F_O_1)
f02 = sampSimu(10000,F_O_2)
logi = sampSimu(10000,Logistic_e)
lapl = sampSimu(10000,laplace_es)

