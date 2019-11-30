#a Simulation function that returns a dataframe of the perfomances(bias, variance, MSE) for M run, for a model of sample size n
est <- list( 'mean'= mean , 'median'=median , 'trim.1'=trim.1 , 
             'trim.2'=trim.2 , 'w.1'=w.1, 'w.2'=w.2 , 'lin.1'=lin.1 , 'lin.2'=lin.2)
simu = function(m, n, modelf){
  datalist = list()
  for (j in 1:8){
    lst = numeric()
    for (i in 1: m){
      samp = modelf(n)
      #samp = as.data.frame(samp)
      estim = est[[j]](samp)
        lst[i] <- estim
    }
    #dname = as.character(quote(est[[j]]))
    datalist[[j]] = computePerf(lst)
  }
  data_df = do.call(rbind, datalist)
  dimnames(data_df)[[1]] = names(est)
  dimnames(data_df)[[2]] = c(paste('Bias', n), paste('Variance', n), paste('MSE', n))
  return(data_df)
}

#return simulation for the different sample sizes
sampSimu = function(m,modelf){
  lst = list()
  for (i in c(10,20,30,50)){
    df = simu(m,i,modelf)
    lst[[i]] = df
  }
  df1 = do.call(cbind, lst)
  return(df1)
}
#Return Simulation results for different models
sampSimu(10000,rnorm)
sampSimu(10000,R_O.1)
sampSimu(10000,R_O.2)
sampSimu(10000,F_O_1)
sampSimu(10000,F_O_2)
sampSimu(10000,Logistic_e)
sampSimu(10000,laplace_es)

