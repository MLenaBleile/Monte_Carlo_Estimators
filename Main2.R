#a Simulation function that returns a dataframe of the perfomances(bias, variance, MSE) for M run, for a model of sample size n
est <- c(mean , median , trim.1 , trim.2 , w.1 , w.2 , lin.1 , lin.2)
simu = function(m, n, modelf){
  datalist = list()
  for (j in 1:8){
    lst = numeric()
    for (i in 1: m){
      samp = modelf(n)
      estim = est[j](samp)
        lst[i] <- estim
    }
    datalist[[j]] = computePerf(lst[i])
  }
  data_df = do.call(rbind, datalist)
  return(data_df)
}

simu(1000, 10, rnorm)
for(j in 1:8){
  print(j)
}

