rstack = function(x, digi=3, nest = 8){
  x = as.matrix(x)
  estimate = c("Arithmetic Mean", "Median", "Trimmed Mean (r=0.1)", "Trimmed Mean (r=0.2)", "Winsorized Mean (r=0.1)", "Winsorized Mean (r=0.2)", "Linearly Weighted Mean (r=0.1)", "Linearly Weighted Mean (r=0.2)")
  colnames(x) = NULL
  out=NULL
  out = rbind(x[,1:3], x[,4:6],x[,7:9], x[,10:12])
  #print(nrow(out))
  #print(out)
  out = as.data.frame(out)
  #print(nrow(out))
  #print(out)
  out$N = c(rep(10,nest), rep(20,nest), rep(30,nest), rep(50,nest))
  #print(nrow(out))
  colnames(out) = c("Bias","Variance", "MSE", "N")
  Estimator = data.frame(Estimator =rep(estimate,nest))[1:32,]
  #print(nrow(Estimator))
  out = round(out, digi)
  rownames(out)=NULL
  return(cbind(Estimator, out))
}

averagedata = function(x,y){
  d_av = data.frame(matrix(vector(), 8, 12),
                    stringsAsFactors=F)
  
  for(i in 1:8){
    for(j in 1:12){
      d_av[i,j] = mean(x[i,j], y[i,j])
    }
  }
  colnames(d_av) = colnames(x)
  rownames(d_av) = estimate
  return(d_av)
}
estimate = c("Arithmetic Mean", "Median", "Trimmed Mean (r=0.1)", "Trimmed Mean (r=0.2)", "Winsorized Mean (r=0.1)", "Winsorized Mean (r=0.2)", "Linearly Weighted Mean (r=0.1)", "Linearly Weighted Mean (r=0.2)")


load("C:/Users/MaryLena/Desktop/v15/.RData")
load("C:/Users/MaryLena/Desktop/v16/.RData")
load("C:/Users/MaryLena/Desktop/v17/.RData")

norm_av = averagedata(averagedata(d_norm16, d_norm17), d_norm15)
FO1_av = averagedata(averagedata(d_FO1_v16, d_FO1_v17), d_FO1_v15)
FO2_av = averagedata(averagedata(d_FO2_v16, d_FO2_v17), d_FO2_v15)
RO1_av = averagedata(averagedata(d_RO1_v16, d_RO1_v17), d_RO1_v15)
RO2_av = averagedata(averagedata(d_RO2_v16,d_RO2_v17),d_RO2_v15)
log_av = averagedata(averagedata(d_Log_v16, d_Log_v17), d_Log_v15)
lap_av = averagedata(averagedata(d_lap_v16, d_lap_v17), d_lap_v15)

norm_pretty = rstack(norm_av)
FO1_pretty = rstack(FO1_av)
FO2_pretty = rstack(FO2_av)
log_pretty = rstack(log_av)
RO1_pretty = rstack(RO1_av)
RO2_pretty = rstack(RO2_av)
lap_pretty = rstack(lap_av)

library(kableExtra)
