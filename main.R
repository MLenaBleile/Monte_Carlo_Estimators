setwd("C:/Users/MaryLena/Desktop/FALL2019/STAT 6324/MonteCarlo")
source("gen_estimators.R")


models = c(rnorm, R_O.1, R_O.2, F_O_1, F_O_2)

m_len = length(models)

estimate = c("Mean", "Median", "Trim1", "Trim2", "Wind1", "Wind2")
n_char = c(10,20,30,50)
estim_names = unlist(outer(estimate, n_char, paste))


 
M_estimates(10,rnorm, estim_names)

Bias_Var_MSE(d)
