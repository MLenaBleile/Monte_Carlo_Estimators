setwd("C:/Users/MaryLena/Desktop/FALL2019/STAT 6324/MonteCarlo")
source("gen_estimators.R")


estimate = c("Mean", "Median", "Trim1", "Trim2", "Wind1", "Wind2", "Lin1", "Lin2")
n_char = c(10,20,30,50)
estim_names = unlist(outer(estimate, n_char, paste))


M = 100000
d_norm = M_estimates(M,rnorm, estim_names)
d_RO = M_estimates(M, R_O.1, estim_names)
d_RO2 = M_estimates(M, R_O.2, estim_names)
d_FO1 = M_estimates(M, F_O_1, estim_names)
d_FO2 = M_estimates(M, F_O_2, estim_names)
d_Lap = M_estimates(M, laplace_es, estim_names)
d_Log = M_estimates(M, Logistic_e, estim_names)

get_rank_estimates(d_norm)
get_rank_estimates(d_RO)
get_rank_estimates(d_RO2)
get_rank_estimates(d_FO1)
get_rank_estimates(d_FO2)
get_rank_estimates(d_Lap)
get_rank_estimates(d_Log)
