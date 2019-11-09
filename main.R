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


Normal = best_estimates(get_estimates(d_norm))
Random_Outlier_1 = best_estimates(get_estimates(d_RO))
Random_Outlier_2 = best_estimates(get_estimates(d_RO2))
Fixed_Outlier_1 = best_estimates(get_estimates(d_FO1))
Fixed_Outlier_2 = best_estimates(get_estimates(d_FO2))
Laplace = best_estimates(get_estimates(d_Lap))
Logarithmic = best_estimates(get_estimates(d_Log))

