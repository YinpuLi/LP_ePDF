source('LP_fix.R')

my_interest = seq(from = 10, to = domain_max, by = 10)

par_MIN     = NULL
val_MIN     = NULL


for(i in 1:length(my_interest)){
    cat("\n i = ", i, "\n")
    cat("\n t = ", my_interest[i], "\n")
    
    
    
    fit_NM     = optim(par = my_initial_par,
                       fn  = solveLP_min,
                       new_domain_1     = NULL,
                       new_domain_2     = NULL,
                       new_n            = NULL,
                       new_n_neighbors  = NULL,
                       new_t            = my_interest[i],
                       new_obs_data     = NULL,
                       epsilon          = 1e-4,
                       method  = "Nelder-Mead",
                       control = list(reltol = 1e-10))
    
    cat("Best pars = ", fit_NM$par ,"\n")
    cat("Best value = ", fit_NM$val, "\n")
    
    par_MIN   = rbind(par_MIN, fit_NM$par)
    val_MIN   = rbind(val_MIN, fit_NM$val)
    
    
}

MIN_result  = cbind(my_interest, par_MIN, val_MIN)
colnames(MIN_result) = c("t", "a", "b", "Minimum value")
MIN_result



par_MAX     = NULL
val_MAX     = NULL

for(i in 1:length(my_interest)){
    cat("\n i = ", i, "\n")
    cat("\n t = ", my_interest[i], "\n")
    
    fit_NM     = optim(par = my_initial_par,
                       fn  = solveLP_max_min,
                       new_domain_1     = NULL,
                       new_domain_2     = NULL,
                       new_n            = NULL,
                       new_n_neighbors  = NULL,
                       new_t            = my_interest[i],
                       new_obs_data     = NULL,
                       epsilon          = 1e-4,
                       method  = "Nelder-Mead",
                       control = list(reltol = 1e-10))
    
    cat("Best pars = ", fit_NM$par ,"\n")
    cat("Best value = ", fit_NM$val, "\n")
    
    par_MAX   = rbind(par_MAX, fit_NM$par)
    val_MAX   = rbind(val_MAX, fit_NM$val)
    
    
}

MAX_result  = cbind(my_interest, par_MIN, - val_MIN)
colnames(MAX_result) = c("t", "a", "b", "Maximum value")
MAX_result

