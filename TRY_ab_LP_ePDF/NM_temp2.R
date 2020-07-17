source('LP_fix.R')

get_initial_val = function(t, y = obs_data){
    obs_min     = min(y)
    obs_max     = max(y)
    
    if((t < obs_max) && (t > obs_min)){
        a_val   = max(median(c(obs_min, t)), t - 1)
        b_val   = min(median(c(obs_max, t)), t + 1)
        
        return(c(a_val, b_val))
    }
    return(c(115, 131))
}


my_interest = seq(from = 10, to = domain_max, by = 10)
ITERS       = 11

i = 1

### Maximizing f(t)


cat("\n i = ", i, "\n")
cat("\n t = ", my_interest[i], "\n")

my_initial_val  = get_initial_val(my_interest[i])
my_initial_par  = val2par(my_initial_val)

cat("\n (a, b) = (", my_initial_val, ")\n")
cat("\n (a_par, b_par) = (", my_initial_par, ")\n")

IMPR_max  = data.frame(#iters = seq(1,10, by = 1),
    NM    = rep(-1, ITERS),
    BFGS  = rep(-1, ITERS),
    CG    = rep(-1, ITERS),
    LB    = rep(-1, ITERS),
    SANN  = rep(-1, ITERS),
    TOTAL = rep(-1, ITERS))
TIME_max  = data.frame(
    NM    = rep(-1, ITERS),
    BFGS  = rep(-1, ITERS),
    CG    = rep(-1, ITERS),
    LB    = rep(-1, ITERS),
    SANN  = rep(-1, ITERS)
)  



iters       = 1



ptm        = proc.time()
fit_NM_max = optim(par = my_initial_par,
                   fn  = solveLP_max_min,
                   new_domain_1     = NULL,
                   new_domain_2     = NULL,
                   new_n            = NULL,
                   new_n_neighbors  = NULL,
                   new_t            = my_interest[i],
                   new_obs_data     = NULL,
                   epsilon          = 1e-4,
                   method  = "Nelder-Mead",
                   control = list(reltol = 1e-10
                                  # ,
                                  # maxit  = 5000,
                                  # alpha  = 5,
                                  # beta   = 0.25,
                                  # gamma  = 3.0
                   ))
ptm_NM_max = proc.time() - ptm

old_par     = fit_NM_max$par
old_val     = 0
old_mthd_val= 0

new_mthd_val= - fit_NM_max$value
mthd_imp    = new_mthd_val - old_mthd_val
old_mthd_val= new_mthd_val

IMPR_max[iters, 1] = mthd_imp
TIME_max[iters, 1] = ptm_NM_max[3]

cat("Best pars = ", fit_NM_max$par ,"\n")
cat("Best value = ", - fit_NM_max$val, "\n")


methods_set = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")


while(iters < ITERS + 1){
    cat("\n\n iteration =", iters, "\n")
    if(iters== 1){
        # iterates over the methods_set except NM
        for(m in 2:length(methods_set)){
            
            cat("methods = ", methods_set[m], "\n")
            # use the previous par 
            
            ptm = proc.time()
            
            fit_temp = optim(par = old_par,
                             fn  = solveLP_max_min,
                             new_domain_1     = NULL,
                             new_domain_2     = NULL,
                             new_n            = NULL,
                             new_n_neighbors  = NULL,
                             new_t            = my_interest[i],
                             new_obs_data     = NULL,
                             epsilon          = 1e-4,
                             method  = methods_set[m],
                             control = list(
                                 reltol = 1e-8
                             )
            )
            
            ptm_temp           = proc.time() - ptm
            TIME_max[iters, m] = ptm_temp[3]
            
            new_mthd_val       = - fit_temp$value
            mthd_imp           = new_mthd_val - old_mthd_val
            IMPR_max[iters, m] = mthd_imp
            
            if(mthd_imp  > 0){
                old_par  = fit_temp$par
                old_mthd_val   = new_mthd_val
                
            }
            
            cat("Best pars = ", fit_temp$par ,"\n")
            cat("Best value = ", - fit_temp$value, "\n")
            cat("Improved by ", mthd_imp, " with ", methods_set[m], "\n")
            
        }
        new_val      = - fit_temp$value
        if(new_val   > old_val + 0.01){
            cat("Making improvement by ", new_val - old_val)
            
            IMPR_max[iters, m + 1] = "NOT RUN"
            TIME_max[iters, m + 1] = "NOT RUN"
            
            IMPR_max$TOTAL[iters] = new_val - old_val
            old_val  = new_val
            iters  = iters + 1
            
            next
        } else {
            # no improvement by these methods, then call SANN
            cat("Not enough improvements by these methods, I am using SANN now.\n")
            
            ptm      = proc.time()
            fit_temp = optim(par = old_par,
                             fn  = solveLP_max_min,
                             new_domain_1     = NULL,
                             new_domain_2     = NULL,
                             new_n            = NULL,
                             new_n_neighbors  = NULL,
                             new_t            = my_interest[i],
                             new_obs_data     = NULL,
                             epsilon          = 1e-4,
                             method  = "SANN",
                             control = list(
                                 reltol = 1e-8
                             )
            )
            
            ptm_temp           = proc.time() - ptm
            TIME_max[iters, m + 1] = ptm_temp[3]
            
            new_mthd_val       = - fit_temp$value
            mthd_imp           = new_mthd_val - old_mthd_val
            IMPR_max[iters, m + 1] = mthd_imp
            
            if(mthd_imp  > 0){
                old_par  = fit_temp$par
                old_mthd_val   = new_mthd_val
                
            }
            
            cat("Best pars = ", fit_temp$par ,"\n")
            cat("Best value = ", - fit_temp$value, "\n")
            cat("Improved by ", mthd_imp, " with SANN", "\n")
            
            new_val  = - fit_temp$value
            cat("Making improvement by ", new_val - old_val)
            old_val  = new_val
            
            IMPR_max$TOTAL[iters] = new_val - old_val
            old_mthd_val = new_mthd_val
            iters  = iters + 1
            
            next
        }
        
        
        
    }
    
    for(m in 1:length(methods_set)){
        # iterates over the methods_set
        # use the previous par 
        cat("methods = ", methods_set[m], "\n")
        # use the previous par 
        
        ptm = proc.time()
        fit_temp = optim(par = old_par,
                         fn  = solveLP_max_min,
                         new_domain_1     = NULL,
                         new_domain_2     = NULL,
                         new_n            = NULL,
                         new_n_neighbors  = NULL,
                         new_t            = my_interest[i],
                         new_obs_data     = NULL,
                         epsilon          = 1e-4,
                         method  = methods_set[m],
                         control = list(
                             reltol = 1e-8
                         )
        )
        
        ptm_temp           = proc.time() - ptm
        TIME_max[iters, m] = ptm_temp[3]
        
        new_mthd_val       = - fit_temp$value
        mthd_imp           = new_mthd_val - old_mthd_val
        IMPR_max[iters, m] = mthd_imp
        
        if(mthd_imp  > 0){
            old_par  = fit_temp$par
            old_mthd_val   = new_mthd_val
            
        }
        
        cat("Best pars = ", fit_temp$par ,"\n")
        cat("Best value = ", - fit_temp$value, "\n")
        cat("Improved by ", mthd_imp, " with ", methods_set[m], "\n")
        
    }
    new_val      = - fit_temp$value
    if(new_val   > old_val + 0.01){
        cat("Making improvement by ", new_val - old_val)
        
        IMPR_max[iters, m + 1] = "NOT RUN"
        TIME_max[iters, m + 1] = "NOT RUN"
        IMPR_max$TOTAL[iters] = new_val - old_val
        old_val  = new_val
        
        iters  = iters + 1
        
        next
    } else {
        # no improvement by these methods, then call SANN
        cat("Not enough improvements by these methods, I am using SANN now.\n")
        
        ptm = proc.time()
        fit_temp = optim(par = old_par,
                         fn  = solveLP_max_min,
                         new_domain_1     = NULL,
                         new_domain_2     = NULL,
                         new_n            = NULL,
                         new_n_neighbors  = NULL,
                         new_t            = my_interest[i],
                         new_obs_data     = NULL,
                         epsilon          = 1e-4,
                         method  = "SANN",
                         control = list(
                             reltol = 1e-8
                         )
        )
        
        ptm_temp           = proc.time() - ptm
        TIME_max[iters, m + 1] = ptm_temp[3]
        
        new_mthd_val       = - fit_temp$value
        mthd_imp           = new_mthd_val - old_mthd_val
        IMPR_max[iters, m + 1] = mthd_imp
        
        if(mthd_imp  > 0){
            old_par  = fit_temp$par
            old_mthd_val   = new_mthd_val
            
        }
    
        cat("Best pars = ", fit_temp$par ,"\n")
        cat("Best value = ", - fit_temp$value, "\n")
        cat("Improved by ", mthd_imp, " with SANN", "\n")
        
        new_val  = - fit_temp$value
        cat("Making improvement by ", new_val - old_val)
        IMPR_max$TOTAL[iters] = new_val - old_val
        old_val  = new_val
        
        iters  = iters + 1
        
        next
    }
}

IMPR_max
IMPR_max_copy = IMPR_max
for(k in 1:nrow(IMPR_max)){
    for(j in 1:ncol(IMPR_max)){
        if(IMPR_max_copy[k, j] != "NOT RUN"){
            IMPR_max_copy[k, j] = round(as.numeric(IMPR_max[k, j]), 3)
        }
    }
}

IMPR_max_copy
TIME_max

fit_temp$par
par2val(fit_temp$par)
old_val


solveLP_max_min(new_par = fit_temp$par, new_t = my_interest[i])



# ########## Min f(t)
# 
# 
# 
# IMPR_min    = data.frame(NM    = rep(0, 10),
#                          BFGS  = rep(0, 10),
#                          CG    = rep(0, 10),
#                          LB    = rep(0, 10),
#                          TOTAL = rep(0, 10))
# 
# 
# 
# i = 10
# 
# 
# cat("\n i = ", i, "\n")
# cat("\n t = ", my_interest[i], "\n")
# 
# my_initial_par  = get_initial_par(my_interest[i])
# cat("\n (a, b) = (", my_initial_par, ")\n")
# 
# ptm        = proc.time()
# fit_NM_min = optim(par = my_initial_par,
#                    fn  = solveLP_min,
#                    new_domain_1     = NULL,
#                    new_domain_2     = NULL,
#                    new_n            = NULL,
#                    new_n_neighbors  = NULL,
#                    new_t            = my_interest[i],
#                    new_obs_data     = NULL,
#                    epsilon          = 1e-4,
#                    method  = "Nelder-Mead",
#                    control = list(reltol = 1e-10
#                                   # ,
#                                   # maxit  = 5000,
#                                   # alpha  = 5,
#                                   # beta   = 0.25,
#                                   # gamma  = 3.0
#                                   ))
# ptm_NM_min  = proc.time() - ptm
# 
# old_par     = fit_NM_min$par
# old_val     = fit_NM_min$value
# 
# cat("Best pars = ", fit_NM_min$par ,"\n")
# cat("Best value = ", fit_NM_min$value, "\n")
# 
# IMPR$NM[1]    = old_val - 0
# 
# 
# methods_set = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")
# iters       = 1
# while(iters < ITERS + 1){
#     cat("\n\n iter =", iters, "\n")
#     if(iters== 1){
#         # iterates over the methods_set except NM
#         for(m in 2:length(methods_set)){
#             
#             cat("methods = ", methods_set[m], "\n")
#             # use the previous par 
#             fit_temp = optim(par = old_par,
#                              fn  = solveLP_min,
#                              new_domain_1     = NULL,
#                              new_domain_2     = NULL,
#                              new_n            = NULL,
#                              new_n_neighbors  = NULL,
#                              new_t            = my_interest[i],
#                              new_obs_data     = NULL,
#                              epsilon          = 1e-4,
#                              method  = methods_set[m],
#                              control = list(
#                                  reltol = 1e-8
#                                  )
#                              )
#             old_par  = fit_temp$par
#             cat("Best pars = ", fit_temp$par ,"\n")
#             cat("Best value = ", fit_temp$value, "\n")
#         }
#         new_val      = fit_temp$value
#         if(new_val   < old_val){
#             cat("Making improvement by ", new_val - old_val)
#             old_val  = new_val
#             iters  = iters + 1
#             next
#         } else {
#             # no improvement by these methods, then call SANN
#             cat("No improvements by these methods, I am using SANN now.\n")
#             fit_temp = optim(par = old_par,
#                              fn  = solveLP_min,
#                              new_domain_1     = NULL,
#                              new_domain_2     = NULL,
#                              new_n            = NULL,
#                              new_n_neighbors  = NULL,
#                              new_t            = my_interest[i],
#                              new_obs_data     = NULL,
#                              epsilon          = 1e-4,
#                              method  = "SANN",
#                              control = list(
#                                  reltol = 1e-8
#                              )
#             )
#             
#             old_par  = fit_temp$par
#             cat("Best pars = ", fit_temp$par ,"\n")
#             cat("Best value = ", fit_temp$value, "\n")
#             
#             new_val  = fit_temp$value
#             cat("Making improvement by ", new_val - old_val)
#             old_val  = new_val
#             iters  = iters + 1
#             next
#         }
#         
#         IMPR$TOTAL[iter] = new_val - old_val
#         
#     }
#     
#     for(m in 1:length(methods_set)){
#         # iterates over the methods_set
#         # use the previous par 
#         cat("methods = ", methods_set[m], "\n")
#         
#         fit_temp = optim(par = old_par,
#                          fn  = solveLP_min,
#                          new_domain_1     = NULL,
#                          new_domain_2     = NULL,
#                          new_n            = NULL,
#                          new_n_neighbors  = NULL,
#                          new_t            = my_interest[i],
#                          new_obs_data     = NULL,
#                          epsilon          = 1e-4,
#                          method  = methods_set[m],
#                          control = list(
#                              reltol = 1e-8
#                          )
#         )
#         old_par  = fit_temp$par
#         cat("Best pars = ", fit_temp$par ,"\n")
#         cat("Best value = ", fit_temp$value, "\n")
#     }
#     
#     new_val      = fit_temp$value
#     if(new_val   < old_val){
#         cat("Making improvement by ", new_val - old_val)
#         old_val  = new_val
#         iters  = iters + 1
#         next
#     } else {
#         # no improvement by these methods, then call SANN
#         cat("\nNo improvements by these methods, I am using SANN now.\n")
#         fit_temp = optim(par = old_par,
#                          fn  = solveLP_min,
#                          new_domain_1     = NULL,
#                          new_domain_2     = NULL,
#                          new_n            = NULL,
#                          new_n_neighbors  = NULL,
#                          new_t            = my_interest[i],
#                          new_obs_data     = NULL,
#                          epsilon          = 1e-4,
#                          method  = "SANN",
#                          control = list(
#                              reltol = 1e-8
#                          )
#         )
#         
#         old_par  = fit_temp$par
#         cat("Best pars = ", fit_temp$par ,"\n")
#         cat("Best value = ", fit_temp$value, "\n")
#         
#         new_val  = fit_temp$value
#         cat("Making improvement by ", new_val - old_val)
#         old_val  = new_val
#         iters  = iters + 1
#         next
#     }
#     
#     IMPR$TOTAL[iter] = new_val - old_val
#     
# }
# 
# IMPR
























    
