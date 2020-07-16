source('LP_fix.R')

my_val_vec = c(100, 140)
(my_par_vec = val2par(my_val_vec))

solveLP_fix(new_pars         = my_par_vec,
            new_domain_1     = 0,
            new_domain_2     = 200,
            new_n            = 10,
            new_n_neighbors  = NULL,
            new_t            = 180,
            new_obs_data     = NULL,
            epsilon          = 1e-4)

solveLP_fix(new_pars         = my_par_vec,
            new_domain_1     = NULL,
            new_domain_2     = NULL,
            new_n            = NULL,
            new_n_neighbors  = NULL,
            new_t            = 180,
            new_obs_data     = NULL,
            epsilon          = 1e-6)
