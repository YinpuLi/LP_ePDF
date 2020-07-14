# new_t    = 10
# my_new_vals = c(9.999999999, 10, 10.000000001)
# my_new_pars = val2par(my_new_vals)
# solveLP_fix(new_pars = my_new_pars,
#             new_n = 10,
#             new_t = new_t)
# 
# new_n_neighbors = 4
# epsilon = 1e-5
# solveLP_fix(new_pars = my_new_pars,
#             new_n = 100,
#             new_n_neighbors = new_n_neighbors,
#             epsilon = epsilon,
#             new_t = new_t)



source('LP_fix.R')


new_t = 100
#new_vals = c(99.99999, 100, 100.00001)
#new_pars = val2par(new_vals)

new_pars = c(99.999999, -15, -15)
par2val(new_pars)

solveLP_fix(new_t = new_t,
            new_n_neighbors = 4,
            new_pars = new_pars)







new_t = 120
#new_vals = c(99.99999, 100, 100.00001)
#new_pars = val2par(new_vals)

new_pars = c(119.999999, -15, -15)
par2val(new_pars)

solveLP_fix(new_t = new_t,
            new_n_neighbors = 4,
            new_pars = new_pars)

