source('LP_fix.R')
new_t = 100
#new_vals = c(99.99999, 100, 100.00001)
#new_pars = val2par(new_vals)

new_pars = c(99.999999, -15)
par2val(new_pars)

solveLP_fix(new_t = new_t,
            new_n_neighbors = 4,
            new_pars = new_pars)



get_eF_fix(new_t = new_t,
            new_n_neighbors = 4,
            new_pars = new_pars,
            show_plot = T)




new_t = 120
#new_vals = c(99.99999, 100, 100.00001)
#new_pars = val2par(new_vals)

new_pars = c(119.999999, -15)
par2val(new_pars)

solveLP_fix(new_t = new_t,
            new_n_neighbors = 4,
            new_pars = new_pars)

