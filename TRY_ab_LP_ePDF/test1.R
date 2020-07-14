source('LP_fix.R')

# interest = 100

get_eF_fix(new_t     = 100,
           show_plot = T)

get_eF_fix(new_t     = 100,
           new_n     = 100,
           show_plot = T)

# try different (a, M, b)
get_eF_fix(new_t     = 100,
           new_pars  = val2par(c(101, 140, 150)),
           show_plot = T)
get_eF_fix(new_t     = 100,
           new_pars  = val2par(c(113, 126, 140)),
           show_plot = T)
get_eF_fix(new_t     = 100,
           new_pars  = val2par(c(80, 126, 140)),
           show_plot = T)
get_eF_fix(new_t     = 100,
           new_pars  = val2par(c(80, 126, 150)),
           show_plot = T)
get_eF_fix(new_t     = 100,
           new_pars  = val2par(c(80, 126, 160)),
           show_plot = T)
get_eF_fix(new_t     = 100,
           new_pars  = c(99, 0, 0),
           show_plot = T)
get_eF_fix(new_t     = 100,
           new_n     = 100,
           new_pars = c(99, -0.1, -0.01)) # status = 5
get_eF_fix(new_t     = 100,
           new_n     = 100,
           new_pars = c(99, -0.1, -0.01)) # lp_max$status = 5
get_eF_fix(new_t     = 100,
           new_n     = 101,
           new_pars = c(99, -0.1, -0.01)) # same pars, but by adding 1 initial knot, lp_max$status = 0

# end of try different (a, M, b)



get_eF_fix(new_t     = 100,
           new_pars  = c(99, 0, 0),
           show_plot = T)

get_eF_fix(new_t     = 100,
           new_pars  = c(99, 0, 0),
           new_n     = 100,
           show_plot = T)


get_eF_fix(new_t = 100,
           # new_n = 100,
            new_pars = c(99, -0.1, 0),
           show_plot = T)# 0.01392759 0.54834655


solveLP_fix(new_t = 100)#0.00000000 0.00316556

solveLP_fix(new_t = 100,
            new_n = 100)#0.00000000 0.01562989

solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, 0, 0))#0.0021315 0.2950725


solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.1, 0))# 0.01392759 0.54834655

solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.1, -0.01))
# Sth. wrong with LP MAX 
# lp_max$status =  5 
# [1] 0.01392899 0.00000000

# Question: what values should I return to when status = 5?


solveLP_fix(new_t     = 100,
            new_n     = 101,
            new_pars = c(99, -0.1, -0.01)) # same pars, but by adding 1 initial knot, lp_max$status = 0
# 0.01355848 0.31393855


## change new_pars slightly
solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.0000001, 0))# 0.01391422 0.54834650

solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.01, -0.001))#0.01391576 0.54834650


solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.02, -0.001))#0.01391714 0.54834650
## change n 
# solveLP_fix(new_t = 100,
#             new_n = 5000,
#             new_pars = c(99, -0.1, 0))#  0.01398153 0.30200599


solveLP_fix(new_t = 100,
            new_n = 1000,
            new_pars = c(99, -0.1, 0))#  0.01398153 0.30200599
get_eF_fix(
    new_t = 100,
    new_n = 500,
    new_pars = c(99, -0.1, 0),
    show_plot = T
)

get_eF_fix(
    new_t = 100,
    new_n = 100,
    new_pars = c(99, -0.1, 0),
    show_plot = T
)

solveLP_fix(new_t = 100,
            new_n = 700,
            new_pars = c(99, -0.1, 0))#  0.01397897 0.35151858
solveLP_fix(new_t = 100,
            new_n = 500,
            new_pars = c(99, -0.1, 0))#  0.01388781 0.38209742
solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.1, 0))# 0.01392759 0.54834655
solveLP_fix(new_t = 100,
            new_n = 10,
            new_pars = c(99, -0.1, 0))# 0.01269023 0.07685946
solveLP_fix(new_t = 100,
            new_n = 3,
            new_pars = c(99, -0.1, 0))# 0.01262497 0.05949516

# change domain setting
solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.1, 0))# 0.01392759 0.54834655

solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.1, 0),
            new_domain_1 = min(obs_data) - 10,
            new_domain_2 = max(obs_data) + 10)# 0.02415318 0.22988831

solveLP_fix(new_t = 100,
            new_n = 100,
            new_pars = c(99, -0.1, 0),
            new_domain_1 = min(obs_data),
            new_domain_2 = max(obs_data))# 0.03184468 0.13136882























# inetetest = 120


get_eF_fix(show_plot = T)


get_eF_fix(new_n     = 100,
           show_plot = T)
get_eF_fix(new_pars  = c(119, 0, 0),
           show_plot = T)
get_eF_fix(new_pars  = c(119, 0, 0),
           new_n     = 100,
           show_plot = T)

solveLP_fix() # 0.02594034 0.11044684
solveLP_max_min()

solveLP_fix(new_pars  = c(119, 0, 0)) # 0.03101572 0.54177301
solveLP_max_min(new_pars = c(119, 0, 0))
solveLP_fix(new_pars  = c(119, -1, 0)) # 0.03184713 1.06298182







# interest = 150

get_eF_fix(new_t     = 150,
           show_plot = T)

get_eF_fix(new_t     = 150,
           new_n     = 100,
           show_plot = T)

get_eF_fix(new_t     = 150,
           new_pars  = c(149, -0.1, 0),
           new_n     = 100,
           show_plot = T)

solveLP_fix(new_t    = 150) # 0.00000000 0.01023845

solveLP_fix(new_t    = 150,
            new_n    = 100) # 0.00000000 0.01094577

solveLP_fix(new_t     = 150,
            new_pars  = c(149.5, -1, -1),
            new_n     = 100) #  0.01043251 0.26498495

