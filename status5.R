source('LP_fix.R')

my_new_pars = c(119.771907, -1.478006, -1.278538)


# n = 3

my_new_vals = par2val(my_new_pars)
solveLP_max_min(new_pars = my_new_pars,
                new_n = 3)
# test step by step
new_pars = my_new_pars
new_n    = 3

new_domain_1     = NULL
new_domain_2     = NULL
new_t            = NULL
new_obs_data     = NULL

grid_obj         = create_grid_obj(new_pars, 
                                   new_domain_1,
                                   new_domain_2, 
                                   new_n,
                                   new_t,
                                   new_obs_data)

print_grid_obj(grid_obj)
grid_obj$x_grid[9] == grid_obj$x_grid[10]

obj_coeff   = get_density_obj_coeff(grid_obj)
const_mat   = get_const_mat(grid_obj)
const_dir   = get_const_dir(grid_obj)
const_rhs   = get_const_rhs(grid_obj)

obj_coeff_adpt  = - obj_coeff


lp_min          = lp("min", obj_coeff_adpt, const_mat, const_dir, const_rhs)
lp_min$solution
lp_min$status
lp_min$objval

# load("C:/Users/yinpu/Box/Test_pdf_based - TestVesion/status5.RData")


# n = 10

my_new_vals = par2val(my_new_pars)
solveLP_max_min(new_pars = my_new_pars,
                new_n = 10)

# test step by step
new_pars = my_new_pars
new_n    = 10

new_domain_1     = NULL
new_domain_2     = NULL
new_t            = NULL
new_obs_data     = NULL

grid_obj         = create_grid_obj(new_pars, 
                                   new_domain_1,
                                   new_domain_2, 
                                   new_n,
                                   new_t,
                                   new_obs_data)

print_grid_obj(grid_obj)
grid_obj$x_grid[13] == grid_obj$x_grid[14]

obj_coeff   = get_density_obj_coeff(grid_obj)
const_mat   = get_const_mat(grid_obj)
const_dir   = get_const_dir(grid_obj)
const_rhs   = get_const_rhs(grid_obj)

obj_coeff_adpt  = - obj_coeff


lp_min          = lp("min", obj_coeff_adpt, const_mat, const_dir, const_rhs)
lp_min$solution
lp_min$status
lp_min$objval





######################################################
####### density(100)
new_t    = 100
my_new_pars = c(99.00, -0.10, -0.01)


# n = 100

par2val(my_new_pars)
solveLP_max_min(new_pars = my_new_pars,
                new_n = 100,
                new_t = new_t)
# this is fine
# test step by step
new_pars = my_new_pars
new_n    = 100


new_domain_1     = NULL
new_domain_2     = NULL
new_t            = NULL
new_obs_data     = NULL

grid_obj         = create_grid_obj(new_pars = my_new_pars, 
                                   new_domain_1,
                                   new_domain_2, 
                                   new_n = 100,
                                   new_t = 100,
                                   new_obs_data)

print_grid_obj(grid_obj)

obj_coeff   = get_density_obj_coeff(grid_obj)
const_mat   = get_const_mat(grid_obj)
const_dir   = get_const_dir(grid_obj)
const_rhs   = get_const_rhs(grid_obj)

obj_coeff_adpt  = - obj_coeff


lp_min          = lp("min", obj_coeff_adpt, const_mat, const_dir, const_rhs)
lp_min$solution
lp_min$status
lp_min$objval


