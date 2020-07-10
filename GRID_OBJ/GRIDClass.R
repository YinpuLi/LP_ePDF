source('GRID_OBJ/Data_Init.R')
source('GRID_OBJ/GRIDFunc.R')
source('GRID_OBJ/KS_criteria.R')



init_grid           = equal_width_grid(added_knots_num, domain_1, domain_2)

par_vec             = c(115.000000, 1.791759, 2.302585)
val_vec             = par2val(par_vec)# c(115, 121, 131)
val_vec             = validate_val_vec(new_val_vec = val_vec,
                                       Min         = domain_1,
                                       Max         = domain_2)
par_vec             = val2par(val_vec)

# the final x_grid should include some more points"
    # observed data
    # point of interest
    # left and right neighbor of point of interest 
    # a, M, b

x_grid              = update_grid(init_grid, c(obs_data, val_vec, interest_val, c(interest_val - 1e-6,
                                                                                  interest_val + 1e-6)))
KS_CI_adapt         = update_criteria(x_grid, obs_data)

# (2) the actual min and max of grid:
domain_min          = min(x_grid) 
domain_max          = max(x_grid)

# (3) the index of t in x_grid
interest_loc       = val2loc(interest_val, x_grid)

# (4) the index of (a, M, b) in x_grid
loc_vec            = val2loc(val_vec, x_grid)

# (5) the segment-width set
seg_set            = get_seg_widths(x_grid)

# (6) The maximum N I could use in later constraint calculation
N_max              = get_Nmax_by_grid(x_grid)



###### My Initial GRID class ######

GRID = list()
GRID$obs_data   = obs_data
GRID$domain_1   = domain_1
GRID$domain_2   = domain_2
GRID$n          = added_knots_num
GRID$init_grid  = init_grid
GRID$t          = interest_val
GRID$pars       = par_vec
GRID$vals       = val_vec
GRID$x_grid     = x_grid
GRID$KS_CI_adapt= KS_CI_adapt 
GRID$deltas     = seg_set
GRID$domain_min = domain_min
GRID$domain_max = domain_max
GRID$t_loc      = interest_loc
GRID$cols       = cols
GRID$locs       = loc_vec
GRID$N          = N_max
#GRID$update     = FALSE



create_grid_obj    = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_t            = NULL,
    new_obs_data     = NULL
){
    # create an empty new_grid_obj
    
    new_GRID         = list()
    
    if(is.null(new_obs_data)){
        new_GRID$obs_data   = obs_data
    } else {
        new_GRID$obs_data   = new_obs_data
    }
    
    
    new_GRID$domain_1   = ifelse(is.null(new_domain_1), domain_1, new_domain_1)
    new_GRID$domain_2   = ifelse(is.null(new_domain_2), domain_2, new_domain_2)
    new_GRID$n          = ifelse(is.null(new_n), added_knots_num, new_n)
    new_GRID$init_grid  = equal_width_grid(new_GRID$n, new_GRID$domain_1, new_GRID$domain_2)
    new_GRID$t          = ifelse(is.null(new_t), interest_val, new_t)
    
    if(is.null(new_pars)){
        new_GRID$pars   = par_vec
    } else {
        new_GRID$pars   = new_pars
    }
    
    new_GRID$vals       = par2val(new_GRID$pars)
    new_GRID$vals       = validate_val_vec(new_val_vec = new_GRID$vals,
                                           Min         = new_GRID$domain_1,
                                           Max         = new_GRID$domain_2)
    
    new_GRID$pars       = val2par(new_GRID$vals)
    
    
    new_GRID$x_grid     = update_grid(new_GRID$init_grid, 
                                      c(new_GRID$obs_data,
                                        new_GRID$vals,
                                        new_GRID$t))
    
    new_GRID$KS_CI_adapt= update_criteria(new_GRID$x_grid, new_GRID$obs_data)
    
    new_GRID$domain_min = min(new_GRID$x_grid)
    new_GRID$domain_max = max(new_GRID$x_grid)
    
    
    new_GRID$deltas     = get_seg_widths(new_GRID$x_grid)
    
    new_GRID$t_loc      = val2loc(new_GRID$t, new_GRID$x_grid)
    new_GRID$cols       = cols
    new_GRID$locs       = val2loc(new_GRID$vals, new_GRID$x_grid)
    new_GRID$N          = get_Nmax_by_grid(new_GRID$x_grid)
    
    return(new_GRID)
    
}

print_grid_obj  = function(grid_obj){
    
    cat('\n ********* My Current GRID Object: *********\n\n')
    
    print(grid_obj)
    
    cat('\n ********* End of GRID Object ********* \n\n')
    
}

is_equal       = function(grid_obj1, grid_obj2){
    FLAG       = TRUE
    grid_names = names(grid_obj1)
    for(i in 1:length(grid_names)){
        (flag   = (sum(grid_obj1[[grid_names[i]]] !=grid_obj2[[grid_names[i]]]) == 0))
        FLAG   = FLAG * flag
        if(!FLAG){return(FLAG)}
    }
    return(FLAG)
}

# print_grid_obj(GRID)

### Update GRID object adaptive to changes: 
update_grid_obj             = function(grid_obj){
    #if(grid_obj$update){
        grid_obj$init_grid  = equal_width_grid(grid_obj$n, grid_obj$domain_1, grid_obj$domain_2)
        grid_obj$vals       = par2val(grid_obj$pars)
        
        grid_obj$vals       = validate_val_vec(new_val_vec = grid_obj$vals,
                                               Min         = grid_obj$domain_1,
                                               Max         = grid_obj$domain_2)
        grid_obj$pars       = val2par(grid_obj$vals)
          
        grid_obj$x_grid     = update_grid(grid_obj$init_grid,
                                         c(
                                             grid_obj$obs_data, 
                                             grid_obj$vals,
                                             grid_obj$t
                                        )
                                        )
        grid_obj$KS_CI_adapt= update_criteria(grid_obj$x_grid,
                                              grid_obj$obs_data)
        grid_obj$domain_min = min(grid_obj$x_grid)
        grid_obj$domain_max = max(grid_obj$x_grid)
        
        grid_obj$t_loc      = val2loc(grid_obj$t, grid_obj$x_grid)
        grid_obj$locs       = val2loc(grid_obj$vals, grid_obj$x_grid)
        grid_obj$deltas     = get_seg_widths(grid_obj$x_grid)
        grid_obj$N          = get_Nmax_by_grid(grid_obj$x_grid)
        
    #}
    return(grid_obj)
}



# for later use:
# if t, a, M, b were to change,
# call update_grid_obj() and give the return value to GRID
# GRID = update_grid_obj(GRID)



