source('GRID_OBJ/Data_Init.R')
source('GRID_OBJ/GRIDFunc.R')
source('GRID_OBJ/KS_criteria.R')

#########
# TODO: rewrite is_equal

########

init_grid           = equal_width_grid(added_knots_num, domain_1, domain_2)

par_vec             = c(115.000000, 2.772589)
val_vec             = par2val(par_vec)# c(115, 121, 131)
val_vec             = validate_val_vec(new_val_vec = val_vec,
                                       Min         = domain_1,
                                       Max         = domain_2)

# need to do the par again, b may extend to 200
par_vec             = val2par(val_vec)

# the final x_grid should include some more points"
    # observed data
    # point of interest
    # some neighbors of point of interest
    # left and right neighbor of point of interest 
    # a, M, b
n_neighbors         = added_neighbor_num
t_neighbors         = get_neighbors(interest_val,
                                    added_neighbor_num,
                                    epsilon = 1e-4)

x_grid              = update_grid(init_grid, c(obs_data, val_vec, interest_val, t_neighbors))
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
GRID$n_neighbors= added_neighbor_num
GRID$epsilon    = 1e-4
GRID$t_neighbors= t_neighbors
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
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4
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
    new_GRID$n_neighbors= ifelse(is.null(new_n_neighbors), added_neighbor_num, new_n_neighbors)
    new_GRID$epsilon    = ifelse(is.null(epsilon), 1e-4, epsilon)
    new_GRID$t_neighbors= get_neighbors(new_GRID$t,
                                        new_GRID$n_neighbors,
                                        new_GRID$epsilon)
    
    
    if(is.null(new_pars)){
        new_GRID$pars   = par_vec
    } else {
        new_GRID$pars   = new_pars
    }
    
    new_GRID$vals       = par2val(new_GRID$pars)
    new_GRID$vals       = validate_val_vec(new_val_vec = new_GRID$vals,
                                           Min         = new_GRID$domain_1,
                                           Max         = new_GRID$domain_2)
    # need to do the $pars again, in case b is validated into 200
    new_GRID$pars       = val2par(new_GRID$vals)
    
    new_GRID$x_grid     = update_grid(new_GRID$init_grid, 
                                      c(new_GRID$obs_data,
                                        new_GRID$vals,
                                        new_GRID$t,
                                        new_GRID$t_neighbors))
    
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
    grid_names = sort(names(grid_obj1)) # names(grid_obj1)
    for(i in 1:length(grid_names)){
        print(i)
        if(grid_names[i] == "KS_CI_adapt"){
            next
        } 
        flag   = sum(grid_obj1[[grid_names[i]]] != grid_obj2[[grid_names[i]]]) == 0
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
        
        grid_obj$t_neighbors= get_neighbors(grid_obj$t, 
                                            grid_obj$n_neighbors,
                                            grid_obj$epsilon)
        grid_obj$vals       = par2val(grid_obj$pars)
        grid_obj$vals       = validate_val_vec(new_val_vec = grid_obj$vals,
                                               Min         = grid_obj$domain_1,
                                               Max         = grid_obj$domain_2)
        # need to do the $pars again, in case b is validated into 200
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
        grid_obj$deltas     = get_seg_widths(grid_obj$x_grid)
        grid_obj$t_loc      = val2loc(grid_obj$t, grid_obj$x_grid)
        grid_obj$locs       = val2loc(grid_obj$vals, grid_obj$x_grid)
        grid_obj$N          = get_Nmax_by_grid(grid_obj$x_grid)
        
    #}
    return(grid_obj)
}



# for later use:
# if t, a, M, b were to change,
# call update_grid_obj() and give the return value to GRID
# GRID = update_grid_obj(GRID)



