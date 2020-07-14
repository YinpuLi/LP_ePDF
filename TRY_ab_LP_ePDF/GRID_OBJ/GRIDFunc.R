############ Helper functions in initializing the GRID OBJ ###################
# equal_width_grid(n, Min, Max) : to get equal widths init_grid


# par2val(pars_vec)             : to get values from parameters
#                               a = par1
#                               M = a + exp(par2)
#                               b = M + exp(par3)

# val2par(vals_vec)             : to get parameters from values
#                               par1 = a
#                               par2 = log(M - a)
#                               par3 = log(b - M)


# val2loc(vals, grid)           : to get index/indices of certain values in current grid setting

# update_grid(init_grid, added_vals) 
#                               : to get the real grid in use, formed by initial grid and some more points

# get_seg_widths(grid)          : to get the segment lengths for the current grid.

# get_Nmax_by_grid/seg_set(grid): to get the nunmber of inner grid knots

# validate_val_vec(c(a,M,b), Min, Max )
#                               : to return the validated values for a, M, b

# get_neighbors(t, n2, epsilon = 1e-4)
#                               : to get a vector of length n2(= added_neighbor_num), with values
#                               : [..., t - 2* epsilon, t - epsilon, t, t + epsilon, t+ 2*epsilon,...]	


# TODO: valdiate_loc_vec

equal_width_grid    = function(
    n,          # added_knots_num
    Min_val ,   # domain_1
    Max_val     # domain_2
    
){
    return(seq(Min_val, Max_val, length.out = n))
}

par2val             = function(pars){
    var_vec         = pars[1]
    for(i in 2:length(pars)){
        val_temp    = tail(var_vec, 1) + exp(pars[i])
        var_vec     = c(var_vec, val_temp)
    }
    
    return(var_vec)
}

val2par             = function(vals){
    pars            = vals[1]
    for(i in 2:length(vals)){
        par_temp    = log(vals[i] - vals[i-1])
        pars        = c(pars, par_temp)
    }
    return(pars)
}

val2loc             = function(
    values,  # val_vec
    grid     # x_grid
){
    loc             = NULL
    for(i in 1:length(values)){
        cur_val     = values[i]
        cur_loc     = which(grid == cur_val)
        
        loc         = c(loc, cur_loc)
    }
    
    return(loc)
}

validate_val_vec    = function(
    new_val_vec,
    Min        ,
    Max      
){
    valid_val_vec   = sort(new_val_vec)         # make a copy of the input, with correct order
    
    if(
        valid_val_vec[2] <= Min
    ){  valid_val_vec     = c(Min + 0.01, Min + 0.02)
    }
    
    if(
        valid_val_vec[1] <= Min
    ){
        valid_val_vec[1]  = min(Min + 0.01,
                                median(c(valid_val_vec[2], Min)))
    }
    if(
        valid_val_vec[1] >= Max
    ){
        valid_val_vec     = c(Max - 0.02, Max - 0.01)
    }
    if(
        valid_val_vec[2] >= Max
    ){
        valid_val_vec[2]  = max(Max - 0.01,
                                median(c(valid_val_vec[1], Max)))
    }
    
    # if(!check_elems(new_val_vec, valid_val_vec)){
    #     cat("Current val vec =", new_val_vec, "is not valid, ", "\n")
    #     cat("Using valid val vec = ", valid_val_vec, "instead!", "\n")
    # }
    
    return(valid_val_vec)
}

update_grid         = function(
    old_grid,    # init_grid
    added_vals   # a vector of values should be added: t, a, M, b, obs_data
                 # # added_vals = c(obs_data, t, a, M, b)
){
    new_grid        = unique(sort(c(old_grid, added_vals)))
    return(new_grid)
}


get_seg_widths     = function(
    grid
){
    delta_set      = NULL
    for(i in 1:(length(grid) - 1)){
        delta      = grid[i + 1] - grid[i]
        delta_set  = c(delta_set, delta)
    }
    return(delta_set)
}

get_Nmax_by_grid   = function(grid){
    return(length(grid) - 2)
}

get_Nmax_by_segset = function(segs){
    return(length(segs) - 1)
}


get_neighbors      = function(
    t,             # interest_val
    n,            # added_nerighbor_num
    epsilon        = 1e-4
){
    neighbors      = t
    for(i in 1:n){
        temp_right = t + i * epsilon
        temp_left  = t - i * epsilon
        
        neighbors  = c(temp_left, neighbors, temp_right)
        
    }
    return(neighbors)
    
}







