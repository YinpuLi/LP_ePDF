source('GRID_OBJ/GRIDClass.R')

get_density_obj_coeff   = function(
    grid_obj            # GRID
   
){
    
    obj_coeff           = rep(0, length(grid_obj$x_grid))
    obj_coeff[grid_obj$t_loc] = 1
    
    return(obj_coeff)
}

init_obj = get_density_obj_coeff(GRID)


