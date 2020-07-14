source('LP_OBJ/con1_valid_est.R')
source('LP_OBJ/con2_modality.R')
source('LP_OBJ/con3_curvature.R')
source('LP_OBJ/con4_KS.R')


##############################################################
################ Combinig all constraints ####################
##############################################################



#########  Functions:
# get_const_mat    : constraint matrix, of size        (5N + 5 + loc_a - loc_b) by (N + 2)
# get_const_dir    : direcition, vector of length      (5N + 5 + loc_a - loc_b)
# get_const_rhs    : right hand side, vector of length (5N + 5 + loc_a - loc_b)



get_const_mat       = function(
    grid_obj
){
    mat1            = get_valid_const_mat(grid_obj)
    mat2            = get_modal_const_mat(grid_obj)
    mat3            = get_curv_const_mat(grid_obj)
    mat4            = get_KS_const_mat(grid_obj)
    return(rbind(mat1, mat2, mat3, mat4))
}


get_const_dir       = function(
    grid_obj
){
    
    dir1            = get_valid_const_dir(grid_obj)
    dir2            = get_modal_const_dir(grid_obj)
    dir3            = get_curv_const_dir(grid_obj)
    dir4            = get_KS_const_dir(grid_obj)
    
    return(c(dir1, dir2, dir3, dir4))
}


get_const_rhs       = function(
    grid_obj
){
    const.rhs1      = get_valid_const_rhs(grid_obj)
    const.rhs2      = get_modal_const_rhs(grid_obj)
    const.rhs3      = get_curv_const_rhs(grid_obj)
    const.rhs4      = get_KS_const_rhs(grid_obj)
    
    const.rhs  = c(const.rhs1, const.rhs2, const.rhs3, const.rhs4)
    
    return(const.rhs)
}


init_mat = get_const_mat(GRID)
init_dir = get_const_dir(GRID)
init_rhs = get_const_rhs(GRID)

#View(cbind(init_mat, init_dir, init_rhs))
# sum(init_mat != rbind(init_valid_const_mat, init_modal_mat, init_curv_mat, init_KS_mat))
# sum(init_dir != c(init_valid_const_dir, init_modal_dir, init_curv_dir, init_KS_dir))
# sum(init_rhs != c(init_valid_const_rhs, init_modal_rhs, init_curv_rhs, init_KS_rhs))
