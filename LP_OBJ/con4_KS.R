source('GRID_OBJ/GRIDClass.R')


#################################################################
##############  Constraint 4 : Pass KS Criteria  ################
#################################################################

# The KS bands are step functions, so we need to update the bands adaptive to the new grid:
# that is, a new lower and upper bound of length same with the new grid.


########### Functions:
# get_KS_const_mat      : constraint coefficient matrix, of size         2N by (N + 2)
# get_KS_const_dir      : direction, vector of length                    2N
# get_KS_const_rhs      : right hand side coefficients, vector of length 2N


# To check and compare, we need to get the constraint results from F_hat and compare with the rhs
#  get_ef_KS_const       : help to calculate the left hand side of constraint with current estimated CDF values 
#  ef_check_ef_KS        : return if there were at least one constraint violated, and print the first index of violated constraint

get_KS_const_mat        = function(
    grid_obj
){
    num                 = grid_obj$N # N
    seg_set             = grid_obj$deltas
    
    mat                 = matrix(rep(0, num * (num + 2)), num, num + 2)
    
    mat[, 1]            = rep(1 + seg_set[1]/2, num)
    
    for(i in 1:num){
        mat[i, i+1]     = seg_set[i] / 2
        j               = 2
        while(j < i + 1){
            mat[i, j]   = 0.5 * (seg_set[j - 1] + seg_set[j])
            j           = j + 1
        }
        
        
    }
    
    mat_duo = rbind(mat, mat)
    
    return(mat_duo)
    
}



get_KS_const_dir       = function(
    grid_obj
){
    inner_knots_num    = grid_obj$N
    const.dir2 = c(rep(">=", inner_knots_num))
    const.dir3 = c(rep("<=", inner_knots_num))
    
    const.dir = c(const.dir2, const.dir3)
    
    return(const.dir)
}

get_KS_const_rhs       = function(
    grid_obj
){
    
    
    const.rhs.ci       = grid_obj$KS_CI_adapt
    num                = grid_obj$N
    
    const.rhs          = c(const.rhs.ci$lower[2: (num + 1)], const.rhs.ci$upper[2:(num+1)])
    return(const.rhs)
}


init_KS_mat = get_KS_const_mat(GRID)
init_KS_dir = get_KS_const_dir(GRID)
init_KS_rhs = get_KS_const_rhs(GRID)

#View(cbind(init_KS_mat, init_KS_dir, init_KS_rhs))

get_ef_KS_const   = function(
    ef,
    const_mat
){
    if(dim(const_mat)[2] != length(ef)){
        print("Dimentions do not match!")
    }
    return(const_mat %*% ef)
}

# check_ef_KS            : return if there were at least one constraint violated, and print the first index of violated constraint
check_ef_KS        = function(
    ef,
    const_mat,
    const_dir,
    const_rhs
){
    const_cur_lhs  = get_ef_KS_const(ef, const_mat)
    const_cur_lhs   = round(const_cur_lhs, 3)
    FLAG            = TRUE
    i               = 1

    while(FLAG && (i < length(const_cur_lhs) + 1)){
        lhs_temp    = const_cur_lhs[i]
        rhs_temp    = const_rhs[i]
        dir_temp    = const_dir[i]

        if(dir_temp == ">="){

            # compare if lhs >= rhs
            FLAG     = FLAG * (lhs_temp >= rhs_temp)

        } else if(dir_temp == "<="){

            # compare if lhs <= rhs
            FLAG    = FLAG * (lhs_temp <= rhs_temp)

        } else if(dir_temp == "<"){

            # compare if lhs < rhs
            FLAG    = FLAG * (lhs_temp < rhs_temp)


        } else if(dir_temp == ">"){

            # compare if lhs > rhs
            FLAG    = FLAG * (lhs_temp > rhs_temp)

        } else if(dir_temp == "="){

            # compare if lhs == rhs
            FLAG    = FLAG * (lhs_temp >= rhs_temp)

        }
        i           = i + 1
    }

    if(i < length(const_cur_lhs)){
        cat(paste("The ", i - 1,"th ", "constainst in KS_const is violated!","\n"))
        cat("const coeff:", const_mat[i - 1, ], "\n")
        cat("ef:", ef[i -1], "\n")
        cat("lhs:", const_cur_lhs[i - 1], "\n")
        cat("dir: ", const_dir[i - 1], "\n")
        cat("rhs: ", const_rhs[i - 1], "\n")
    }


    return(FLAG)
}



