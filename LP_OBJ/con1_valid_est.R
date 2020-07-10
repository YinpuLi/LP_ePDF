source('GRID_OBJ/GRIDClass.R')

# This file aims to providing requirements of valid pdf and cdf


#################################################################
################  Constraint 1 : Valid Estimation ###############
#################################################################



########### Functions:
# get_valid_const_mat      : constraint coefficient matrix, of size         (N + 3) by (N + 2)
# get_valid_const_dir      : direction, vector of length                    (N + 3)
# get_valid_const_rhs      : right hand side coefficients, vector of length (N + 3)

# To check and compare, we need to get the constraint results from f_hat and compare with the rhs
# get_ef_valid_const       : help to calculate the left hand side of constraint with current estimated CDF values 
# ef_check_valid           : return if there were at least one constraint violated, and print the first index of violated constraint

get_valid_const_mat = function(
    grid_obj
){
    num             = grid_obj$N # N
    seg_set         = grid_obj$deltas
    
    mat             = diag(num + 2)
    vec             = rep(0, num + 2)
    vec[1]          = 1 + seg_set[1] / 2
    vec[num + 2]    = seg_set[num + 1] / 2
    
    
    for(i in 2:(num + 1)){
        vec[i]      = mean(c(seg_set[i - 1],
                             seg_set[i]))
    }
    
    mat             = rbind(mat, vec)
    
    return(mat)
}


get_valid_const_dir = function(
    grid_obj
){
    inner_knots_num = grid_obj$N
    
    dir1            = "="
    dir2            = rep(">=", inner_knots_num)
    dir3            = "="
    dir4            = "="
    return(c(dir1, dir2, dir3, dir4))
    
}

get_valid_const_rhs = function(
    grid_obj
){
    
    inner_knots_num = grid_obj$N
    rhs1            = rep(0, inner_knots_num + 2)
    rhs2            = 1
    return(c(rhs1, rhs2))
}

init_valid_const_mat = get_valid_const_mat(GRID)
init_valid_const_dir = get_valid_const_dir(GRID)
init_valid_const_rhs = get_valid_const_rhs(GRID)


get_ef_valid_const  = function(
    ef,                               # the current f hat
    const_mat                         # should be the const_1 matrix
){
    if(dim(const_mat)[2] != length(ef)){
        print("Dimentions do not match!")
    }
    return(const_mat %*% ef)
}




check_ef_valid      = function(
    ef,                               # the current F hat
    const_mat,                        # should be the const_1 mat
    const_dir,                        # should be the const_1 dir
    const_rhs                         # should be the const_1 rhs
){
    const_cur_lhs   = get_ef_valid_const(ef, const_mat)  # the constraint calculated from
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
        cat(paste("The ", i - 1,"th ", "constainst in valid_const is violated!","\n"))
        cat("const coeff:", const_mat[i - 1, ], "\n")
        cat("ef:", ef[i -1], "\n")
        cat("lhs:", const_cur_lhs[i - 1], "\n")
        cat("dir: ", const_dir[i - 1], "\n")
        cat("rhs = ", const_rhs[i - 1], "\n")
    }


    return(FLAG)
}







