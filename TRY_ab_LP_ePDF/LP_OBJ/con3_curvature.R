source('GRID_OBJ/GRIDClass.R')

#################################################################
#################  Constraint 3 : Curvature ###################
#################################################################

#########  Functions:
# get_curv_const_mat    : constraint matrix, of size        N by (N + 2)
# get_curv_const_dir    : direcition, vector of length      N
# get_curv_const_rhs    : right hand side, vector of length N

# To check and compare, we need to get the constraint results from f_hat and compare with the rhs
# get_ef_curv_const        : help to calculate the left hand side of constraint with current estimated PDF values 
# check_ef_curv            : return if there were at least one constraint violated, and print the first index of violated constraint

get_curv_const_mat        = function(
    grid_obj            
){
    seg_set               = grid_obj$deltas
    num                   = grid_obj$N
    
    mat                   = matrix(rep(0, num * (num + 2)), num, num + 2)
    
    for( i in 1:num){
        mat[i, i : (i + 2)] = c(seg_set[i + 1],
                                - seg_set[i] - seg_set[i + 1],
                                seg_set[i])
    }
    
    return(mat)
}


# we start from the fix inflection points:
get_curv_const_dir = function(
    grid_obj
){
    
    # get the index/loction of inflection points
    loc_vec               = grid_obj$locs
    loc_a                 = loc_vec[1]
    loc_b                 = loc_vec[2]
    
    #print(paste0("loc_vec =", loc_vec))
    inner_knots_num       = grid_obj$N
    
    V = c(
        rep(">=", loc_a),
        rep("<=", loc_b - loc_a - 1),
        rep(">=", inner_knots_num - loc_b + 1)
    )
    return(V)
}

get_curv_const_rhs = function(
    grid_obj
){
    return(c(rep(0, grid_obj$N)))
}





init_curv_mat = get_curv_const_mat(GRID)
init_curv_dir = get_curv_const_dir(GRID)
init_curv_rhs = get_curv_const_rhs(GRID)



#View(cbind(init_curv_mat, init_curv_dir, init_curv_rhs))




get_ef_unimodal_const   = function(
    ef,
    const_mat
){
    if(dim(const_mat)[2] != length(ef)){
        print("Dimentions do not match!")
    }
    return(const_mat %*% ef)
}


check_ef_unimodal   = function(
    ef,                               # the current F hat
    const_mat,                        # should be the const_3 mat
    const_dir,                        # should be the const_3 dir
    const_rhs                         # should be the const_3 rhs
){
    const_cur_lhs   = get_ef_unimodal_const(ef, const_mat)  # the constraint calculated from
    FLAG            = TRUE
    i               = 1

    while(FLAG && (i < length(const_cur_lhs) + 1)){
        lhs_temp    = round(const_cur_lhs[i], 3)
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
        if(i == (length(const_dir))){break}
        i           = i + 1
    }

    if(i < length(const_cur_lhs)){
        cat(paste("The ", i - 1,"th ", "constainst in unimodal_const is violated!","\n"))
        cat("const coeff:", const_mat[i - 1, ], "\n")
        #cat("ef:", ef[i -1], "\n")
        cat("lhs:", const_cur_lhs[i - 1], "\n")
        cat("dir: ", const_dir[i - 1], "\n")
        cat("rhs = ", const_rhs[i - 1], "\n")
    } else if((i == (length(const_dir))) && (!FLAG)){
        cat(paste("The last(", i - 1,"th) ", "constainst in unimodal_const is violated!","\n"))
        cat("const coeff:", const_mat[i - 1, ], "\n")
        # cat("ef:", ef[i -1], "\n")
        cat("lhs:", const_cur_lhs[i - 1], "\n")
        cat("dir: ", const_dir[i - 1], "\n")
        cat("rhs = ", const_rhs[i - 1], "\n")

    }


    return(FLAG)
}
















