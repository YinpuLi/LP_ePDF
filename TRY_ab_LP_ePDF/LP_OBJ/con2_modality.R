source('GRID_OBJ/GRIDClass.R')

#################################################################
##################  Constraint 2 : Mordality ####################
#################################################################

#########  Functions:
# get_modal_const_mat    : constraint matrix, of size         (N + 2 + loc_a - loc_b) by (N + 2)
# get_modal_const_dir    : direcition, vector of length       (N + 2 + loc_a - loc_b)
# get_mordal_const_rhs   : right hand side, vector of length  (N + 2 + loc_a - loc_b)

# To check and compare, we need to get the constraint results from F_hat and compare with the rhs
# get_ef_modal_const      : help to calculate the left hand side of constraint with current estimated pdf values 
# check_ef_modal          : return if there were at least one constraint violated, and print the first index of violated constraint



get_modal_const_mat       = function(
    grid_obj 
){
    num                   = grid_obj$N
    loc_vec               = grid_obj$locs
    
    mat                   = matrix(rep(0, (num + 2 + loc_vec[1] - loc_vec[2]) * (num + 2)),
                                   nrow = num + 2 + loc_vec[1] - loc_vec[2],
                                   ncol = num + 2)
    
    for(i in 1:loc_vec[1]){
            mat[i, i]     = -1
            mat[i, i + 1] = 1
    }
    
    # col_num_tracker
    j                     = num + 2 
    for(i in (num + 2 + loc_vec[1] - loc_vec[2]) : (loc_vec[1]+1)){
        
        mat[i, j - 1]     = -1
        mat[i, j    ]     = 1
        j                 = j - 1
    }
    
    return(mat)
    
}


get_modal_const_dir       = function(
    grid_obj
    
){
    loc_vec               = grid_obj$locs
    
    inner_knots_num       = grid_obj$N
    
    dir                   = rep("<=", inner_knots_num + 2 + loc_vec[1] - loc_vec[2])
    
    dir[1:loc_vec[1]]     = ">="
    
    return(dir)
    
}



get_modal_const_rhs       = function(
    grid_obj
){
    return(c(rep(0, grid_obj$N + 2 + grid_obj$locs[1] - grid_obj$locs[2])))
}


init_modal_mat            = get_modal_const_mat(GRID)
init_modal_dir            = get_modal_const_dir(GRID)
init_modal_rhs            = get_modal_const_rhs(GRID)
#View(cbind(init_modal_mat, init_modal_dir, init_modal_rhs))


get_ef_modal_const  = function(
    ef,                     # f_hat
    const_mat       
){
    if(dim(const_mat)[2] != length(ef)){
        print("Dimentions do not match!")
    }
    return(const_mat %*% ef)
}


check_ef_modal      = function(
    ef,                               # the current f hat
    const_mat,                        # should be the const_2 mat
    const_dir,                        # should be the const_2 dir
    const_rhs                         # should be the const_2 rhs
){
    const_cur_lhs   = get_ef_modal_const(ef, const_mat)  # the constraint calculated from
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
        cat(paste("The ", i - 1,"th ", "constainst in mode const is violated!","\n"))
        cat("const coeff:", const_mat[i - 1, ], "\n")
        #cat("ef:", ef[i -1], "\n")
        cat("lhs:", const_cur_lhs[i - 1], "\n")
        cat("dir: ", const_dir[i - 1], "\n")
        cat("rhs = ", const_rhs[i - 1], "\n")
    } else if((i == (length(const_dir))) && (!FLAG)){
        cat(paste("The last(", i - 1,"th) ", "constainst in mode const is violated!","\n"))
        cat("const coeff:", const_mat[i - 1, ], "\n")
        # cat("ef:", ef[i -1], "\n")
        cat("lhs:", const_cur_lhs[i - 1], "\n")
        cat("dir: ", const_dir[i - 1], "\n")
        cat("rhs = ", const_rhs[i - 1], "\n")

    }


    return(FLAG)
}



