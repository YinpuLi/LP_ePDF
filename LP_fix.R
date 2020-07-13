source('LP_OBJ/dens_obj.R')
source('LP_OBJ/constraints.R')

######### Functions:
# solveLP_fix: provide the min and max in density estimation problem
# solveLP_min: provide the min in density estimation problem
# solveLP_max: provide the max in density estimation problem
# solveLP_max_min: trying to solve LP_max problem, but adaptive to Nelder-Mead algorithm, 
                #  as NM minimize functions by default,
                #  we should maiximize f(x) by minimizing -f(x)
                #  that is, lp_min(-obj func) 
# get_eF_min : provide the estimated CDF(F hat) in minLP (density estimation) problem
# get_eF_max:  provide the estimated CDF(F hat) in maxLP (density estimation) problem
# get_eF_fix : provide the estimated CDF(F hat) in both mibLP and maxLP (density estimation) problem





get_increments    = function(
    f_hat,
    grid_obj
){
    # k = 2 to N+2
    segs          = grid_obj$deltas
    f_hat1        = f_hat[1 : (grid_obj$N + 1 )] # delta_1 to delta_
    f_hat2        = f_hat[-1]
    
    incre         = segs/2 * (f_hat1 + f_hat2)
    
    return(incre)
}

get_cdf           = function(
    f_hat,
    grid_obj
){
    F_hat         = 0
    increment     = get_increments(f_hat, grid_obj)
    sum           = 0   # cumulative sum
    
    for(i in 1:length(increment)){
        sum       = sum + increment[i]
        F_hat     = c(F_hat, sum)
    }
    
    return(F_hat)
}


solveLP_fix          = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4
){
    
    grid_obj         = create_grid_obj(new_pars, 
                                       new_domain_1,
                                       new_domain_2, 
                                       new_n,
                                       new_n_neighbors,
                                       new_t,
                                       new_obs_data,
                                       epsilon)
    
    grid_obj_copy    = grid_obj
    
    obj_coeff   = get_density_obj_coeff(grid_obj)
    const_mat   = get_const_mat(grid_obj)
    const_dir   = get_const_dir(grid_obj)
    const_rhs   = get_const_rhs(grid_obj)
    
    
    lp_min          = lp("min", obj_coeff, const_mat, const_dir, const_rhs)
    lp_max          = lp("max", obj_coeff, const_mat, const_dir, const_rhs)
    
    if(lp_min$status != 0){
        cat("Sth. wrong with LP MIN", "\n")
        cat("lp_min$status = ", lp_min$status, "\n")
        if(lp_min$status == 2){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN does not converge!", "\n", 
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
        } else if(lp_min$status == 5){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN: Numerical failure encountered!", "\n",
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
            print_grid_obj(grid_obj)
            #save(grid_obj, file = "status5.RData")
        }
    }
    if(lp_max$status != 0){
        cat("Sth. wrong with LP MAX", "\n")
        cat("lp_max$status = ", lp_max$status, "\n")
        if(lp_max$status == 2){
            small_num    = -1e6
            lp_max$objval   = small_num
            cat("LP MAX does not converge!", "\n", 
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            
        } else if(lp_max$status == 5){
            small_num       = -1e6
            lp_max$objval   = small_num
            cat("LP MAX: Numerical failure encountered!", "\n",
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            print_grid_obj(grid_obj)
            #save(grid_obj, file = "status5.RData")
            
        }
    }
    
    return(c(lp_min$objval, lp_max$objval))
}





solveLP_min          = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4
){
    grid_obj         = create_grid_obj(new_pars, 
                                       new_domain_1,
                                       new_domain_2, 
                                       new_n,
                                       new_n_neighbors,
                                       new_t,
                                       new_obs_data,
                                       epsilon)
    
    grid_obj_copy    = grid_obj
    
    obj_coeff   = get_density_obj_coeff(grid_obj)
    const_mat   = get_const_mat(grid_obj)
    const_dir   = get_const_dir(grid_obj)
    const_rhs   = get_const_rhs(grid_obj)
    
    
    lp_min          = lp("min", obj_coeff, const_mat, const_dir, const_rhs)
    
    if(lp_min$status != 0){
        cat("Sth. wrong with LP MIN", "\n")
        cat("lp_min$status = ", lp_min$status, "\n")
        if(lp_min$status == 2){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN does not converge!", "\n", 
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
        }
    }
    

    #print(c(pars, lp_min$objval))

    return(lp_min$objval)


}

solveLP_max          = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4
){
    grid_obj         = create_grid_obj(new_pars, 
                                       new_domain_1,
                                       new_domain_2, 
                                       new_n,
                                       new_n_neighbors,
                                       new_t,
                                       new_obs_data,
                                       epsilon)
    
    
    grid_obj_copy    = grid_obj
    
    obj_coeff   = get_density_obj_coeff(grid_obj)
    const_mat   = get_const_mat(grid_obj)
    const_dir   = get_const_dir(grid_obj)
    const_rhs   = get_const_rhs(grid_obj)
    
    lp_max      = lp("max", obj_coeff, const_mat, const_dir, const_rhs)


    if(lp_max$status != 0){
        cat("Sth. wrong with LP MAX", "\n")
        cat("lp_max$status = ", lp_max$status, "\n")
        if(lp_max$status == 2){
            small_num    = -1e6
            lp_max$objval   = small_num
            cat("LP MAX does not converge!", "\n", 
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            
        } else if(lp_max$status == 5){
            small_num       = -1e6
            lp_max$objval   = small_num
            cat("LP MAX: Numerical failure encountered!", "\n",
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            
        }
    }
    
    #print(paste0(pars, lp_max$objval))

    return(lp_max$objval)
}



solveLP_max_min      = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4
){
    grid_obj         = create_grid_obj(new_pars, 
                                       new_domain_1,
                                       new_domain_2, 
                                       new_n,
                                       new_n_neighbors,
                                       new_t,
                                       new_obs_data,
                                       epsilon)
   
    
    obj_coeff   = get_density_obj_coeff(grid_obj)
    const_mat   = get_const_mat(grid_obj)
    const_dir   = get_const_dir(grid_obj)
    const_rhs   = get_const_rhs(grid_obj)
    
    obj_coeff_adpt  = - obj_coeff
    
    
    lp_min          = lp("min", obj_coeff_adpt, const_mat, const_dir, const_rhs)

    if(lp_min$status != 0){
        cat("\nSth. wrong with LP MIN", "\n")
        cat("lp_min$status = ", lp_min$status, "\n")
        if(lp_min$status == 2){
            big_num      = 1e6
            lp_min$objval = big_num
           cat("LP MIN does not converge!", "\n",
              "I am using a SUPER BIG number as objval = ", big_num, "\n")

        } else if(lp_min$status == 5){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN: Numerical failure encountered!", "\n",
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
            print_grid_obj(grid_obj)
            save(grid_obj, file = "status5.RData")
        }
    }
    #print(paste0(pars, lp_min$objval))
    cat("\nMAX_MIN val =", lp_min$objval, "\n")
    
    return(lp_min$objval)


}


# # I am writing a new greedy function
# solveLP_greedy_new  = function(
#     inner_knots_num = N,
#     domain_min      = domain1,
#     domain_max      = domain2,
#     x_grid          = my_grid,
#     interest        = 100,
#     obs_data        = y
#     
# ){
#     
#     obj.coeff       = get_density_obj_coeff(inner_knots_num, interest, domain_min, domain_max)
#     const.mat       = get_const_mat(inner_knots_num)
#     const.rhs       = get_const_rhs(inner_knots_num, obs_data, domain_min, domain_max)
#     
#     
#     # the location of reflection points will only influence the const_dir
#     const.dir1      = get_valid_const_dir(inner_knots_num)
#     const.dir2      = get_criteria_const_dir(inner_knots_num)
#     
#     obj_min         = NA
#     obj_max         = NA
#     mat             = NULL  # matrix recording the (inflection1, inflection2, lp_min$val, lp_max$val)
#     
#     bad_status_rec  = NULL
#     
#     
#     for(i in 1:(inner_knots_num + 2 - 5)){
#         for(j in (i+2):(inner_knots_num + 2 - 3)){
#             loc_vec         = c(i, j)
#             inflec_loc1     = loc_vec[1]
#             inflec_loc2     = loc_vec[2]
#             
#             mode_loc        = i + 1
#             
#             while(mode_loc  < j){
#                 
#                 mode_val        = x_grid[mode_loc]
#                 
#                 new_loc_vec     = c(loc_vec[1], mode_loc, loc_vec[2])
#                 par_vec         = loc2par(new_loc_vec,                            # grid indices, needs to be a vector of 2 integers, ele1 < ele2
#                                           x_grid          = x_grid,
#                                           inner_knots_num ,
#                                           domain_min      = domain_min,
#                                           domain_max      = domain_max)
#                                           
#                 
#                 
#                 
#                 const.dir3_temp = get_unimodality_const_dir( par_vec               = par_vec,       # vector of 2 elements, the reparameterized inflection parameters
#                                                              domain_min            = domain_min,
#                                                              domain_max            = domain_max,
#                                                              x_grid                = x_grid,
#                                                              inner_knots_num)
#                 
#                 const.dir4_temp = get_modal_const_dir(par_vec    = par_vec,
#                                                       domain_min = domain_min,
#                                                       domain_max = domain_max,
#                                                       x_grid     = x_grid,
#                                                       inner_knots_num)
#                 
#                 
#                 const.dir_temp  = c(const.dir1, const.dir2, const.dir3_temp, const.dir4_temp)
#                 
#                 lp_min  = lp("min", obj.coeff, const.mat, const.dir_temp, const.rhs)
#                 lp_max  = lp("max", obj.coeff, const.mat, const.dir_temp, const.rhs)
#                 
#                 
#                 
#                 if(lp_min$status != 0){
#                   #  cat("lp_min not converge!", "\n")
#                   #  cat("lp_min$status = ", lp_min$status, "\n")
#                   #  cat(inflec_loc1, mode_loc, inflec_loc2,  "\n")
#                     rec_temp      = c("min", lp_min$status, inflec_loc1, mode_loc, inflec_loc2, lp_min$objval)
#                     bad_status_rec = rbind(bad_status_rec, rec_temp)
#                 }
#                 if(lp_max$status != 0){
#                   #  cat("lp_max not converge!", "\n")
#                   #  cat("lp_max$status = ", lp_max$status, "\n")
#                     cat(inflec_loc1, mode_loc, inflec_loc2,  "\n")
#                     rec_temp      = c("max", lp_max$status, inflec_loc1, mode_loc, inflec_loc2, lp_max$objval)
#                     bad_status_rec = rbind(bad_status_rec, rec_temp)
#                     
#                 }
#                 
#                 vec     = c(loc_vec[1], mode_loc, loc_vec[2], lp_min$objval, lp_max$objval)
#                 mat     = rbind(mat, vec)
#                 
#                 #print(vec)
#                 
#                 
#                 if((is.na(obj_min)) || (lp_min$objval < obj_min)){
#                     obj_min = lp_min$objval
#                 } 
#                 
#                 
#                 if((is.na(obj_max)) || (lp_max$objval > obj_max)){
#                     obj_max = lp_max$objval
#                 }
#                 
#                 mode_loc    = mode_loc + 1
#             }
#             
#             
#             
#             
#         }
#         
#         # if(i %% 5 == 0){
#         #     write.csv(mat, "mat_greedy.csv")
#         # }
#         
#     }
#     
#     #print(c(obj_min, obj_max)) 
#     max_loc1      = mat[which.max(mat[, 5]), 1]
#     max_loc2      = mat[which.max(mat[, 5]), 3]
#     max_mode_loc  = mat[which.max(mat[, 5]), 2]
#     
#     new_max_loc   = c(max_loc1, max_mode_loc, max_loc2)
#     
#     max_pars      = loc2par(new_max_loc, x_grid = x_grid, inner_knots_num, domain_min, domain_max)
#     max_vals      = par2val(max_pars, domain_min, domain_max)
#     
#     
#     min_loc1      = mat[which.min(mat[, 4]), 1]
#     min_loc2      = mat[which.min(mat[, 4]), 3]
#     min_mode_loc  = mat[which.min(mat[, 4]), 2]
#     
#     new_min_loc   = c(min_loc1, min_mode_loc, min_loc2)
#     
#     min_pars      = loc2par(new_min_loc, x_grid = x_grid, inner_knots_num, domain_min, domain_max)
#     min_vals      = par2val(min_pars, domain_min, domain_max)
#     
#     myresult = list(greedy_process = mat, 
#                     optimal_result = c(obj_min, obj_max),
#                     index_min      = new_min_loc,
#                     index_max      = new_max_loc,
#                     inflec_val_min = min_vals,
#                     inflec_val_max = max_vals)
#     
#     View(bad_status_rec)
#     
#     colnames(bad_status_rec) = c("lp_dir", "lp_status", "a", "M", "b", "lp_obj")
#     
#     write.csv(bad_status_rec, 
#               file = paste("N30_in", interest, "bad_status_record.csv"))
#     
#     return(myresult)
#     
# }

get_eF_fix           = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4,
    show_plot        = FALSE
){
    grid_obj         = create_grid_obj(new_pars, 
                                       new_domain_1,
                                       new_domain_2, 
                                       new_n,
                                       new_n_neighbors,
                                       new_t,
                                       new_obs_data,
                                       epsilon)
    obj_coeff   = get_density_obj_coeff(grid_obj)
    const_mat   = get_const_mat(grid_obj)
    const_dir   = get_const_dir(grid_obj)
    const_rhs   = get_const_rhs(grid_obj)
    
    lp_min          = lp("min", obj_coeff, const_mat, const_dir, const_rhs)
    lp_max          = lp("max", obj_coeff, const_mat, const_dir, const_rhs)
    if(lp_min$status != 0){
        cat("Sth. wrong with LP MIN", "\n")
        cat("lp_min$status = ", lp_min$status, "\n")
        if(lp_min$status == 2){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN does not converge!", "\n", 
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
        } else if(lp_min$status == 5){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN: Numerical failure encountered!", "\n",
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
            print_grid_obj(grid_obj)
            #save(grid_obj, file = "status5.RData")
        }
    }
    if(lp_max$status != 0){
        cat("Sth. wrong with LP MAX", "\n")
        cat("lp_max$status = ", lp_max$status, "\n")
        if(lp_max$status == 2){
            small_num    = -1e6
            lp_max$objval   = small_num
            cat("LP MAX does not converge!", "\n", 
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            
        } else if(lp_max$status == 5){
            small_num       = -1e6
            lp_max$objval   = small_num
            cat("LP MAX: Numerical failure encountered!", "\n",
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            print_grid_obj(grid_obj)
            #save(grid_obj, file = "status5.RData")
            
        }
    }
    
    f_hat_min       = lp_min$solution
    f_hat_max       = lp_max$solution
    
    F_hat_min       = get_cdf(f_hat    = f_hat_min,
                              grid_obj = grid_obj)
    F_hat_max       = get_cdf(f_hat    = f_hat_max,
                              grid_obj = grid_obj)
    
    
    if(show_plot){
        
        pdf_dat = data.frame(
            grid = c(grid_obj$x_grid, grid_obj$x_grid), 
            pdfs = c(f_hat_min, f_hat_max),
            LP   = c(rep("Min", length(f_hat_min)), rep("Max", length(f_hat_max)))
        )
        cdf_dat  = data.frame(
            grid = c(grid_obj$x_grid, grid_obj$x_grid),
            cdfs = c(F_hat_min, F_hat_max),
            LP   = c(rep("Min", length(f_hat_min)), rep("Max", length(f_hat_max)))
        )
        
        CI_dat   = data.frame(
            x    = grid_obj$x_grid,
            lower= grid_obj$KS_CI_adapt$lower,
            upper= grid_obj$KS_CI_adapt$upper
        )
        
        break_f  = round(c(grid_obj$domain_min,
                           grid_obj$domain_max,
                           #min(grid_obj$obs_data),
                           #max(grid_obj$obs_data),
                           #grid_obj$obs_data,
                           grid_obj$t,
                           grid_obj$vals), 2)
        label_f  = as.character(break_f)
        
        p_F      = 
            ggplot(cdf_dat,
                   aes(x     = grid,
                       y     = cdfs,
                       shape = LP,
                       col   = LP))     +
            geom_point(size  = 2)       + 
            geom_line()                 +
            geom_vline(xintercept       = grid_obj$vals,
                       col              = cols[8], 
                       linetype         = "dotdash",
                       size             = 1) +
            geom_vline(xintercept       = c(grid_obj$t), size = 1, col = cols[6]) +
            scale_x_continuous(limits   = c(grid_obj$domain_min, 
                                            grid_obj$domain_max),
                               breaks   = break_f,
                               labels   = label_f,
                               name     = "Domain") +
            scale_shape_discrete(solid  = T,
                                 name   = "Estimated CDF",
                                 breaks = c("Min", "Max"),
                                 labels = c(paste("Minimizing", "F(",grid_obj$t,")"), 
                                            paste("Maximizing", "F(",grid_obj$t,")"))) +
            scale_color_manual(name     = "Estimated CDF",
                               breaks   = c("Min", "Max"),
                               labels   = c(paste("Minimizing", "F(",grid_obj$t,")"),
                                            paste("Maximizing", "F(",grid_obj$t,")")),
                               values   = c(cols[6], cols[7]) ) +
            xlab("Domain")              +
            ylab(expression(hat(F)))    +
            ggtitle(paste("Estimated CDFs in Minimizing and Maximizing Density at", grid_obj$t),
                    subtitle            = 
                        paste("(a, M, b) = (", 
                              round(grid_obj$vals[1],0), ",",
                              round(grid_obj$vals[2],0), ",",
                              round(grid_obj$vals[3],0), ")")) +
            theme(legend.position       = c(0.15, .9),
                  plot.title            = element_text(size = 15, face = "bold", hjust = 0.5),
                  axis.text.x           = element_text(angle = 45),
                  plot.subtitle         = element_text(hjust = 0.5,
                                                       face = "bold.italic",
                                                       size = 13)) +
            geom_ribbon(CI_dat,
                        mapping         = aes(
                            x           = x,
                            ymin        = lower,
                            ymax        = upper),
                        inherit.aes = FALSE,
                        stat        ="stepribbon", 
                        alpha       = 0.1, fill = cols[2], col = cols[2]
            )
        
        # X11()
        # print(p_F)
        
        p_f = 
            ggplot(pdf_dat, 
                   aes(x     = grid, 
                       y     = pdfs, 
                       shape = LP, 
                       col   = LP))     +
            geom_point(size  = 2)       + 
            geom_line()                 +
            geom_vline(xintercept       = grid_obj$vals,
                       col              = cols[8], 
                       linetype         = "dotdash",
                       size             = 1) +
            geom_vline(xintercept       = c(grid_obj$t), size = 1, col = cols[6]) +
            scale_x_continuous(limits   = c(grid_obj$domain_min, 
                                            grid_obj$domain_max),
                               breaks   = break_f,
                               labels   = label_f,
                               name     = "Domain") +
            scale_shape_discrete(solid  = T,
                                 name   = "Estimated PDF",
                                 breaks = c("Min", "Max"),
                                 labels = c(paste("Minimizing", "f(",grid_obj$t,")"), 
                                            paste("Maximizing", "f(",grid_obj$t,")"))) +
            scale_color_manual(name     = "Estimated PDF",
                               breaks   = c("Min", "Max"),
                               labels   = c(paste("Minimizing", "f(",grid_obj$t,")"),
                                            paste("Maximizing", "f(",grid_obj$t,")")),
                               values   = c(cols[6], cols[7]) ) +
            xlab("Domain")              +
            ylab(expression(hat(f)))    +
            ggtitle(paste("Estimated PDFs in Minimizing and Maximizing Density at", grid_obj$t),
                    subtitle            = 
                        paste("(a, M, b) = (", 
                              round(grid_obj$vals[1],0), ",",
                              round(grid_obj$vals[2],0), ",",
                              round(grid_obj$vals[3],0), ")")) +
            theme(legend.position       = c(0.15, .9),
                  plot.title            = element_text(size = 15, face = "bold", hjust = 0.5),
                  axis.text.x           = element_text(angle = 45),
                  plot.subtitle         = element_text(hjust = 0.5,
                                                       face = "bold.italic",
                                                       size = 13)) 
        
        
        X11()
        grid.arrange(p_F, p_f, nrow = 1)
        
        return(data.frame(CDF2Min = F_hat_min,
                          CDF2Max = F_hat_max,
                          PDF2Min = f_hat_min,
                          PDF2Max = f_hat_max))
        
    } else{
        
        return(data.frame(CDF2Min = F_hat_min,
                          CDF2Max = F_hat_max,
                          PDF2Min = f_hat_min,
                          PDF2Max = f_hat_max))
    }
    
    
    
}



get_eF_max           = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4,
    show_plot        = FALSE
){
    grid_obj         = create_grid_obj(new_pars, 
                                       new_domain_1,
                                       new_domain_2, 
                                       new_n,
                                       new_n_neighbors,
                                       new_t,
                                       new_obs_data,
                                       epsilon)
    
    obj_coeff   = get_density_obj_coeff(grid_obj)
    const_mat   = get_const_mat(grid_obj)
    const_dir   = get_const_dir(grid_obj)
    const_rhs   = get_const_rhs(grid_obj)
    
    lp_max          = lp("max", obj_coeff, const_mat, const_dir, const_rhs)
    
    if(lp_max$status != 0){
        cat("Sth. wrong with LP MAX", "\n")
        cat("lp_max$status = ", lp_max$status, "\n")
        if(lp_max$status == 2){
            small_num    = -1e6
            lp_max$objval   = small_num
            cat("LP MAX does not converge!", "\n", 
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            
        } else if(lp_max$status == 5){
            small_num       = -1e6
            lp_max$objval   = small_num
            cat("LP MAX: Numerical failure encountered!", "\n",
                "I am using a SUPER SMALL(negative) number as objval = ", small_num, "\n")
            print_grid_obj(grid_obj)
            #save(grid_obj, file = "status5.RData")
            
        }
    }

    f_hat_max       = lp_max$solution
    F_hat_max       = get_cdf(f_hat    = f_hat_max,
                              grid_obj = grid_obj)
    
    # if we want the plot:

    if (show_plot){
        
        pdf_dat = data.frame(
            grid = grid_obj$x_grid, 
            pdfs = f_hat_max
        )
        cdf_dat = data.frame(
            grid = grid_obj$x_grid, 
            cdfs = F_hat_max
        )
        
        CI_dat   = data.frame(
            x    = grid_obj$x_grid,
            lower= grid_obj$KS_CI_adapt$lower,
            upper= grid_obj$KS_CI_adapt$upper
        )
        
        break_f  = round(c(grid_obj$domain_min,
                           grid_obj$domain_max,
                           #min(grid_obj$obs_data),
                           #max(grid_obj$obs_data),
                           #grid_obj$obs_data,
                           grid_obj$t,
                           grid_obj$vals), 2)
        label_f  = as.character(break_f)
        
        
        p_F      = 
            ggplot(cdf_dat,
                   aes(x     = grid,
                       y     = cdfs))   +
            geom_point(size  = 2)       + 
            geom_line()                 +
            geom_vline(xintercept       = grid_obj$vals,
                       col              = cols[8], 
                       linetype         = "dotdash",
                       size             = 1) +
            geom_vline(xintercept       = c(grid_obj$t), size = 1, col = cols[6]) +
            scale_x_continuous(limits   = c(grid_obj$domain_min, 
                                            grid_obj$domain_max),
                               breaks   = break_f,
                               labels   = label_f,
                               name     = "Domain") +
            scale_shape_discrete(solid  = T,
                                 name   = "Estimated CDF",
                                 breaks = c("Min", "Max"),
                                 labels = c(paste("Minimizing", "F(",grid_obj$t,")"), 
                                            paste("Maximizing", "F(",grid_obj$t,")"))) +
            scale_color_manual(name     = "Estimated CDF",
                               breaks   = c("Min", "Max"),
                               labels   = c(paste("Minimizing", "F(",grid_obj$t,")"),
                                            paste("Maximizing", "F(",grid_obj$t,")")),
                               values   = c(cols[6], cols[7]) ) +
            xlab("Domain")              +
            ylab(expression(hat(F)))    +
            ggtitle(paste("Estimated CDFs in Maximizing Density at", grid_obj$t),
                    subtitle            = 
                        paste("(a, M, b) = (", 
                              round(grid_obj$vals[1],0), ",",
                              round(grid_obj$vals[2],0), ",",
                              round(grid_obj$vals[3],0), ")")) +
            theme(legend.position       = c(0.15, .9),
                  plot.title            = element_text(size = 15, face = "bold", hjust = 0.5),
                  axis.text.x           = element_text(angle = 45),
                  plot.subtitle         = element_text(hjust = 0.5,
                                                       face = "bold.italic",
                                                       size = 13)) +
            geom_ribbon(CI_dat,
                        mapping         = aes(
                            x           = x,
                            ymin        = lower,
                            ymax        = upper),
                        inherit.aes = FALSE,
                        stat        ="stepribbon", 
                        alpha       = 0.1, fill = cols[2], col = cols[2]
            )
        
        # X11()
        # print(p_F)
        p_f      = 
            ggplot(pdf_dat,
                   aes(x     = grid,
                       y     = pdfs))   +
            geom_point(size  = 2)       + 
            geom_line()                 +
            geom_vline(xintercept       = grid_obj$vals,
                       col              = cols[8], 
                       linetype         = "dotdash",
                       size             = 1) +
            geom_vline(xintercept       = c(grid_obj$t), size = 1, col = cols[6]) +
            scale_x_continuous(limits   = c(grid_obj$domain_min, 
                                            grid_obj$domain_max),
                               breaks   = break_f,
                               labels   = label_f,
                               name     = "Domain") +
            # scale_shape_discrete(solid  = T,
            #                      name   = "Estimated CDF",
            #                      breaks = c("Min", "Max"),
            #                      labels = c(paste("Minimizing", "F(",grid_obj$t,")"), 
            #                                 paste("Maximizing", "F(",grid_obj$t,")"))) +
            # scale_color_manual(name     = "Estimated CDF",
            #                    breaks   = c("Min", "Max"),
            #                    labels   = c(paste("Minimizing", "F(",grid_obj$t,")"),
            #                                 paste("Maximizing", "F(",grid_obj$t,")")),
            #                    values   = c(cols[6], cols[7]) ) +
            xlab("Domain")              +
            ylab(expression(hat(F)))    +
            ggtitle(paste("Estimated CDFs in Maximizing Density at", grid_obj$t),
                    subtitle            = 
                        paste("(a, M, b) = (", 
                              round(grid_obj$vals[1],0), ",",
                              round(grid_obj$vals[2],0), ",",
                              round(grid_obj$vals[3],0), ")")) +
            theme(legend.position       = c(0.15, .9),
                  plot.title            = element_text(size = 15, face = "bold", hjust = 0.5),
                  axis.text.x           = element_text(angle = 45),
                  plot.subtitle         = element_text(hjust = 0.5,
                                                       face = "bold.italic",
                                                       size = 13)) 
        X11()
        grid.arrange(p_F, p_f, nrow = 1)
        
        
    }

    return(data.frame(CDF2Max = F_hat_max,
                      PDF2Max = f_hat_max))

}


get_eF_min           = function(
    new_pars         = NULL,
    new_domain_1     = NULL,
    new_domain_2     = NULL,
    new_n            = NULL,
    new_n_neighbors  = NULL,
    new_t            = NULL,
    new_obs_data     = NULL,
    epsilon          = 1e-4,
    show_plot        = FALSE
){
    grid_obj         = create_grid_obj(new_pars, 
                                       new_domain_1,
                                       new_domain_2, 
                                       new_n,
                                       new_n_neighbors,
                                       new_t,
                                       new_obs_data,
                                       epsilon)
    
    obj_coeff   = get_density_obj_coeff(grid_obj)
    const_mat   = get_const_mat(grid_obj)
    const_dir   = get_const_dir(grid_obj)
    const_rhs   = get_const_rhs(grid_obj)
    
    lp_min          = lp("min", obj_coeff, const_mat, const_dir, const_rhs)
    
    if(lp_min$status != 0){
        cat("Sth. wrong with LP MIN", "\n")
        cat("lp_min$status = ", lp_min$status, "\n")
        if(lp_min$status == 2){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN does not converge!", "\n", 
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
        } else if(lp_min$status == 5){
            big_num      = 1e6
            lp_min$objval = big_num
            cat("LP MIN: Numerical failure encountered!", "\n",
                "I am using a SUPER BIG number as objval = ", big_num, "\n")
            print_grid_obj(grid_obj)
            #save(grid_obj, file = "status5.RData")
        }
    }
    f_hat_min       = lp_min$solution
    F_hat_min       = get_cdf(f_hat    = f_hat_min,
                              grid_obj = grid_obj)
    

    if(show_plot){

        pdf_dat = data.frame(
            grid = grid_obj$x_grid, 
            pdfs = f_hat_min
        )
        cdf_dat = data.frame(
            grid = grid_obj$x_grid, 
            cdfs = F_hat_min
        )
        
        CI_dat   = data.frame(
            x    = grid_obj$x_grid,
            lower= grid_obj$KS_CI_adapt$lower,
            upper= grid_obj$KS_CI_adapt$upper
        )
        
        break_f  = round(c(grid_obj$domain_min,
                           grid_obj$domain_max,
                           #min(grid_obj$obs_data),
                           #max(grid_obj$obs_data),
                           #grid_obj$obs_data,
                           grid_obj$t,
                           grid_obj$vals), 2)
        label_f  = as.character(break_f)
        
        
        p_F      = 
            ggplot(cdf_dat,
                   aes(x     = grid,
                       y     = cdfs))   +
            geom_point(size  = 2)       + 
            geom_line()                 +
            geom_vline(xintercept       = grid_obj$vals,
                       col              = cols[8], 
                       linetype         = "dotdash",
                       size             = 1) +
            geom_vline(xintercept       = c(grid_obj$t), size = 1, col = cols[6]) +
            scale_x_continuous(limits   = c(grid_obj$domain_min, 
                                            grid_obj$domain_max),
                               breaks   = break_f,
                               labels   = label_f,
                               name     = "Domain") +
            # scale_shape_discrete(solid  = T,
            #                      name   = "Estimated CDF",
            #                      labels = c(paste("Minimizing", "F(",grid_obj$t,")"), 
            #                                 paste("Maximizing", "F(",grid_obj$t,")"))) +
            # scale_color_manual(name     = "Estimated CDF",
            #                    breaks   = c("Min", "Max"),
            #                    labels   = c(paste("Minimizing", "F(",grid_obj$t,")"),
            #                                 paste("Maximizing", "F(",grid_obj$t,")")),
            #                    values   = c(cols[6], cols[7]) ) +
            xlab("Domain")              +
            ylab(expression(hat(F)))    +
            ggtitle(paste("Estimated CDFs in Minimizing Density at", grid_obj$t),
                    subtitle            = 
                        paste("(a, M, b) = (", 
                              round(grid_obj$vals[1],0), ",",
                              round(grid_obj$vals[2],0), ",",
                              round(grid_obj$vals[3],0), ")")) +
            theme(legend.position       = c(0.15, .9),
                  plot.title            = element_text(size = 15, face = "bold", hjust = 0.5),
                  axis.text.x           = element_text(angle = 45),
                  plot.subtitle         = element_text(hjust = 0.5,
                                                       face = "bold.italic",
                                                       size = 13)) +
            geom_ribbon(CI_dat,
                        mapping         = aes(
                            x           = x,
                            ymin        = lower,
                            ymax        = upper),
                        inherit.aes = FALSE,
                        stat        ="stepribbon", 
                        alpha       = 0.1, fill = cols[2], col = cols[2]
            )
        
        # X11()
        # print(p_F)
        p_f      = 
            ggplot(pdf_dat,
                   aes(x     = grid,
                       y     = pdfs))   +
            geom_point(size  = 2)       + 
            geom_line()                 +
            geom_vline(xintercept       = grid_obj$vals,
                       col              = cols[8], 
                       linetype         = "dotdash",
                       size             = 1) +
            geom_vline(xintercept       = c(grid_obj$t), size = 1, col = cols[6]) +
            scale_x_continuous(limits   = c(grid_obj$domain_min, 
                                            grid_obj$domain_max),
                               breaks   = break_f,
                               labels   = label_f,
                               name     = "Domain") +
            # scale_shape_discrete(solid  = T,
            #                      name   = "Estimated CDF",
            #                      breaks = c("Min", "Max"),
            #                      labels = c(paste("Minimizing", "F(",grid_obj$t,")"))) +
            # scale_color_manual(name     = "Estimated CDF",
            #                    breaks   = c("Min", "Max"),
            #                    labels   = c(paste("Minimizing", "F(",grid_obj$t,")")),
            #                    values   = c(cols[6], cols[7]) ) +
            xlab("Domain")              +
            ylab(expression(hat(F)))    +
            ggtitle(paste("Estimated CDFs in Minimizing Density at", grid_obj$t),
                    subtitle            = 
                        paste("(a, M, b) = (", 
                              round(grid_obj$vals[1],0), ",",
                              round(grid_obj$vals[2],0), ",",
                              round(grid_obj$vals[3],0), ")")) +
            theme(legend.position       = c(0.15, .9),
                  plot.title            = element_text(size = 15, face = "bold", hjust = 0.5),
                  axis.text.x           = element_text(angle = 45),
                  plot.subtitle         = element_text(hjust = 0.5,
                                                       face = "bold.italic",
                                                       size = 13)) 
        X11()
        grid.arrange(p_F, p_f, nrow = 1)
        
        
    }
    
    return(data.frame(CDF2Min = F_hat_min,
                      PDF2Min = f_hat_min))
    
    
}

