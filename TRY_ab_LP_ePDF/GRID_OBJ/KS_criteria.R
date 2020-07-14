#################################################################
#############  KS Criteria functions and quantites ##############
#################################################################

# This file does not source anything.
# It provides functions to update the KS criteria adaptive to current grid setting:
#       * ecdf_ks_ci(obs_data) provides ECDF and KS CI for original observed data;
#       * udpate_criteria(GRID):
#           returns a data frame with 2 column entries, lower = KS_lower_adapt, upper = KS_upper_adapte.

# we may not need the updated KS bands adaptive to current grid setting
# but the adaptive bands will be used to plot

ecdf_ks_ci = function(x){
    n      = length(x)
    ec     = ecdf(sort(x))
    xx     = get("x", envir = environment(ec))
    yy     = get("y", envir = environment(ec))
    D      = sfsmisc::KSd(n)
    yyu    = pmin(yy + D, 1)
    yyl    = pmax(yy - D, 0)
    ecu    = stepfun(xx, c(yyu, 1))
    ecl    = stepfun(xx, c(yyl, yyl[n]))
    fun.ecdf = ecdf(x)
    my.ecdf  = fun.ecdf(sort(x))
    
    return(list(x=x, est = my.ecdf, upper=ecu(x),lower=ecl(x)))
}



update_criteria     = function(grid,
                               emp_data){
    
    
    criteria_lower  = ecdf_ks_ci(sort(emp_data))$lower      # the lower bound of current KS bands
    criteria_upper  = ecdf_ks_ci(sort(emp_data))$upper      # the upper bound of current KS bands
    
    new_lower = NULL; new_upper = NULL
    
    for(i in 1:length(grid)){
        cur_knot  = grid[i]
        cur_index = which(sort(obs_data) > cur_knot)[1]
        cur_lower = criteria_lower[cur_index]
        cur_upper = criteria_upper[cur_index]
        cur_lower = ifelse(is.na(cur_lower), tail(new_lower,1), cur_lower)
        cur_upper = ifelse(is.na(cur_upper), 1, cur_upper)
        new_lower = c(new_lower, cur_lower)
        new_upper = c(new_upper, cur_upper)
        
    }
    updated.ci  = data.frame(lower = new_lower, upper = new_upper)
    return(updated.ci)
    
}


