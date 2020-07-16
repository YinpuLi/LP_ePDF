source('LP_fix_inflec.R')
source('LP_temp.R')
###################################################
##########  Nelder Mead Optimization ##############
###################################################

# only for maximizing problem


# now we have the best pars = c(93.9727493 , 0.9226362)

my_interest = c(100, 120, 150)
i = 3
new_interest     = my_interest[i]


#init_val_vec     = c(94, 96, 99)
#init_par_vec     = val2par(init_val_vec, domain1, domain2)
init_val_vec
init_par_vec


(fit2 = optim(par = init_par_vec,
              fn  = solveLP_max_min,
              seg_set         = my_deltas,
              added_knots_num = n,
              inner_knots_num = N,
              domain_min      = domain1,
              domain_max      = domain2,
              x_grid          = my_grid,
              interest        = my_interest,
              interest_indx   = my_interest_index,
              obs_data        = y,
              method = "Nelder-Mead",
              control = list(reltol = 1e-10)))


cur_par = fit2$par



for(i in 1:5){
   
   # TODO: I have not added any updated grid stuff!
   fit = optim(par = cur_par,
               fn  = solveLP_max_min_adapt,
               method = "Nelder-Mead",
               control = list(reltol = 1e-10))
   
   cat("i = ", i, "\n",
       "new par = ", fit$par, "\n",
       "new val = ", fit$val, "\n"
       )
   
   
   cur_par = fit$par
   
}























fit2$par
- fit2$value
solveLP_max(pars = fit2$par, interest = new_interest)
(my_val = par2val(fit2$par, domain1, domain2))

check_elems(
   my_val, init_val_vec
)


# Current val vec = 94.17742 32562.88 32565.52 is not valid,  
# Using valid val vec =  94.17742 170.69 170.695 instead! 
#    Current val vec = 94.17742 32562.88 32565.52 is not valid,  
# Using valid val vec =  94.17742 170.69 170.695 instead! 
#    Current val vec = 94.17742 32562.88 32565.52 is not valid,  
# Using valid val vec =  94.17742 170.69 170.695 instead! 
#    Current val vec = 94.17742 32562.88 32565.52 is not valid,  
# Using valid val vec =  94.17742 170.69 170.695 instead! 
#    Sth. wrong with LP MIN 
# lp_min$status =  2 
# Current val vec = 94.17742 96.81613 32565.52 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    Current val vec = 94.17742 96.81613 32565.52 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    Current val vec = 94.17742 96.81613 32565.52 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    Current val vec = 94.17742 96.81613 32565.52 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    Current val vec = 100.4559 100.4561 1506.851 is not valid,  
# Using valid val vec =  100.4559 100.4561 170.69 instead! 
#    Current val vec = 100.4559 100.4561 1506.851 is not valid,  
# Using valid val vec =  100.4559 100.4561 170.69 instead! 
#    Current val vec = 100.4559 100.4561 1506.851 is not valid,  
# Using valid val vec =  100.4559 100.4561 170.69 instead! 
#    Current val vec = 100.4559 100.4561 1506.851 is not valid,  
# Using valid val vec =  100.4559 100.4561 170.69 instead! 
#    Current val vec = 98.88629 98.91008 391.6138 is not valid,  
# Using valid val vec =  98.88629 98.91008 170.69 instead! 
#    Current val vec = 98.88629 98.91008 391.6138 is not valid,  
# Using valid val vec =  98.88629 98.91008 170.69 instead! 
#    Current val vec = 98.88629 98.91008 391.6138 is not valid,  
# Using valid val vec =  98.88629 98.91008 170.69 instead! 
#    Current val vec = 98.88629 98.91008 391.6138 is not valid,  
# Using valid val vec =  98.88629 98.91008 170.69 instead! 
#    Current val vec = 95.74704 388.4508 401.1294 is not valid,  
# Using valid val vec =  95.74704 170.69 170.695 instead! 
#    Current val vec = 95.74704 388.4508 401.1294 is not valid,  
# Using valid val vec =  95.74704 170.69 170.695 instead! 
#    Current val vec = 95.74704 388.4508 401.1294 is not valid,  
# Using valid val vec =  95.74704 170.69 170.695 instead! 
#    Current val vec = 95.74704 388.4508 401.1294 is not valid,  
# Using valid val vec =  95.74704 170.69 170.695 instead! 
#    Sth. wrong with LP MIN 
# lp_min$status =  2 
# Current val vec = 98.10148 98.35202 231.8851 is not valid,  
# Using valid val vec =  98.10148 98.35202 170.69 instead! 
#    Current val vec = 98.10148 98.35202 231.8851 is not valid,  
# Using valid val vec =  98.10148 98.35202 170.69 instead! 
#    Current val vec = 98.10148 98.35202 231.8851 is not valid,  
# Using valid val vec =  98.10148 98.35202 170.69 instead! 
#    Current val vec = 98.10148 98.35202 231.8851 is not valid,  
# Using valid val vec =  98.10148 98.35202 170.69 instead! 
#    Current val vec = 94.17742 96.81613 389.5199 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    Current val vec = 94.17742 96.81613 389.5199 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    Current val vec = 94.17742 96.81613 389.5199 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    Current val vec = 94.17742 96.81613 389.5199 is not valid,  
# Using valid val vec =  94.17742 96.81613 170.69 instead! 
#    $par
# [1] 94.17742  0.97029  0.97029
# 
# $value
# [1] -0.141593
# 
# $counts
# function gradient 
# 234       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL



# Steve: using various numerical optimization routines available:

# based on previous experiments over N = 100,
# the max of maxLP is giving results in around (94, 100), with both inflection points falling within the range
# so I could initialize the par_initial_max within this area 
# and make N larger and larger.


ptm        = proc.time()


fit        = optim(
   par     = init_par_vec,
   fn      = solveLP_max_min,
   method  = "Nelder-Mead",
   control = list(reltol = 1e-10))

ptm_NM     = proc.time() - ptm
# user  system elapsed 
# 0.22    0.06    0.28 

fit$par
- fit$value
solveLP_max(pars = fit$par, interest = new_interest)
(my_val = par2val(fit$par, domain1, domain2))

check_elems(
   my_val, init_val_vec
)



old_best   = 1e60
new_best   = 0

it         = 0

time_rec   = NULL       # recording elapsed time 
impr_rec   = NULL
fitval_rec = NULL

methods    = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")

while(old_best > (new_best + 0.01) && it < 3){
   cat("\n")
   cat("\n")
   it      = it + 1
   print(paste0("it = ", it))
   
   for(m in 1 : length(methods)){
      
      #print(methods[m])
      
      ptm  = proc.time()
      
      pre  = - fit$value
      
      print(paste0("pre fit val = ", pre))
      
      #suppressWarnings(try(
         fit = optim(par     = fit$par,
                     fn      = solveLP_max_min,
                     method  = methods[m],
                     control = list(reltol = 1e-10))
     # ))
      
      ptm_temp               = proc.time() - ptm
      
      print(paste0(methods[m], " uses ", ptm_temp[3], " s."))
      
      time_rec               = c(time_rec, ptm_temp[3])
      
      post                   = - fit$value
      
      fitval_rec             = c(fitval_rec, post)
      
      print(paste0("post fit val = ", post))
      
      impr_temp              = post - pre 
      
      print(paste0("improved = ", impr_temp))
      
      impr_rec               = c(impr_rec, impr_temp)
      
      cat("\n")
   }
   old_best                  = - fit$value
   
}

TIME_rec           = matrix(time_rec, nrow = 10, ncol = 5, byrow = T)
colnames(TIME_rec) = methods

IMPR_rec           = matrix(impr_rec, nrow = 10, ncol = 5, byrow = T)
colnames(IMPR_rec) = methods

TIME_rec
IMPR_rec

# need to check the final fit 
# are the inflection values approaching to the lower/upper bounds of the domain?
pars2val(fit$par)   # [1] 93.97275 96.48866
# the answer is no, the best inflection vals is around (94, 96.5)



new_best # 0.1634891(N = 100, it = 10)
         # 1.481341 (N = 1000, it = 5)

fit
fit$par
fit$value

# $par
# [1] 93.9727493  0.9226362
# 
# $value
# [1] -0.1634891
# 
# $counts
# function gradient 
# 10000       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL












