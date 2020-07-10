source('packages_in_use.R')


# This file include the initial values decided by the user:
#       * 1) obs_data: the survival time y;
#       * 2) domain_1: c_0, by default, 0;
#       * 3) domain_2: C_0, by defailt, 200;
#       * 4) added_knots_num: the approximate knots number, w/o counting the obs_data, a, b, y, t
#       * 5) interest_val: t, the point of interest, equals to 100 by default
#       * 6) cols: color set for later visualization


obs_data = c(121.4, 119.3, 127.4, 130.9,  99.9, 123.8, 115.7, 125.5,
             98.9, 120.4, 126.7, 129.9, 129.4,
             157.6, 115.4, 132.0, 130.6, 160.7, 146.4, 139.2)


domain_1        = 0
domain_2        = 200

added_knots_num = 10 # n

# Need to input/specify the value for interest = t

interest_val    = 100



#cols            = viridis(n = 15) 
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cols = magma(10)
