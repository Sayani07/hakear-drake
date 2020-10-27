library(drake)
library(dplyr)
library(tidyr)

# Simulation study
# fixed global parameters
# set simulation parameters
seed1 = set.seed(9999) # seed while generating random observations
# distance between which quantiles are computed,
   quantile_prob_vec = seq(0.01, 0.99, 0.01) 
# ordered or un-ordered categories of x levels while computing distances
   dist_ordered_val = TRUE 
   
#' variable (maybe) global parameters
# grid with how many facet levels
 range_nfacet_vec = c(2, 3, 4, 5, 7, 9, 14, 21, 24, 31)
# grid with how many x levels
 range_nx_vec = c(2, 3, 4, 5, 7, 9, 14, 21, 24, 31)
# number of observations to be generated
  ntimes_val = 500
# number of permutation of data done to compute estimates
  nperm_val = 10
# number of permutations of data done to display histogram for each panel
   nsim_val = 100
 
    
    
