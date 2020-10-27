library(drake)
library(dplyr)
library(tidyr)
library(distributional)


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

# Null parameters
mean = 5
sd = 10
shape = 5
scale = 1
rate = 1/5

nx = 4
nfacet = 5

# Design 1: Null
sim_null_dist1 = distributional::dist_normal(mean, sd)

sim_null_dist2 = distributional::dist_exponential(rate)

sim_null_dist3 = distributional::dist_gamma(shape, rate)

sim_null_dist4 = distributional::dist_webull(shape, scale)


# Design 2: Alternate where only distributions across facet varies     
sim_varf_dist1 = rep(dist_normal(seq(mean,mean*nfacet, by = mean), sd), each = nx)

sim_varf_dist2 = rep(dist_exponential(seq(rate, rate*nfacet, by = rate)), each = nx)

sim_varf_dist3 = rep(dist_gamma(seq(shape, shape*nfacet, by = shape), rate), each = nx)

sim_varf_dist4 = rep(dist_weibull(seq(shape, shape*nfacet, by = shape), scale), each = nx)

# Design 3: Alternate where only distributions across x-varies

sim_varx_dist1 = rep(dist_normal(seq(mean,mean*nx, by = mean), sd), nfacet)

sim_varx_dist2 = rep(dist_exponential(seq(rate, rate*nx, by = rate)), nfacet)

sim_varx_dist3 = rep(dist_gamma(seq(shape, shape*nx, by = shape), rate),nfacet)

sim_varx_dist4 = rep(dist_weibull(seq(shape, shape*nx, by = shape), scale), nfacet)


# Design 4: Alternate where all distribution varies

sim_varall_dist1 = dist_normal(seq(mean,mean*nfacet*nx, by = mean), sd)

sim_varall_dist2 = dist_exponential(seq(rate, rate*nfacet*nx, by = rate))

sim_varall_dist3 = rep(dist_gamma(seq(shape, shape*nfacet*nx, by = shape), rate))

sim_varall_dist4 = dist_weibull(seq(shape, shape*nfacet*nx, by = shape), scale)
