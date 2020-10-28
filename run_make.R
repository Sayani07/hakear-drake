library(drake)
drake::drake_cache("/Users/sgup0008/Documents/hakear-drake/.drake")$unlock()
library(tictoc)
 source("_drake.R")
 loadd()
tic()
r_make()
toc()

# 819.644 sec elapsed
# 
# 
# mmpd_null_dist = mclapply(
#   seq_len(length(sim_null_split)),
#   function(i){
#     replicate(nsim,
#               {compute_mmpd_panel(shuffle_x_for_each_facet(sim_null_split[[i]]),
#                                   nperm = 2, 
#                                   dist_ordered = TRUE)})
#   })