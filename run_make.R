library(drake)
library(tictoc)
 source("_drake.R")
loadd()
tic()
make(the_plan, parallelism = "clustermq", jobs = 4)
toc()

# 819.644 sec elapsed
# 
# 
# mmpd_null_dist = mclapply(
#   seq_len(length(sim_null_split)),
#   function(i){
#     replicate(nsim,
#               {compute_mmpd_panel(shuffle_x_for_each_facet(sim_null_split[[i]]),
#                                   nperm = 2, x
#                                   dist_ordered = TRUE)})
#   })