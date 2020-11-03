library(lineprof)
library(drake)
source("_drake.R")

loadd()





l = lineprof(mclapply(
  sim_null_split,
  function(x){
    compute_mmpd_panel(x,
                       nperm = 2, 
                       dist_ordered = TRUE)
  }))