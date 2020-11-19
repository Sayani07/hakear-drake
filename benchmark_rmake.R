library(lineprof)
library(drake)
source("_drake.R")

loadd()

l = proofthis(lapply(
  sim_null_split,
  function(x){
    compute_mmpd_panel(x,
                       quantile_prob = seq(0.1, 0.9, 0.1),
                       dist_ordered = TRUE,
    nperm = 2)
  }))

shine(l)
