library(tictoc)
tic()
l = length(sim_null_split)



tic()
mclapply(sim_null_split,  function(i){
  replicate(nsim,
            {compute_mmpd_panel(shuffle_x_for_each_facet(i),
                                nperm = 2, 
                                dist_ordered = TRUE)})
})
toc()


tic()
mclapply(sim_null_split, function(x) compute_mmpd_panel(x, 
                                                       nperm = 20, dist_ordered = TRUE))
toc()


h  = sim_null_neat %>% select(1:2) %>% 
  ungroup() %>% group_split(nx,nfacet) 

mapply(bind_cols, h, mmpd = mmpd_dist_null_grid) %>% t() %>% as_tibble() %>% unnest(c(nfacet, nx, mmpd))

