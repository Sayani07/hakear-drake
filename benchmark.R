library(lineprof)

source("R/compute_mmpd_panel.R")

sim_panel_data  = sim_panel(nx = 7,
                            nfacet = 4,
                            ntimes = 500,
                            sim_dist = distributional::dist_normal(5, 10))


l <- lineprof(compute_mmpd_panel(sim_panel_data))
shine(l)
l1 <- l

# quantile_prob = seq(0.01, 0.99,0.01)
# 
# microbenchmark::microbenchmark(
# sim_panel_data %>% 
#   # for each group find quantiles
#   group_by(id_facet, id_x) %>% 
#   summarize(list_data = list(sim_data), 
#             sim_data_quantile = quantile(unlist(list_data), quantile_prob)) %>% 
#   # put each x on the columns so that pairwise distance could be computed
#   pivot_wider(id_cols = c(1,2,4),
#               names_from = id_x,
#               values_from = sim_data_quantile,
#               values_fn = list(sim_data_quantile = list)) %>% ungroup(),
# 
# 
# 
# group_by(sim_panel_data, id_facet, id_x) %>% 
#   summarize(list_data = list(sim_data), 
#             sim_data_quantile = quantile(unlist(list_data), quantile_prob)) 
# )
