library(drake)
library(dplyr)
library(tidyr)
# plan for visualising raw mmpd
the_plan =
  drake_plan(
    # Simulation study
    # fixed global parameters
    # set simulation parameters
    seed1 = set.seed(9999),# seed while generating random observations
    # distance between which quantiles are computed,
    quantile_prob_vec = seq(0.01, 0.99, 0.01),
    # ordered or un-ordered categories of x levels while computing distances
    dist_ordered_val = TRUE, 
    
    #' variable (maybe) global parameters
    # grid with how many facet levels
    #range_nfacet_vec = c(2, 3, 4, 5, 7, 9, 14, 21, 24, 31)
    range_nfacet_vec = c(21, 30, 45),
    # grid with how many x levels 
    #range_nx_vec = c(2, 3, 4, 5, 7, 9, 14, 21, 24, 31)
    range_nx_vec = c(2, 3, 4, 5, 7, 9, 14),
    
    # number of observations to be generated
    #ntimes_val = 500
    ntimes_val = 500,
    # number of permutation of data done to compute estimates
    #nperm_val = 10
    nperm_val = 100,
    # number of permutations of data done to display histogram for each panel
    nsim_val = 100,
    #nsim_val = 2,
    
    # Null parameters
    mean = 5,
    sd = 10,
    shape = 5,
    scale = 1,
    rate = 1/5,
    
    nx = 4,
    nfacet = 5,
    
    # Design 1: Null
    sim_null_normal = function(nx, nfacet){
      rep(distributional::dist_normal(mean, sd), nx*nfacet)
    },
    # sim_null_exp = function(nx, nfacet){
    #   distributional::dist_exponential(rate)},
    # 
    sim_null_gamma = function(nx, nfacet){
      distributional::dist_gamma(shape, rate)},
    
    # sim_null_weibull = function(nx, nfacet){
    #   distributional::dist_weibull(shape, scale)},
    
    # create grid for each of the 16 designs
    
    # Null design
    
    sim_panel_grid_data = 
      sim_panel_grid(range_nx = range_nx_vec,
                     range_nfacet = range_nfacet_vec, 
                     ntimes = 500, 
                     sim_dist = sim_null_normal) %>% 
      unnest(c(data)),
    
    sim_grid_split = sim_panel_grid_data %>%
      group_by(nx, nfacet) %>%
      group_split(),
    
    # only one grid with one ntimes
    mmpd_raw_grid = lapply(
      sim_grid_split,
      function(x) {
        tic()
        obj = compute_mmpd(
          x,
          gran_x = "id_x",
          gran_facet = "id_facet",
          response = sim_data)
        t = toc(log = TRUE, quiet = TRUE)
        return(obj)
      }
    ),
    
    # generaling for many permutations - to store the permutation data set 
    
    shuffle_grid_split = purrr::map_df(sim_grid_split,
                                       function(x){
                                         purrr::map_df(seq_len(nsim_val), #nsim,
                                                       function(i)
                                                         
                                                       {
                                                         new_sim_data = 
                                                           sample(x$sim_data, 
                                                                  size = nrow(x))
                                                         
                                                         new_data = x %>% 
                                                           select(-sim_data) %>% 
                                                           mutate(perm_id = i, sim_data = new_sim_data) %>% select(perm_id, everything())
                                                         
                                                       })
                                       }) %>% 
      group_split(perm_id, nfacet, nx),
    
    mmpd_shuffle_grid = lapply(
      shuffle_grid_split,
      function(x) {
        tic()
        obj = compute_mmpd(
          x,
          gran_x = "id_x",
          gran_facet = "id_facet",
          response = sim_data)
        t = toc(log = TRUE, quiet = TRUE)
        return(obj)
      }
    ),
    
    id = purrr::map_df(seq_len(nsim_val), 
                       function(i){
                         mutate(expand.grid(nfacet = range_nfacet_vec, nx =  range_nx_vec), perm_id = i)
                       }),
    
    data_all = mutate(id, raw_mmpd = unlist(mmpd_shuffle_grid)),
    
    hist_plot = data_all %>% 
      ggplot() +
      geom_histogram(aes(x = raw_mmpd)) +
      facet_grid(nx~nfacet)
    
    #     data_long = data_all %>% pivot_longer(c(1:2), names_to = "category", values_to = "levels")
    # data_long = data_all %>% pivot_longer(c(1:2), names_to = "category", values_to = "levels")
    
    
    # data_long %>% 
    #   ggplot() +
    #   ggridges::geom_density_ridges(aes(x = raw_mmpd, y = as.factor(levels))) + facet_wrap(~category)
    
    
  )


