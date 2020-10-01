sim_data_test = sim_panel_grid(range_nx = 2:3, 
                               range_nfacet = 2:3,
                               ntimes = 1,
                               sim_dist = distributional::dist_normal(5, 10))

nx <- sim_data_test %>% distinct(nx) %>% .$nx
nfacet <- sim_data_test %>% distinct(nfacet) %>% .$nfacet


shuffled_data <- (nfacet) %>% 
  map_df(function(i){
    (nx)%>%
      map_df(function(j){
      filter_data = nested_data %>% dplyr::filter(nfacet==i, nx==j)
      new_sim_data = sample(filter_data$sim_data, nrow(filter_data))
      bind_cols(filter_data, new_sim_data = new_sim_data)
      }) %>% select(-sim_data) %>% 
      rename("sim_data" = "new_sim_data")
  })
  

