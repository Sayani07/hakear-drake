##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_panel_data
##' @return
##' @author Sayani Gupta
##' @export
##' @example 
#'sim_panel_ex  = sim_panel(nx = 3,
#'                        nfacet = 2,
#'                        ntimes = 2,
#'                        sim_dist = #'sim_varf_dist1)
#'shuffle_x_for_each_facet(sim_panel_ex)


shuffle_x_for_each_facet <- function(sim_panel_data) {
  
  sim_panel_data <- sim_panel_data %>% 
    unnest(cols = c(data)) %>% 
    ungroup()
  
  nx <- sim_panel_data %>% distinct(nx) %>% .$nx
  nfacet <- sim_panel_data %>% distinct(nfacet) %>% .$nfacet
  
  
  shuffled_data <- lapply(seq_len(nfacet), function(i){
    #shuffled_data <- (seq_len(nfacet)) %>% 
    #map_df(function(i){
    #(nx)%>%
    #map_df(function(j){
    filter_data = sim_panel_data %>% dplyr::filter(id_facet==i) 
    
    new_sim_data = sample(filter_data$sim_data, nrow(filter_data))
    
    bind_cols(filter_data, new_sim_data = new_sim_data)
  }) %>% 
    bind_rows() %>% 
    select(-sim_data) %>% 
        rename("sim_data" = "new_sim_data")
  
  
  # nsample <- sim_panel_data %>% 
  #   group_by(nfacet, nx) %>% 
  #   summarise(count = n()) %>% 
  #   ungroup() %>% 
  #   .$count
  # 
  # nested_data <- sim_panel_data %>%
  #   group_by(nfacet, nx)  %>% 
  #   nest(sim_data) %>% 
  #   ungroup() %>% 
  #   mutate(n = nsample)
  # 
  #     shuffled_data <- nested_data %>%
  #       mutate(samp = map2(data, n, sample_n, replace = TRUE)) %>%
  #       select(-data, -n) %>% 
  #       unnest(samp)
  #     
      shuffled_data %>%
        group_by(nfacet, nx,id_facet, id_x)  %>% 
        nest()
}
