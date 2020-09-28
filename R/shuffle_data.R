##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_panel_data
##' @return
##' @author Sayani Gupta
##' @export
shuffle_data <- function(sim_panel_data) {

  
  nx <- sim_panel_data %>% distinct(nx) %>% .$nx
  nfacet <- sim_panel_data %>% distinct(nfacet) %>% .$nfacet
  
  
  shuffled_data <- (nfacet) %>% 
    map_df(function(i){
      (nx)%>%
        map_df(function(j){
          filter_data = sim_panel_data %>% dplyr::filter(nfacet==i, nx==j)
          new_sim_data = sample(filter_data$sim_data, nrow(filter_data))
          bind_cols(filter_data, new_sim_data = new_sim_data)
        }) %>% select(-sim_data) %>% 
        rename("sim_data" = "new_sim_data")
    })
  
  
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
      shuffled_data 
}
