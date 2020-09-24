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
  
  nsample <- sim_panel_data %>% 
    group_by(nfacet, nx) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    .$count
  
  nested_data <- sim_panel_data %>%
    group_by(nfacet, nx)  %>% 
    nest(sim_data) %>% 
    ungroup() %>% 
    mutate(n = nsample)
  
      shuffled_data <- nested_data %>%
        mutate(samp = map2(data, n, sample_n, replace = TRUE)) %>%
        select(-data, -n) %>% 
        unnest(samp)
      
      shuffled_data 
}
