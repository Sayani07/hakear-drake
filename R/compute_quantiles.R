##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_panel_data
##' @param quantile_prob
##' @return
##' @author Sayani07
##' @export
compute_quantiles <- function(sim_panel_data, quantile_prob = seq(0.01, 0.99,0.01)) {

  sim_panel_data = sim_panel_data %>% unnest(data)
  facet <- unique(sim_panel_data$id_facet)
  nfacet <- length(facet)
  
  #quantile_prob <- seq(0.01, 0.05, 0.01)
  
  sim_facet_data <- sim_panel_data %>% 
    # for each group find quantiles
    group_by(id_facet, id_x) %>% 
    summarize(list_data = list(sim_data), 
              sim_data_quantile = quantile(unlist(list_data), quantile_prob), .groups = 'drop') %>% ungroup() %>% select(-list_data) %>% 
    nest(sim_data_quantile = sim_data_quantile)
    # put each x on the columns so that pairwise distance could be computed
    # pivot_wider(id_cols = c(1,2,4),
    #             names_from = id_x,
    #             values_from = sim_data_quantile,
    #             values_fn = list(sim_data_quantile = list)) %>%   

}
