##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_null_orig
##' @param quantile_prob
##' @param dist_ordered
##' @param nperm
##' @return
##' @author Sayani Gupta
##' @export
compute_mmpd_panel_grid <- function(sim_null_orig, quantile_prob = seq(0.01,
                                    0.99, 0.01), dist_ordered = TRUE, nperm =
                                    200) {

  nx <- sim_null_orig %>% distinct(nx) %>% .$nx
  nfacet <- sim_null_orig %>% distinct(nfacet) %>% .$nfacet
  
  (nx) %>% 
    map_df(function(i){
      (nfacet) %>% 
        map_df(function(j){
          
  mmpd <- sim_null_orig %>% 
  dplyr::filter(nx == i & nfacet == j) %>%  
  compute_mmpd_panel(quantile_prob, dist_ordered, nperm=200)
  
  bind_cols(nx = i, nfacet = j, mmpd = mmpd)
        
  })
    })

}
