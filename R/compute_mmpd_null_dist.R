##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_null_orig
##' @param nsim
##' @param nperm
##' @param dist_ordered
##' @param quantile_prob
##' @return
##' @author Sayani Gupta
##' @export
compute_mmpd_null_dist <- function(sim_null_orig, nsim = 500, nperm = 20, dist_ordered = TRUE, quantile_prob = seq(0.01, 0.99, 0.01)) {

  nx <- sim_null_orig %>% distinct(nx) %>% .$nx
  nfacet <- sim_null_orig %>% distinct(nfacet) %>% .$nfacet
  
  # nsample <- sim_null_orig %>% 
  #   group_by(nfacet, nx) %>% 
  #   summarise(count = n()) %>% 
  #   ungroup() %>% 
  #   .$count
  # 
  # nested_data <- sim_null_orig %>%
  #   group_by(nfacet, nx) %>% 
  #   nest() %>% 
  #   ungroup() %>% 
  #   mutate(n = nsample)
  # 
  lapply(seq_len(nsim), function(i) {
    
  shuffled_data <- sim_null_orig %>%
    shuffle_x_for_each_facet()
    # mutate(samp = map2(data, n, sample_n)) %>% 
    # select(-data) %>% 
    # unnest(samp)
  
  compute_mmpd_panel_grid(shuffled_data,
                          quantile_prob,
                          dist_ordered,
                          nperm)
  }) %>% bind_rows()

}
