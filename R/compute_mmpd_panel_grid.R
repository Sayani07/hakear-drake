##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_null_split
##' @param quantile_prob
##' @param dist_ordered
##' @param nperm
##' @return
##' @author Sayani Gupta
##' @export
compute_mmpd_panel_grid <- function(sim_null_split, quantile_prob = seq(0.01,
                                    0.99, 0.01), dist_ordered = TRUE, nperm = 20) {

  l = length(sim_null_split)
  
  mclapply(seq_len(l), function(i){
    compute_mmpd_panel(sim_null_split[[i]],
                       nperm = 20, 
                       dist_ordered = TRUE)
  })
}
  
  
  
  
#   nx <- sim_null_orig %>% 
#     bind_rows() %>% 
#       distinct(nx) %>% .$nx
#   
#   nfacet <- sim_null_orig %>% distinct(nfacet) %>% .$nfacet
#   
#   (nx) %>% 
#     map_df(function(i){
#       (nfacet) %>% 
#         map_df(function(j){
#           
#   mmpd <- sim_null_orig %>% 
#   dplyr::filter(nx == i, nfacet == j) %>%  
#   compute_mmpd_panel(quantile_prob, 
#                      dist_ordered, 
#                      nperm = nperm)
#   
#   bind_cols(nx = i, nfacet = j, mmpd = mmpd)
#         
#   })
#     })
# 
# }
