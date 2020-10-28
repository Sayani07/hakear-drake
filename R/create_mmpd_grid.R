##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param range_nfacet
##' @param range_nx
##' @param ntimes_val
##' @param nperm_val
##' @param nsim_val
##' @param quantile_prob_vec
##' @param dist_ordered_val
##' @return
##' @author Sayani Gupta
##' @export
create_mmpd_grid <- function(range_nfacet = range_nfacet_vec, 
                      range_nx = range_nx_vec, 
                      ntimes = ntimes_val, 
                      nperm = nperm_val,
                      nsim = nsim_val, 
                      quantile_prob = quantile_prob_vec, 
                      dist_ordered = dist_ordered_val,
                      sim_dist = sim_null_dist1) {

  
  sim_null_orig  = sim_panel_grid (range_nx = range_nx_vec,                             range_nfacet =
                                     range_nfacet_vec,
                                   ntimes = ntimes_val,
                                   sim_dist = sim_null_dist1)
  
  sim_null_split = sim_null_orig %>% 
    group_by(nx, nfacet) %>% 
    group_split()
  
  # observed mmpd
  mmpd_null_orig = suppressMessages(lapply(
    sim_null_split,
    function(x){
      compute_mmpd_panel(x,
                         quantile_prob = quantile_prob_vec,
                         dist_ordered = dist_ordered_val,
                         nperm = nperm_val
      )
    }))
  
  # distribution of mmpd
  mmpd_null_dist = suppressMessages(lapply(
    sim_null_split,
    function(x){
      replicate(nsim_val,
                {compute_mmpd_panel(shuffle_x_for_each_facet(x),
                                    quantile_prob = quantile_prob_vec,
                                    dist_ordered = dist_ordered_val,
                                    nperm = nperm_val
                )
                  })
    }))
  
sim_null_neat  = sim_null_orig %>% group_by(nfacet, nx) %>% nest()  
  
  null_neat  = sim_null_neat %>% select(1:2) %>% 
    ungroup() %>% group_split(nx,nfacet)
  
  mmpd_dist_null_data = t(mapply(bind_cols, null_neat, mmpd = mmpd_null_dist)) %>% as_tibble() %>% unnest(c(nfacet, nx, mmpd))
  
  mmpd_null_orig_data = t(mapply(bind_cols, null_neat, mmpd =  mmpd_null_orig)) %>% as_tibble() %>% unnest(c(nfacet, nx, mmpd))
  
  
  # visualise mmpd distribution for entire panel grid
  # plot_data  =  plot_mmpd_null_grid(mmpd_dist_null_data, mmpd_null_orig_data),  
  
  joined_data = left_join(mmpd_dist_null_data, mmpd_null_orig_data,
                          by = c("nx", "nfacet"))

  # compute_p_value = joined_data %>%
  #   group_by(nx, nfacet) %>%
  #   summarize(p_value = mean(abs(mmpd.x)> abs(mmpd.y)))
  # 
  # dist_name = !!sim_dist
  # 
  # save(joined_data, file = glue::glue("data/",substitute(dist_name),".rds"))
  joined_data
}
