##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mmpd_dist_null_data
##' @param mmpd_null_orig_data
##' @return
##' @author Sayani Gupta
##' @export
plot_mmpd_null_grid <- 
  function(mmpd_dist_null_data, mmpd_null_orig_data) {
  
  joined_data <- left_join(mmpd_dist_null_data, mmpd_null_orig_data,
                               by = c("nx", "nfacet"))
  
  # number of times shuffled difference exceed true difference
  
  compute_p_value <- joined_data %>% 
    group_by(nx, nfacet) %>% 
    summarize(p_value = mean(abs(mmpd.x)> abs(mmpd.y)), .groups = "drop")
  
  
  ggplot() + 
    geom_histogram(data = mmpd_dist_null_data, 
                   aes(x = mmpd))  + 
    geom_vline(data = joined_data, 
               aes(xintercept = mmpd.y), colour = "red") +
    geom_text(data = compute_p_value, size = 3,  
              aes(x = -Inf,
                  y =  Inf,
                  label = paste("p-value:",p_value),
                                hjust   = 0,
                                vjust   = 1)) + 
    facet_grid(nx ~ nfacet)
}
