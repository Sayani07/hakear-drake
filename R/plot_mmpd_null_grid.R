##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mmpd_dist_null_grid
##' @param mmpd_null_orig
##' @return
##' @author Sayani Gupta
##' @export
plot_mmpd_null_grid <- function(mmpd_dist_null_grid, mmpd_null_orig) {
  
  compute_p_value <- left_join(mmpd_dist_null_grid, mmpd_null_orig,
                               by = c("nx", "nfacet")) %>% 
    group_by(nx, nfacet) %>% 
    summarise(p_value = sum(if_else(abs(mmpd.x)>abs(mmpd.y),1,0))/n(), .groups = 'drop')

  ggplot() + 
    geom_histogram(data = mmpd_dist_null_grid, aes(x = mmpd))  + 
    geom_vline(data = mmpd_null_orig, aes(xintercept = mmpd),colour = "red") +
    geom_text(data = compute_p_value, size = 3,  
              aes(x = -Inf,
                  y =  Inf,
                  label = paste("p-value:",p_value),
                  hjust   = 0,
                  vjust   = 1)) + 
    facet_grid(nx ~ nfacet)
}
