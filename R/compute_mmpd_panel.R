##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param slct_lvl_orig
##' @param quantile_prob
##' @param dist_ordered
##' @param nperm
##' @return
##' @author Sayani07
##' @export
compute_mmpd_panel <- function(.data, quantile_prob = seq(0.01,
                               0.99, 0.01), dist_ordered = TRUE, nperm = 200) {

  .data %>% 
    compute_quantiles(quantile_prob = quantile_prob) %>% 
    distance_panel(dist_ordered = dist_ordered) %>% 
    mpd(nperm) %>% 
    mmpd(nperm)
}
