##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mmpd_null_dist
##' @return
##' @author Sayani Gupta
##' @export
plot_mmpd_null_grid <- function(mmpd_null_dist) {
  ggplot(mmpd_null_dist, aes(x = sim_data)) +
    geom_histogram() +
    facet_grid(nx ~ nfacet)
}
