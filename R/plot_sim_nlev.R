##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param norm_data_n_nlev
##' @param nlev
##' @return
##' @author Sayani07
##' @export
plot_sim_nlev <- function(norm_data_n_nlev, nlev = 5:20) {
  norm_data_n_nlev %>% 
    dplyr::filter(nlevel %in% nlev) %>% 
    ggplot(aes(x = norm_max)) + geom_histogram() +
    facet_wrap(~nlevel)

}
