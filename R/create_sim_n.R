##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param norm_data1
##' @param nsim
##' @return
##' @author Sayani07
##' @export
create_sim_n <- function(nlevel = 2, nsim = 500, nperm = 2000, sim_dist = dist_normal(5, 10) ) {
  (1:nsim) %>%
    map_df(function(i) {
      sim_data <- create_data_sim1(nlevel, sim_dist) %>% 
        create_data_sim1_nperm(nperm) %>% 
        create_norm_max1()
      bind_cols(sim_id = i, sim_data = sim_data)
})
}
