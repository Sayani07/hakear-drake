##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nlevel
##' @param ncol
##' @param sim_dist
##' @param nsim
##' @return
##' @author Sayani07
##' @export
create_simulate_norm <- function(nlevel = 6,
                                 ncol = 3, 
                                 nrow = 2, 
                                 sim_dist = dist_normal(5, 10),
                                 nperm = 500,
                                 nsim = 200) {

  (1:nsim) %>%
    map_df(function(i) {
      sim_data <- create_data_sim1(nlevel, sim_dist) %>% 
        create_panel(ncol,nrow) %>% 
        boot_panel(nperm) %>% 
        compute_norm()
      
      bind_cols(sim_id = i, sim_data = sim_data)
    })

}
