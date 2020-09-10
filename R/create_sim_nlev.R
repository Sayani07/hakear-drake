##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nlevel
##' @param nsim
##' @param nperm
##' @return
##' @author Sayani07
##' @export
create_sim_nlev <- function(nlevel = 2:20, nsim = 50, nperm = 200) {

  (nlevel) %>% purrr::map_df(function(i){
    sim_data <- create_sim_n(nlevel = i, nsim, nperm)
    bind_cols(nlevel = i, norm_max = sim_data)
  })
  
}
