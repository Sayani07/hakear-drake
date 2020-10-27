##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param range_nx
##' @param range_nfacet
##' @param ntimes
##' @param sim_dist
##' @return
##' @author Sayani Gupta
##' @export
sim_panel_grid <- function(range_nx = 2:7, range_nfacet = 2:7, ntimes = 5000, sim_dist = sim_null_dist1) {

  
(range_nx) %>%
    map_df(function(i){
(range_nfacet) %>% 
        map_df(function(j){
sim_panel(nx = i, nfacet = j, ntimes = ntimes, sim_dist =  sim_dist)
        })
    })
}
