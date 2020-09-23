##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nx
##' @param nfacet
##' @param ntimes
##' @param sim_dist
##' @return
##' @author Sayani07
##' @export
sim_panel <- function(nx = 2, nfacet = 3, ntimes = 5, sim_dist =
                      distributional::dist_normal(5, 10)) {
(seq_len(nfacet)) %>% 
    map_df(function(i){
  sim_data2 <-  (seq_len(nx)) %>% map_df(function(j){
    sim_data <- generate(sim_dist, times = ntimes)
    bind_cols(id_x = j, sim_data = unlist(sim_data))
  })
  bind_cols(nfacet = nfacet, nx = nx, id_facet = i, sim_data = sim_data2)
})
}
