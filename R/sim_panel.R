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
  if(length(sim_dist)==1)
  {
    sim_dist = rep(sim_dist, nx*nfacet)
  }
  id_x <- rep(seq_len(nx), nfacet)
  id_facet <- rep(seq_len(nfacet), each = nx)
  
  sim_data2  <- tibble(id_x, 
                       id_facet,
                       sim_dist) %>% 
    group_by(id_facet, 
             id_x) %>%
    summarise(sim_data = generate(sim_dist, times = ntimes), .groups = 'drop') %>% unnest(sim_data)
  
  bind_cols(nfacet = nfacet, nx = nx, sim_data = sim_data2)
}
