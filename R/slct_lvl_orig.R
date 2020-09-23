##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_null_orig
##' @param nx
##' @param nfacet
##' @return
##' @author Sayani07
##' @export
slct_lvl_orig <- function(sim_null_orig,
                          nx = 4, 
                          nfacet = 7) {

sim_null_orig %>% 
    dplyr::filter(id_facet %in% seq_len(nx), 
                  id_x %in% seq_len(nfacet))

}
