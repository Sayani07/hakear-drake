##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param distance_panel_data
##' @param nperm
##' @return
##' @author Sayani07
##' @export
mpd <- function(distance_panel_data, nperm = 2000) {



nperm_data_n <- (1:nfacet) %>%
  map_df(function(k) {
  nperm_data_1 <- bind_cols(perm_id = 1, perm_data = distance_panel_data) 
  nperm_data_n <- (2:nperm) %>%
    map_df(function(i) {
      perm_data <- sample(sim_data, size = length(sim_data), replace = TRUE)
      bind_cols(perm_id = i, perm_data = perm_data)
    })
  })
  
  bind_rows(nperm_data_1, nperm_data_n)
  

}
