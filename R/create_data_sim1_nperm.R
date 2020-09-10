##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title create simulated data with n levels from distribution sim_dist
##' @param sim_data
##' @param nperm
##' @return
##' @author Sayani07
##' @export
create_data_sim1_nperm <- function(sim_data, nperm = 2000) {

nperm_data_1 <- bind_cols(perm_id = 1, perm_data = sim_data) 
nperm_data_n <- (2:nperm) %>%
    map_df(function(i) {
    perm_data <- sample(sim_data, size = length(sim_data), replace = TRUE)
    bind_cols(perm_id = i, perm_data = perm_data)
})

bind_rows(nperm_data_1, nperm_data_n)

}
