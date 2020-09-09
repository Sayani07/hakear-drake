##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title create simulated data with n levels from any distribution
##' @param nlevel
##' @param sim_dist
##' @return
##' @author Sayani07
##' @export
create_data_sim1 <- function(nlevel = 2, sim_dist = dist_normal(5,10)) {

   (1:nlevel) %>%
    purrr::map(function(i) {
      x <- sim_dist %>%
        distributional::generate(nlevel)
      
     x %>%  unlist()
    
    })

}
