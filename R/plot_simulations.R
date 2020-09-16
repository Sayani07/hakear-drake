##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_sim
##' @return
##' @author Sayani07
##' @export
plot_simulations <- function(data_sim) {

  data_sim %>%
    ggplot(aes(x = MMPD)) + geom_histogram()

}
