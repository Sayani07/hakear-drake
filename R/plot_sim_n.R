##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param norm_data_n
##' @return
##' @author Sayani07
##' @export
plot_sim_n <- function(norm_data_n) {
norm_data_n %>%
    ggplot(aes(x = norm_max)) + geom_histogram()
}
