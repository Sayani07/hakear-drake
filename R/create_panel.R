##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_orig
##' @param ncol
##' @return
##' @author Sayani07
##' @export
create_panel <- function(data_orig, ncol = 3, nrow = 2) {

    data_orig %>% 
    as_tibble(.name_repair = "unique") %>% 
    mutate(id = row_number()) %>% 
    mutate(panel_id = rep(1:ncol, each = nrow))

}
