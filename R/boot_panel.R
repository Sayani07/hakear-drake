##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_panel
##' @param nperm
##' @return
##' @author Sayani07
##' @export
boot_panel <- function(data_panel, nperm = 200) {

  nperm_data_1 <- bind_cols(perm_id = 1, perm_data = data_panel)
  
  nperm_data_n <- (2:nperm) %>%
    purrr::map_df(function(i){
  perm_data <- data_panel %>% 
    sample_n(size = nrow(data_panel), replace = TRUE)
  bind_cols(perm_id = i, perm_data = perm_data)
    })
  
  bind_rows(nperm_data_1, nperm_data_n)

}
