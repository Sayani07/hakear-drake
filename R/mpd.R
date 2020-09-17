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
  nfacet <- nrow(distance_panel_data)

  nperm_data_facet <- (seq_len(nfacet)) %>%
    map_df(function(k) {
      
      panel_row <- distance_panel_data %>%
        magrittr::extract(k, ) %>%
        tidyr::pivot_longer(cols = -1, names_to = "j") %>% select(id_facet, value)

      nperm_data_1 <- bind_cols(perm_id = 1, perm_data = panel_row) %>%
        rename("perm_data" = "value")

      nperm_data_n <- (2:nperm) %>%
        map_df(function(i) {
          
          perm_data <- sample(panel_row$value, size = nrow(panel_row), replace = TRUE)
          
          bind_cols(perm_id = i, id_facet = k, perm_data = perm_data)
          
        })
      
      bind_rows(nperm_data_1, nperm_data_n)
    })


  nperm_data_facet %>%
    group_by(id_facet) %>%
    summarise(
      max = max(perm_data, na.rm = TRUE),
      mean_perm = mean(perm_data, na.rm = TRUE),
      sd_perm = sd(perm_data, na.rm = TRUE)
    ) %>%
    mutate(norm_x = if_else(sd_perm !=0, 
                            (max - mean_perm) / sd_perm,max))
}
