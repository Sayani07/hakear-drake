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

source("R/shuffle_data.R")
mpd <- function(sim_panel_data, distance_panel_data, nperm = 2000) {
  nfacet <- nrow(distance_panel_data)

  # nperm_data_facet <- lapply(seq_len(nfacet),
  #                           function(k) {
  #     
      panel_row <- distance_panel_data %>%
        tidyr::pivot_longer(cols = -1, names_to = "j") %>% select(id_facet, value)
      
      nperm_data_1 <- bind_cols(perm_data = panel_row) %>%
        rename("perm_data" = "value")
      
      nperm_data <- data.frame(2:nperm)
      #nperm_data_n <- (2:nperm) %>%
      nperm_data_n <- lapply(2:nperm, 
                            function(i) {
          z = NULL
          
          perm_data <- sim_panel_data %>% 
            #dplyr::group_by(id_facet) %>% 
            shuffle_x_for_each_facet() %>% 
            compute_quantiles() %>% 
            distance_panel() %>% 
            pivot_longer(-1, names_to = "dist", 
                         values_to = "perm_data") %>% select(-dist)
            #rename("perm_data" = "...2")
          
          #z = bind_cols(perm_id = i, id_facet = k, perm_data = perm_data$perm_data)
           #z = bind_cols(perm_data = perm_data$perm_data,
        })
      
      nperm_data_facet <- bind_rows(nperm_data_n, nperm_data_n, .id = "perm_id")

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

