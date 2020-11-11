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

mpd2 <- function(sim_panel_data,
                 distance_panel_data,
                 nperm = 20) {
  sim_panel_adjust <- sim_panel_data %>%
    ungroup() %>%
    unnest(c(data))
  
  panel_row <- distance_panel_data %>%
    tidyr::pivot_longer(cols = -1, names_to = "j") %>% select(id_facet, value)
  
  sim_size <- nrow(sim_panel_adjust)
  
  # nperm_data_facet <- lapply(seq_len(nfacet),
  #                           function(k) {
  #
  # panel_row <- distance_panel_data %>%
  #   tidyr::pivot_longer(cols = -1, names_to = "j") %>% select(id_facet, value)
  
  nperm_data_1 <- bind_cols(perm_data = sim_panel_adjust) %>%
    rename("perm_data" = "value")
  
  nperm_data <- data.frame(2:nperm)
  #nperm_data_n <- (2:nperm) %>%
  nperm_data_n <- lapply(2:nperm,
                         function(i) {
                           z = NULL
                           perm_data_sam <- sample(sim_panel_adjust$sim_data)
                           perm_data = sim_panel_adjust %>% select(-sim_data) %>%
                             mutate(sim_data = perm_data_sam) %>%
                             group_by(nfacet, nx, id_facet, id_x) %>%
                             nest() %>%
                             compute_quantiles() %>%
                             distance_panel() %>%
                             pivot_longer(-1, names_to = "dist",
                                          values_to = "perm_data") %>%
                             select(-dist) %>%
                             group_by(id_facet) %>% summarise(m = max(unlist(perm_data), na.rm = TRUE)) %>% summarise(mprime = median(m, na.rm = TRUE))
                           #rename("perm_data" = "...2")
                           
                           #z = bind_cols(perm_id = i, id_facet = k, perm_data = perm_data$perm_data)
                           #z = bind_cols(perm_data = perm_data$perm_data,
                         })
  
  nperm_data_facet <- bind_rows(nperm_data_1, nperm_data_n, .id = "perm_id")
  
  
  stat <- nperm_data_facet %>%
    summarise(mean = mean(mprime, na.rm = TRUE),
              sd = sd(mprime, na.rm = TRUE))
  
  nperm_data_facet %>%
    mutate(norm_stat = scale(mprime))
  
  #
  #   group_by(id_facet) %>%
  #     summarise(
  #       max = max(perm_data, na.rm = TRUE),
  #       mean_perm = mean(perm_data, na.rm = TRUE),
  #       sd_perm = sd(perm_data, na.rm = TRUE)
  #     ) %>%
  
   
}

