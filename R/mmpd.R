##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param normx_data
##' @param nperm
##' @return
##' @author Sayani07
##' @export
mmpd <- function(normx_data, nperm = 2000) {

  panel_facet <- normx_data %>% select(id_facet, norm_x) 
  
  nperm_data_1 <- bind_cols(perm_id = 1, perm_data = panel_facet) %>% 
    rename("perm_data" ="norm_x")
  
nperm_data_n <- (2:nperm) %>%
    map_df(function(i) {
      perm_data <- sample(panel_facet$norm_x, size = nrow(panel_row), replace = TRUE)
      bind_cols(perm_id = i, id_facet = k, perm_data = perm_data)
    })

perm_facet_data <- bind_rows(nperm_data_1, nperm_data_n)

perm_facet_data %>% 
  summarise(mean_perm = mean(perm_data),
            sd_perm = sd(perm_data)) %>% 
  mutate(mmpd = (median(nperm_data_1$perm_data) - mean_perm)/sd_perm) %>% pull(mmpd) %>% round(3)

}
