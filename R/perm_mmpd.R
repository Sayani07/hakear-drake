##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_panel_data
##' @param nperm
##' @return
##' @author Sayani07
##' @export
perm_mmpd <- function(sim_panel_data, nperm = 5) {

  (seq_len(nperm)) %>% 
    map(function(i)
      {
      set.seed(i)
    perm_data = sample(sim_panel_data$sim_data, size = nrow(sim_panel_data))
    sample_data <- bind_cols(sim_panel_data, perm_data = perm_data) %>% 
      select(-sim_data) %>% 
      rename("sim_data" = "perm_data") 
    
    perm_mmpd_data <- sample_data %>% compute_mmpd_panel(nperm = 2)
    
    })
  perm_mmpd_data %>% unlist() %>% as_tibble(
  .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
}
