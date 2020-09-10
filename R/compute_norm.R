##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_boot
##' @param method
##' @return
##' @author Sayani07
##' @export
compute_norm <- function(data_boot, method = "std") {

  nrow_data <- nrow(data_boot)
  nlevel <- data_boot %>% group_by(perm_id) %>% tally() %>% slice(1) %>% .$n
  data_orig <- data_boot %>% slice(seq_len(nlevel))
  
  stat_data <- data_boot %>% 
    summarise(mean = mean(value), 
              sd = sd(value))
  
  data_orig %>% 
    group_by(panel_id) %>% 
    summarise(norm_max = (max(value) - stat_data$mean)/stat_data$sd) %>% summarize(MMPD = median(norm_max)/n())

}
