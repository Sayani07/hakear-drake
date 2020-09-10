##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_data_n
##' @return
##' @author Sayani07
##' @export
create_norm_max1 <- function(sim_data_n) {
  
  nrow_data <- nrow(sim_data_n)
  nlevel <- sim_data_n %>% group_by(perm_id) %>% tally() %>% slice(1) %>% .$n
  data_orig <- sim_data_n %>% slice(seq_len(nlevel))
  
  stat_data <- sim_data_n %>% 
    summarise(mean = mean(perm_data), 
              sd = sd(perm_data))

data_orig %>% summarise(norm_max = (max(perm_data) - stat_data$mean)/stat_data$sd)

}
