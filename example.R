library(tidyverse)
set.seed(1111)
demo = rnorm(40, 5, 10) %>% 
  as_tibble() %>% 
  mutate(nfacet = rep(1:8, each = 5),
         distance = rep(1:8, 5)) %>% 
  group_by(nfacet) %>%
  summarise(max = max(value)) %>% 
  summarise(median_max = median(max))

perm_data = lapply((1:100), function(x){
  rnorm(40, 5, 10) %>% 
    as_tibble() %>% 
    mutate(nfacet = rep(1:8, each = 5),
           distance = rep(1:8, 5)) %>% 
    group_by(nfacet) %>%
    summarise(max = max(value)) %>% 
    summarise(median_max = median(max))
}) %>% bind_rows(.id = "perm_id")

mmpd_obs = (demo - mean(perm_data$median_max))/sd(perm_data$median_max)


set.seed(0909)

sim_data = lapply((1:200),
                  function(x)
                  {
                    demo = rnorm(40, 5, 10) %>% 
                      as_tibble() %>% 
                      mutate(nfacet = rep(1:8, each = 5),
                             distance = rep(1:8, 5)) %>% 
                      group_by(nfacet) %>%
                      summarise(max = max(value)) %>% 
                      summarise(median_max = median(max))
                    
                    perm_data = lapply((1:100), function(x){
                      rnorm(40, 5, 10) %>% 
                        as_tibble() %>% 
                        mutate(nfacet = rep(1:8, each = 5),
                               distance = rep(1:8, 5)) %>% 
                        group_by(nfacet) %>%
                        summarise(max = max(value)) %>% 
                        summarise(median_max = median(max))
                    }) %>% bind_rows(.id = "perm_id")
                    
                    mmpd_obs = (demo - mean(perm_data$median_max))/sd(perm_data$median_max)
                  })


new_sim_data = sim_data %>% 
  bind_rows(.id = "sim_id") 
library(ggplot2)
ggplot(data = new_sim_data) + geom_histogram( aes(x = median_max))

