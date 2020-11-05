compute_p_value = varx_exp %>%
  group_by(nx, nfacet) %>%
  summarize(p_value = mean(abs(mmpd.x)> abs(mmpd.y)))

ggplot() +
  geom_histogram(data = varx_exp,
                 aes(x = mmpd.x))  +
  geom_vline(data = varx_exp,
             aes(xintercept = mmpd.y), colour = "red") +
  geom_text(data = compute_p_value, size = 3,
            aes(x = -Inf,
                y =  Inf,
                label = paste("p-value:",p_value),
                hjust   = 0,
                vjust   = 1)) +
  facet_grid(nx ~ nfacet)

# all together
# loadd()
# null_all = bind_rows(normal = null_normal,
#                      exponential = null_exp,
#                      gamma = null_gamma,
#                      weibull = null_weibull,
#                      .id = "distribution")
# 
# 
# varf_all = bind_rows(normal = varf_normal,
#                        exponential = varf_exp,
#                        gamma = varf_gamma,
#                        weibull = varf_weibull,
#                        .id = "distribution")
# 
# varx_all = bind_rows(normal = varx_normal,
#                      exponential = varx_exp,
#                      gamma = varx_gamma,
#                      weibull = varx_weibull,
#                      .id = "distribution")
# 
# 
# varall_all = bind_rows(normal = varall_normal,
#                      exponential = varall_exp,
#                      gamma = varall_gamma,
#                      weibull = varall_weibull,
#                      .id = "distribution")
# 
# design_all <- bind_rows(null= null_all,
#                         vary_f = varf_all,
#                         vary_x = varx_all,
#           vary_all = varall_all,
#           .id = "design")

design_all = read_rds("data/design_all.rds")

compute_p_value = design_all %>%
  group_by(design, distribution, nx, nfacet) %>%
  summarize(p_value = mean(abs(mmpd.x)> abs(mmpd.y)))

compute_p_value$design = 
  factor(compute_p_value$design,
         levels = c("null","vary_f", "vary_x", "vary_all"))

plot_check = ggplot(compute_p_value
                    %>% dplyr::filter(distribution=="gamma")) +
  geom_point(aes(x=design, y = p_value, 
                 color=distribution),
                 alpha = 0.5)+
  facet_grid(nx~nfacet) +
  geom_line(aes(x = design, y = p_value, group = distribution, color = distribution))
plot_check
  
# if all levels have all dist
compute_p_value %>% 
  pivot_wider(id_cols = -2,
              names_from = distribution,
              values_from = p_value) %>% View


# visualize time
time_data = read_rds("data/design_time.rds")

time_data$design = 
  factor(time_data$design,
         levels = c("null","vary_f", "vary_x", "vary_all"))

time_data %>% ggplot(aes(x= as.factor(nx),  y = time_dist, colour = as.factor(nfacet), group = nfacet)) + geom_point() + geom_line() +
  facet_grid(distribution~design)
