library(drake)
library(dplyr)
library(tidyr)
the_plan <-
    drake_plan(
      # Simulation study
      # fixed global parameters
      # set simulation parameters
      seed1 = set.seed(9999),# seed while generating random observations
      # distance between which quantiles are computed,
      quantile_prob_vec = seq(0.01, 0.99, 0.01),
      # ordered or un-ordered categories of x levels while computing distances
      dist_ordered_val = TRUE, 
      
      #' variable (maybe) global parameters
      # grid with how many facet levels
      #range_nfacet_vec = c(2, 3, 4, 5, 7, 9, 14, 21, 24, 31)
      range_nfacet_vec = c(2, 3, 4, 5, 7,9, 14),
      # grid with how many x levels
      #range_nx_vec = c(2, 3, 4, 5, 7, 9, 14, 21, 24, 31)
      range_nx_vec = c(2, 3, 4, 5, 7, 9, 14),
      
      # number of observations to be generated
      #ntimes_val = 500
      ntimes_val = 500,
      # number of permutation of data done to compute estimates
      #nperm_val = 10
      nperm_val = 20,
      # number of permutations of data done to display histogram for each panel
      nsim_val = 100,
      #nsim_val = 2,
      
      # Null parameters
      mean = 5,
      sd = 10,
      shape = 5,
      scale = 1,
      rate = 1/5,
      
      nx = 4,
      nfacet = 5,
      
      # Design 1: Null
      sim_null_normal = function(nx, nfacet){
        rep(distributional::dist_normal(mean, sd), nx*nfacet)
      },
      sim_null_exp = function(nx, nfacet){
        distributional::dist_exponential(rate)},
      
      sim_null_gamma = function(nx, nfacet){
        distributional::dist_gamma(shape, rate)},
      
      sim_null_weibull = function(nx, nfacet){
        distributional::dist_weibull(shape, scale)},
      
      # Design 2: Alternate where only distributions across facet varies     
      sim_varf_normal = function(nx, nfacet){
        rep(dist_normal(seq(mean,mean*nfacet, by = mean), sd), each = nx)},
      
      sim_varf_exp = function(nx, nfacet){rep(dist_exponential(seq(rate, rate*nfacet, by = rate)), each = nx)},
      
      sim_varf_gamma = function(nx, nfacet){rep(dist_gamma(seq(shape, shape*nfacet, by = shape), rate), each = nx)},
      
      sim_varf_weibull = function(nx, nfacet){rep(dist_weibull(seq(shape, shape*nfacet, by = shape), scale), each = nx)},
      
      # Design 3: Alternate where only distributions across x-varies
      
      sim_varx_normal = function(nx, nfacet){rep(dist_normal(seq(mean,mean*nx, by = mean), sd), nfacet)},
      
      sim_varx_exp = function(nx, nfacet){rep(dist_exponential(seq(rate, rate*nx, by = rate)), nfacet)},
      
      sim_varx_gamma = function(nx, nfacet){rep(dist_gamma(seq(shape, shape*nx, by = shape), rate),nfacet)},
      
      sim_varx_weibull = function(nx, nfacet){rep(dist_weibull(seq(shape, shape*nx, by = shape), scale), nfacet)},
      
      
      # Design 4: Alternate where all distribution varies
      
      sim_varall_normal = function(nx, nfacet){dist_normal(seq(mean,mean*nfacet*nx, by = mean), sd)},
      
      sim_varall_exp = function(nx, nfacet){dist_exponential(seq(rate, rate*nfacet*nx, by = rate))},
      
      sim_varall_gamma = function(nx, nfacet){rep(dist_gamma(seq(shape, shape*nfacet*nx, by = shape), rate))},
      
      sim_varall_weibull = function(nx, nfacet){dist_weibull(seq(shape, shape*nfacet*nx, by = shape), scale)},
      
# create grid for each of the 16 designs

# Null design      
null_normal = 
  create_mmpd_grid(
    sim_dist = sim_null_normal,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

null_exp = 
  create_mmpd_grid(
    sim_dist = sim_null_exp,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

null_gamma = 
  create_mmpd_grid(
    sim_dist = sim_null_gamma,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

null_weibull = 
  create_mmpd_grid(
    sim_dist = sim_null_weibull,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

# varf design      
varf_normal = 
  create_mmpd_grid(
    sim_dist = sim_varf_normal,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varf_exp = 
  create_mmpd_grid(
    sim_dist = sim_varf_exp,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varf_gamma = 
  create_mmpd_grid(
    sim_dist = sim_varf_gamma,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varf_weibull = 
  create_mmpd_grid(
    sim_dist = sim_varf_weibull,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

# varx design      
varx_normal = 
  create_mmpd_grid(
    sim_dist = sim_varx_normal,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varx_exp = 
  create_mmpd_grid(
    sim_dist = sim_varx_exp,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varx_gamma = 
  create_mmpd_grid(
    sim_dist = sim_varx_gamma,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varx_weibull = 
  create_mmpd_grid(
    sim_dist = sim_varx_weibull,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),


# varall design      
varall_normal = 
  create_mmpd_grid(
    sim_dist = sim_varall_normal,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varall_exp = 
  create_mmpd_grid(
    sim_dist = sim_varall_exp,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varall_gamma = 
  create_mmpd_grid(
    sim_dist = sim_varall_gamma,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val),

varall_weibull = 
  create_mmpd_grid(
    sim_dist = sim_varall_weibull,
    range_nfacet = range_nfacet_vec, 
    range_nx = range_nx_vec, 
    ntimes = ntimes_val, 
    nperm = nperm_val,
    nsim = nsim_val, 
    quantile_prob = quantile_prob_vec, 
    dist_ordered = dist_ordered_val)
)





#'       ## Plan targets in here.
#'       
#'       # simulate many panel data with x levels and facets
#'        sim_null_orig  = sim_panel_grid (range_nx = range_nx_vec,                             range_nfacet =
#'         range_nfacet_vec,
#'                                           ntimes = 500,
#'                                           sim_dist = distributional::dist_normal(5, 10)),
#'        
#'      # neat way to look at it
#'      sim_null_neat  = sim_null_orig %>% group_by(nfacet, nx) %>% nest(),
#'        
#'        
#'        # split it into groups (each with a list) so that you can distribute it across cores
#'        
#'     sim_null_split = group_split(sim_null_orig,
#'                                    nx, nfacet),
#'       
#'        #sim_null_max_dist = why_normalise(sim_null_orig),
#'         
#'       
#'       # plot panel grid
#'       # as facet and x-axis levels increase. there are more observation as for each combination ntimes observations are generated
#'       
#'       #'plot_sim_null = sim_null_orig %>% 
#'       #'ggplot() +
#'       #'geom_histogram(aes(x = sim_data)) + 
#'       #'facet_grid(nx~nfacet),
#'       
#'       # compute mmpd for each panel in the entire grid
#' 
#'     mmpd_null_orig = mclapply(
#'       sim_null_split,
#'       function(x){
#'         compute_mmpd_panel(x,
#'                            nperm = 10, 
#'                            dist_ordered = TRUE)
#'       }),
#'     # 
#'     #   seq_len(length(sim_null_split)),
#'     #                           function(i){
#'     #  
#'     # }),
#'     # 
#'     # see how observed mmpd looks for the grid
#'     
#' #'    sim_null_neat %>% select(1:2) %>%
#' #'     ungroup() %>% bind_cols(bind_rows(mmpd = unlist(mmpd_null_orig)))
#' #'     
#' 
#'       # compute mmpd distribution for each panel
#' mmpd_null_dist = mclapply(
#'   seq_len(length(sim_null_split)),
#'   function(i){
#'   replicate(nsim,
#'             {compute_mmpd_panel(shuffle_x_for_each_facet(sim_null_split[[i]]),
#'                                 nperm = 2, 
#'                                 dist_ordered = TRUE)})
#' }),
#' 
#' 
#' null_neat  = sim_null_neat %>% select(1:2) %>% 
#'   ungroup() %>% group_split(nx,nfacet) ,
#' 
#' mmpd_dist_null_data = t(mapply(bind_cols, null_neat, mmpd = mmpd_null_dist)) %>% as_tibble() %>% unnest(c(nfacet, nx, mmpd)),
#' 
#' mmpd_null_orig_data = t(mapply(bind_cols, null_neat, mmpd =  mmpd_null_orig)) %>% as_tibble() %>% unnest(c(nfacet, nx, mmpd)),
#' 
#'     
#'     # visualise mmpd distribution for entire panel grid
#'     # plot_data  =  plot_mmpd_null_grid(mmpd_dist_null_data, mmpd_null_orig_data),  
#' 
#' joined_data = left_join(mmpd_dist_null_data, mmpd_null_orig_data,
#'                          by = c("nx", "nfacet")),
#' 
#' # number of times shuffled difference exceed true difference
#' 
#' # compute_p_value = joined_data %>% 
#' #   group_by(nx, nfacet) %>% 
#' #   summarize(p_value = mean(abs(mmpd.x)> abs(mmpd.y))),
#' # 
#' # 
#' # ggplot() + 
#' #   geom_histogram(data = mmpd_dist_null_data, 
#' #                  aes(x = mmpd))  + 
#' #   geom_vline(data = joined_data, 
#' #              aes(xintercept = mmpd.y), colour = "red") +
#' #   geom_text(data = compute_p_value, size = 3,  
#' #             aes(x = -Inf,
#' #                 y =  Inf,
#' #                 label = paste("p-value:",p_value),
#' #                 hjust   = 0,
#' #                 vjust   = 1)) + 
#' #   facet_grid(nx ~ nfacet),
#' 
#'     # analysis file
#'     report = target(
#'       command = {
#'         rmarkdown::render(knitr_in("doc/mmpd_null_dist.Rmd"))
#'         file_out("doc/mmpd_null_dist.html")
#'       }
#'     ),
  
    
    # testing power
    
    # simulate many panel data with x levels and facets
    
    # 
    # sim_panel_data = slct_lvl_orig(sim_null_orig,
    #                                  nx = 2,
    #                                  nfacet = 3),
    # 
    
    
    
    #    
    #    
    # 
    # 
#' @examples
#' set.seed(5000)
#'    sim_panel_data  = sim_panel(nx = 7,
#'                                    nfacet = 4,
#'                                    ntimes = 500,
#'                                    sim_dist = distributional::dist_normal(5, 10))
#'     #
#'       # compute quantiles of simulated panel data
#' 
#'        sim_panel_quantiles  = compute_quantiles(sim_panel_data,
#'                                          quantile_prob = seq(0.01, 0.99, 0.01))
#' 
#'       # compute pairwise JS distances for each facet
#' set.seed(5000)
#'       distance_panel_data  = distance_panel(sim_panel_quantiles, #method = "JS",
#'                                            dist_ordered = FALSE)
#'       #
#' set.seed(5000)
#' #        compute mpd - normalised max pairwise distances for each facet
#'       normx_data = mpd(sim_panel_data, distance_panel_data,
#'                        nperm = 2)
#'       #  # compute mmpd - no
#'       
#'       
#' set.seed(5000)      
#'       rmalised max pairwise distances across all facets
#'       #  # change the function names
#'        normfacet_data = mmpd(sim_panel_data, 
#'        normx_data,
#'                               nperm = 20)

