library(drake)
library(dplyr)
library(tidyr)
the_plan <-
    drake_plan(

      ## Plan targets in here.
      
      # simulate many panel data with x levels and facets
       set.seed(9999),
       sim_null_orig  = sim_panel_grid (range_nx = c(2, seq(3,15,2)), 
                                          range_nfacet = c(2, seq(3,15,2)),
                                          ntimes = 500,
                                          sim_dist = distributional::dist_normal(5, 10)),
       
      
       #sim_null_max_dist = why_normalise(sim_null_orig),
        
      
      # plot panel grid
      
      #plot_sim_null = plot_panel_grid(sim_null_orig),
      #ggplot(sim_null_orig, aes(x = sim_data)) + geom_histogram() + facet_grid(nx~nfacet)
      
      # compute mmpd for each panel
      set.seed(9999),
      mmpd_null_orig = compute_mmpd_panel_grid(sim_null_orig,
                                     quantile_prob = seq(0.01, 0.99, 0.01),
                                     dist_ordered = TRUE,
                                     nperm = 15),
      # compute mmpd distribution for each panel
    set.seed(54321),  
    mmpd_dist_null_grid =   compute_mmpd_null_dist(sim_null_orig,
                                              nsim = 100),
     
    
    # visualise mmpd distribution for entire panel grid
    plot_dist_null_grid =   plot_mmpd_null_grid(mmpd_dist_null_grid,
                                                mmpd_null_orig),  
    # analysis file
    report = target(
      command = {
        rmarkdown::render(knitr_in("doc/mmpd_null_dist.Rmd"))
        file_out("doc/mmpd_null_dist.html")
      }
    ),
  
    
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

)
