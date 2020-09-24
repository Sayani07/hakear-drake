library(drake)
library(dplyr)
library(tidyr)
the_plan <-
    drake_plan(

      ## Plan targets in here.
      
      # simulate many panel data with x levels and facets
      set.seed(9999),
      sim_null_orig  = sim_panel_grid(range_nx = 2:5, 
                                        range_nfacet = 2:5,
                                        ntimes = 500,
                                        sim_dist = distributional::dist_normal(5, 10)),
      
      # plot panel grid
      
      #plot_sim_null = plot_panel_grid(sim_null_orig),
      #ggplot(sim_null_orig, aes(x = sim_data)) + geom_histogram() + facet_grid(nx~nfacet)
      
      # compute mmpd for each panel
      
      mmpd_null_orig = compute_mmpd_panel_grid(sim_null_orig,
                                     quantile_prob = seq(0.01, 0.99, 0.01),
                                     dist_ordered = TRUE,
                                     nperm = 200),
      # compute mmpd distribution for each panel
    set.seed(54321),  
    mmpd_dist_null_grid =   compute_mmpd_null_dist(sim_null_orig,
                                              nsim = 10),
     
    
    # visualise mmpd distribution for entire panel grid
    plot_dist_null_grid =   plot_mmpd_null_grid(mmpd_dist_null_grid,
                                                mmpd_null_orig),  
  
    
    # testing power
    
    # simulate many panel data with x levels and facets
    
    # 
    # sim_panel_data = slct_lvl_orig(sim_null_orig,
    #                                  nx = 2,
    #                                  nfacet = 3),
    # 
    
    
    
       
      # 
      # sim_panel_data = slct_lvl_orig(sim_null_orig,
      #                                  nx = 2,
      #                                  nfacet = 3),
      # 

      
      
      # compute quantiles of simulated panel data 
      
      # sim_panel_quantiles  = compute_quantiles(sim_panel_data,
      #                                   quantile_prob = seq(0.01, 0.99, 0.01)),
       
      # compute pairwise JS distances for each facet
      
      # distance_panel_data  = distance_panel(sim_panel_quantiles, #method = "JS",
      #                                       dist_ordered = FALSE),
      
       # compute mpd - normalised max pairwise distances for each facet
      # normx_data = mpd(distance_panel_data,
      #                  nperm = 2000),
      #  # compute mmpd - normalised max pairwise distances across all facets       
      #  # change the function names 
      #  normfacet_data = mmpd(normx_data,
      #                        nperm = 2000),

      # analysis file
      report = target(
        command = {
          rmarkdown::render(knitr_in("doc/mmpd_null_dist.Rmd"))
          file_out("doc/mmpd_null_dist.html")
        }
      )
    )
