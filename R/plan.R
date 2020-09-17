library(drake)
the_plan <-
  no_deps(
    drake_plan(

      ## Plan targets in here.
      
      # simulate panel data with x levels and facets
       
      sim_panel_data  = sim_panel(nx = 4, 
                                        nfacet = 7,
                                        ntimes = 500,
                                        sim_dist = distributional::dist_normal(5, 10)),
      
      
      # compute quantiles of simulated panel data 
      
      sim_panel_quantiles  = compute_quantiles(sim_panel_data,
                                        quantile_prob = seq(0.01, 0.99, 0.01)),
       
      # compute pairwise JS distances for each facet
      
      distance_panel_data  = distance_panel(sim_panel_quantiles, #method = "JS",
                                            dist_ordered = FALSE),
      
       # compute mpd - normalised max pairwise distances for each facet
      normx_data = mpd(distance_panel_data,
                       nperm = 2000),
       # compute mpd - normalised max pairwise distances for each facet       
       normfacet_data = mmpd(normx_data,
                             nperm = 2000),

      # analysis file
      target_name = target(
        command = {
          rmarkdown::render(knitr_in("doc/mmpd_dist.Rmd"))
          file_out("doc/mmpd_dist.html")
        }
      )
    )
  )

