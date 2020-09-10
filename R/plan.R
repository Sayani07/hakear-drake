library(drake)
the_plan <-
  drake_plan(

    ## Plan targets in here.

    # create simulated data with n levels from distribution sim_dist
    #sim = distributional::dist_normal(5, 10)
    sim_data_1 = create_data_sim1(nlevel = 2, sim_dist),
    #create_data_sim1 = function(nlevel, sim_dist)

      # sample with repition from the data set create_data_sim1 nperm times
    sim_data_n = create_data_sim1_nperm(sim_data_1, nperm = 200),
        # calculate the mean and sd of nperm max #observations and compute norm_max =  (max - mean(x))/sd(x) where max is the max of create_data_sim1
    norm_data1 = create_norm_max1(sim_data_n),

          # repeat last three steps nsim types, thus computing norm_max nsim times
    norm_data_n = create_sim_n(nlevel = 2, nsim = 50,nperm = 200),
            # plot distribution of nsim norm_max

    plot_norm_n = plot_sim_n(norm_data_n),
    
    # Do it for many levels
    
    norm_data_n_nlev =  create_sim_nlev(nlevel = 2:20, nsim = 50, nperm = 100),
    
    plot_norm_n_nlev = plot_sim_nlev(norm_data_n_nlev, nlev = 5:20),
  
              # analysis file
    target_name = target(
      command = {
        rmarkdown::render(knitr_in("doc/null_distribution.Rmd"))
        file_out("doc/null_distribution.html")
      }
    )
    
    
  )
