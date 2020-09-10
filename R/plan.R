the_plan <-
  drake_plan(

    ## Plan targets in here.

    # create simulated data with n levels from distribution sim_dist
    #sim = distributional::dist_normal(5, 10)
    sim_data_1 = create_data_sim1(nlevel = 2, sim_dist),
    #create_data_sim1 = function(nlevel, sim_dist)

      # sample with repition from the data set create_data_sim1 nperm times
    sim_data_n = create_data_sim1_nperm(sim_data_1, nperm = 2000),
        # calculate the mean and sd of nperm max #observations and compute norm_max =  (max - mean(x))/sd(x) where max is the max of create_data_sim1
    norm_data1 = create_norm_max1(sim_data_n),

          # repeat last three steps nsim types, thus computing norm_max nsim times
    norm_data_n = create_sim_n(nlevel = 2, nsim = 500,nperm = 1000),
            # plot distribution of nsim norm_max

    plot_norm_n = plot_sim_n(norm_data_n),
              # analysis file
              target_name = target(
                command = {
                  rmarkdown::render(knitr_in("doc/null.Rmd"))
                  file_out("doc/null.html")
                }
              )
    
  )
