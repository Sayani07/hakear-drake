the_plan <-
  drake_plan(

    ## Plan targets in here.

    # create simulated data with n levels from distribution sim_dist
    create_data_sim1 <- function(nlevel, sim_dist)

      # sample with repition from the data set create_data_sim1 nperm times
      create_data_sim1_nperm <- function(
                                         create_data_sim1, nperm)
        # calculate the mean and sd of nperm max #observations and compute norm_max =  (max - mean(x))/sd(x) where max is the max of create_data_sim1
        create_norm_max1 <- function(create_data_sim1, create_data_sim1_nperm)

          # repeat last three steps nsim types, thus computing norm_max nsim times
          create_sim_n <- function(create_norm_max1, nsim)
            # plot distribution of nsim norm_max

            plot_sim_n <- function(create_sim_n) 
              null_distribution <- target(
                command = {
                  rmarkdown::render(knitr_in("doc/null_distribution.Rmd"))
                  file_out("doc/null_distribution.pdf")
                }
  )
  )
