library(drake)
the_plan <-
  no_deps(
    drake_plan(

      ## Plan targets in here.

      # create simulated data with n levels from distribution sim_dist
      #sim_data_1 = create_data_sim1(nlevel = 2, sim_dist = distributional::dist_normal(5, 10)),
      # create_data_sim1 = function(nlevel, sim_dist)

      # sample with repition from the data set create_data_sim1 nperm times
      #sim_data_n = create_data_sim1_nperm(sim_data_1, nperm = 200),
      # calculate the mean and sd of nperm max #observations and compute norm_max =  (max - mean(x))/sd(x) where max is the max of create_data_sim1
      #norm_data1 = create_norm_max1(sim_data_n),

      # repeat last three steps nsim types, thus computing norm_max nsim times
      norm_data_n = create_sim_n(nlevel = 2, nsim = 50, nperm = 200),
      # plot distribution of nsim norm_max

      plot_norm_n = plot_sim_n(norm_data_n),

      # Do it for many levels

      norm_data_n_nlev = create_sim_nlev(nlevel = 2:30, nsim = 500, nperm = 2000),

      plot_norm_n_nlev = plot_sim_nlev(norm_data_n_nlev, nlev = 2:20),



      # analysis file
      target_name = target(
        command = {
          rmarkdown::render(knitr_in("doc/null_distribution.Rmd"))
          file_out("doc/null_distribution.html")
        }
      )
    )
  )

my_plan <-
  no_deps(
    drake_plan(

      ## Plan targets in here.
      data_orig = create_data_sim1(
        nlevel = 6,
        sim_dist = dist_normal(5, 10)
      ),

      data_panel = create_panel(data_orig, ncol = 3, nrow = 2),
      data_boot = boot_panel(data_panel, nperm = 2000),
      norm_boot = compute_norm(data_boot, method = "std"),

      # repeat the above process nsim times to see distribution
      set.seed(54321),
      data_sim = create_simulate_norm(
        nlevel = 6,
        ncol = 3,
        nrow = 2,
        sim_dist = dist_normal(5, 10),
        nperm = 500,
        nsim = 200
      ),

      plot_sim = plot_simulations(data_sim),

      data_sim_3_7 = create_simulate_norm(
        nlevel = 21,
        ncol = 7,
        nrow = 3,
        sim_dist = dist_normal(5, 10),
        nperm = 500,
        nsim = 200
      ),


      data_sim_7_3 = create_simulate_norm(
        nlevel = 21,
        ncol = 3,
        nrow = 7,
        sim_dist = dist_normal(5, 10),
        nperm = 500,
        nsim = 200
      ),

      plot_sim_7_3 = plot_simulations(data_sim_7_3),



      target_name = target(
        command = {
          rmarkdown::render(knitr_in("doc/panel_null.Rmd"))
          file_out("doc/panel_null.html")
        }
      )
    )
  )
