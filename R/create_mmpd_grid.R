##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_dist
##' @param range_nfacete
##' @param range_nx
##' @param ntimes
##' @param nperm
##' @param nsim
##' @param quantile_prob
##' @param dist_ordered
##' @return
##' @author Sayani Gupta
##' @export
create_mmpd_grid <- function(
  sim_dist = sim_null_normal,
  range_nfacet = 2:4,
  range_nx = 2:4,
  ntimes = 50,
  nperm = 10,
  nsim = 100,
  quantile_prob = seq(0.01, 0.99, 0.01),
  dist_ordered = TRUE) {
  
  sim_null_orig <-
    sim_panel_grid(
      range_nx, range_nfacet,
      ntimes,
      sim_dist
    ) %>%
    unnest(c(data))
  
  sim_null_split <- sim_null_orig %>%
    group_by(nx, nfacet) %>%
    group_split()
  
  # observed mmpd
  tic.clearlog()
  tic("mmpd value")
  mmpd_null_orig <- suppressMessages(lapply(
    sim_null_split,
    function(x) {
      tic("mmpd value")
      obj = compute_mmpd_panel(
        x,
        quantile_prob,
        dist_ordered,
        nperm
      )
      t <- toc(log = TRUE, quiet = TRUE)
      return(obj)
    }
  ))
  log.lst <- tic.log(format = FALSE)
  timings <- tibble(time_obs = unlist(lapply(log.lst, function(x) x$toc - x$tic)))

  
  # distribution of mmpd
  tic.clearlog()
  mmpd_null_dist <-
    suppressMessages(lapply(
      sim_null_split,
      function(x) {
        tic("mmpd distribution")
        obj = replicate(nsim, 
                  {
          compute_mmpd_panel(shuffle_x_for_each_facet(x),
                             quantile_prob = quantile_prob,
                             dist_ordered = dist_ordered,
                             nperm
          )
        })
        t <- toc(log = TRUE, quiet = TRUE)
        return(obj)
      }
    ))
  
  log.dist <- tic.log(format = FALSE)
  timings_dist <- tibble(time_dist = unlist(lapply(log.dist, function(x) x$toc - x$tic)))

  
  sim_null_neat <- sim_null_orig %>%
    group_by(nfacet, nx) %>%
    nest()
  
  null_neat <- sim_null_neat %>%
    select(1:2) %>%
    ungroup() %>%
    group_split(nx, nfacet)
  
  mmpd_dist_null_data <- t(mapply(bind_cols, null_neat, mmpd = mmpd_null_dist)) %>%
    as_tibble() %>%
    unnest(c(nfacet, nx, mmpd))
  
  mmpd_null_orig_data <- t(mapply(bind_cols, null_neat, mmpd = mmpd_null_orig)) %>%
    as_tibble() %>%
    unnest(c(nfacet, nx, mmpd))
  
  
  # visualise mmpd distribution for entire panel grid
  # plot_data  =  plot_mmpd_null_grid(mmpd_dist_null_data, mmpd_null_orig_data),
  
  joined_data <- left_join(mmpd_dist_null_data, mmpd_null_orig_data,
                           by = c("nx", "nfacet")
  )
  
  
  # compute_p_value = joined_data %>%
  #   group_by(nx, nfacet) %>%
  #   summarize(p_value = mean(abs(mmpd.x)> abs(mmpd.y)))
  #
  # dist_name = !!sim_dist
  #
  # save(joined_data, file = glue::glue("data/",substitute(dist_name),".rds"))
  time_data <- sim_null_orig %>% ungroup %>%  select(nfacet, nx) %>% expand.grid() %>% unique() %>% arrange(nfacet, nx) %>% bind_cols(timings, timings_dist)
  
  list(joined_data, time_data)
  
  # name_file = str_remove(!!sim_dist, "sim_")
  # 
  # write_csv(time_data, file = paste0("data/", name_file, ".csv"))
  
}
