#' @title computing distances between quantile categories
#'
#' @param sim_panel_quantiles quantile data
#' @param dist_ordered if categories are ordered
#' @param quantile_prob numeric vector of probabilities with value #'in [0,1]  whose sample quantiles are wanted. Default is set to #' "decile" plot
#'
#' @return
#' @author Sayani07
#' @export
distance_panel <- function(sim_panel_quantiles,
                           quantile_prob = seq(0.01, 0.99, 0.01),
                           dist_ordered = TRUE) {
  # ncoly <- sim_panel_quantiles %>%
  #   distinct(id_facet) %>%
  #   nrow()
  # nrowy <- sim_panel_quantiles %>%
  #   distinct(id_x) %>%
  #   nrow()
  
  # range of i, j and k are defined in this way since some cyclic granularities start from 0 and others from 1 -  it was creating a problem while filtering in m1 and m2, where m2 was leading to a tibble of 0 rows and JS function was failing
  
  if(class(sim_panel_quantiles$id_x) %in% c("character", "integer"))
  {
    sim_panel_quantiles$id_x = as.numeric(as.factor(sim_panel_quantiles$id_x)) %>% factor()
  }
  if(class(sim_panel_quantiles$id_facet) %in% c("character", "integer"))
  {
    sim_panel_quantiles$id_facet = as.numeric(as.factor(sim_panel_quantiles$id_facet)) %>% factor()
  }
  
  i_start <- levels(sim_panel_quantiles$id_x) %>%
    as.numeric() %>%
    min()
  
  i_stop <- levels(sim_panel_quantiles$id_x) %>%
    as.numeric() %>%
    max() - 1
  
  # j_start = i+1
  
  j_stop <- levels(sim_panel_quantiles$id_x) %>%
    as.numeric() %>%
    max()
  
  k_start <- levels(sim_panel_quantiles$id_facet) %>%
    as.numeric() %>%
    min()
  
  k_stop <- levels(sim_panel_quantiles$id_facet) %>%
    as.numeric() %>%
    max()
  
  
  # (seq_len(nrowy)) %>%
  
  
  # k_range <- seq_len(ncoly) %>% data.frame()
  
  lapply(k_start:k_stop, function(k) {
    
    # nperm_data_n <- lapply(2:nperm,
    #                        function(i)
    
    # i_range <- data.frame(1:(nrowy-1))
    dist_facet <- lapply(i_start:i_stop, function(i) {
      
      # j_range <-   data.frame(((i + 1):nrowy))
      
      dist <- lapply((i + 1):j_stop, function(j) {
        m <- sim_panel_quantiles %>%
          unnest(cols = c(sim_data_quantile))
        
        m1 <- m[m$id_facet == k & m$id_x == i, ]
        
        m2 <- m[m$id_facet == k & m$id_x == j, ]
        
        z <- JS(prob = quantile_prob, m1$sim_data_quantile, m2$sim_data_quantile)
        if (dist_ordered) {
          if (j != i + 1) {
            z <- NA
          }
        }
        
        return(z)
      }) %>%
        t() %>%
        as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
      
      
      dist %>%
        select_if(not_is_na)
    }) %>% bind_cols()
    
    bind_cols(id_facet = k, dist_facet = dist_facet)
  }) %>% bind_rows()
}



not_is_na <- function(x) any(!is.na(x))

JS <- function(prob, q, p) {
  # Compute approximate densities
  x <- seq(min(q, p), max(q, p), l = 201)
  qpmf <- pmf(x, prob, q)
  ppmf <- pmf(x, prob, p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5 * (sum(stats::na.omit(ppmf * log(ppmf / m, base = 2))) +
                                  sum(stats::na.omit(qpmf * log(qpmf / m, base = 2)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q) {
  qcdf <- stats::approx(q, p, xout = x, yleft = 0, yright = 1, ties = max, na.rm = TRUE)$y
  qpmf <- c(0, diff(qcdf) / (x[2] - x[1]))
  return(qpmf / sum(qpmf))
}
