##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_panel_data
##' @param quantile_prob
##' @param method
##' @return
##' @author Sayani07
##' @export
distance_panel <- function(sim_panel_quantiles,
                           quantile_prob = seq(0.01, 0.99, 0.01),
                           dist_ordered = TRUE) {
  ncoly <- sim_panel_quantiles %>% distinct(id_facet) %>% nrow()
  nrowy <- sim_panel_quantiles %>% distinct(id_x) %>% nrow()
  
  #(seq_len(nrowy)) %>% 
  
  
  #k_range <- seq_len(ncoly) %>% data.frame()
  
    lapply(seq_len(ncoly), function(k) {
      
    #nperm_data_n <- mclapply(2:nperm, 
     #                        function(i) 
        
        #i_range <- data.frame(1:(nrowy-1))
        dist_facet <- lapply(1:(nrowy-1), function(i) {
          
        #j_range <-   data.frame(((i + 1):nrowy))
          
          dist <- lapply((i + 1):nrowy, function(j) {
              m1 <- sim_panel_quantiles %>% 
                unnest(cols = c(sim_data_quantile)) %>% 
                dplyr::filter(id_facet == k, id_x == i)  %>%
                select(sim_data_quantile)
              
              m2 <- sim_panel_quantiles %>% 
                unnest(cols = c(sim_data_quantile)) %>% 
                dplyr::filter(id_facet == k, id_x == j)%>%
                select(sim_data_quantile) 
              
              z <- JS(prob = quantile_prob, m1$sim_data_quantile, m2$sim_data_quantile)
              if (dist_ordered) {
                if (j != i + 1) {
                  z <- NA
                }
              }
              return(z)
            }) %>% t() %>% as_tibble(.name_repair = "unique")
          
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
