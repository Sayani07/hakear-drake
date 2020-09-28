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
  ncoly <- ncol(sim_panel_quantiles)
  nrowy <- nrow(sim_panel_quantiles)
  
  #(seq_len(nrowy)) %>% 
  (seq_len(nrowy)) %>% 
    purrr::map_dfr(function(k) {
      dist_facet <- (2:(ncoly-1)) %>%
        purrr::map_dfc(function(i) {
          dist <- ((i + 1):ncoly) %>%
            purrr::map_dfc(function(j) {
              m1 <- sim_panel_quantiles %>% magrittr::extract(k,i) %>% unlist()
              m2 <- sim_panel_quantiles %>% magrittr::extract(k,j) %>% unlist()
              z <- JS(prob = quantile_prob, m1, m2)
              if (dist_ordered) {
                if (j != i + 1) {
                  z <- NA
                }
              }
              return(z)
            })
          dist %>% 
            select_if(not_is_na) 
        })
      bind_cols(id_facet = k, dist_facet = dist_facet)
      
    })
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
  qcdf <- stats::approx(q, p, xout = x, yleft = 0, yright = 1, ties = mean)$y
  qpmf <- c(0, diff(qcdf) / (x[2] - x[1]))
  return(qpmf / sum(qpmf))
}
