calcTE.ci <- function(estimate, lower, upper, level = 0.95,
                      sm = "",
                      logtransf = ifelse(sm %in% c("OR", "RR", "HR"),
                                         TRUE, FALSE)) {
  
  
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  if (any(level <= 0 | level >= 1))
    stop("no valid level for confidence interval")
  ##
  alpha <- 1 - level
  
  
  ##
  ## Parmar et al. (1998), Stat Med
  ##
  ## Section 4.1 Indirect variance estimation
  ##
  if (logtransf) {
    TE <- log(estimate)
    lower <- log(lower)
    upper <- log(upper)
  }
  else{
    TE <- estimate
    lower <- lower
    upper <- upper
    }
  ##
  ## Equation (7)
  ##
  varTE <- ((upper - lower) / (2 * qnorm(alpha / 2, lower.tail = FALSE)))^2
  seTE <- sqrt(varTE)
  
  
  res <- list(TE = TE, seTE = seTE, sm = sm,
              ##
              estimate = estimate, lower = lower, upper = upper,
              level = level, logtransf = logtransf)
  ##
  res
}
