calcTE.ci <- function(estimate, lower, upper, level = 0.95, sm = "",
                      logtransf = meta:::is.relative.effect(sm)) {
  
  
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  
  
  ##
  ## Check arguments
  ##
  if (missing(estimate))
    stop("Mandatory argument 'estimate' missing")
  if (missing(lower))
    stop("Mandatory argument 'lower' missing")
  if (missing(upper))
    stop("Mandatory argument 'upper' missing")
  ##
  if (any (estimate <= lower))
    stop("Estimate must be larger than lower limit")
  if (any (estimate >= upper))
    stop("Estimate must be smaller than upper limit")
  if (any (lower >= upper))
    stop("Lower limit must be smaller than upper limit")
  ##
  meta:::chklevel(level)
  ##
  meta:::chklogical(logtransf)
  
  
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
  varTE <- ((upper - lower) /
            (2 * qnorm((1 - level) / 2, lower.tail = FALSE)))^2
  seTE <- sqrt(varTE)
  
  
  res <- list(TE = TE, seTE = seTE, sm = sm,
              ##
              estimate = estimate, lower = lower, upper = upper,
              level = level, logtransf = logtransf)
  ##
  res
}
