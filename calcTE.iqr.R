calcTE.iqr <- function(median, lower, upper,
                       sm = "",
                       logtransf = meta:::is.relative.effect(sm) {
  
  
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  
  
  ##
  ## Estimate mean and its standard error from median and interquartile range
  ## using method by Wan et al. (2014), BMC Med Res Methodol 14 (1): 135
  ##
  
  
  ##
  ## Check arguments
  ##
  if (missing(median))
    stop("Mandatory argument 'median' missing")
  if (missing(lower))
    stop("Mandatory argument 'lower' missing")
  if (missing(upper))
    stop("Mandatory argument 'upper' missing")
  ##
  if (any (median <= lower))
    stop("Median must be larger than first quartile")
  if (any (median >= upper))
    stop("Median must be smaller than third quartile")
  if (any (lower >= upper))
    stop("First quartile must be smaller than third quartile")
  ##
  meta:::chklogical(logtransf)
  
  
  if (logtransf) {
    m <- log(median)
    q1 <- log(lower)
    b <- log(upper)
  }
  else{
    m <- median
    q1 <- lower
    b <- upper
    }
  ##
  ## Equation (14)
  ##
  TE <- (q1 + m + q3) / 3
  ##
  ## Equation (16)
  ##
  seTE <- (q3 - q1) / (2 * qnorm((0.75 * n - 0.125) / (n + 0.25)))
  
  
  res <- list(TE = TE, seTE = seTE, sm = sm,
              ##
              median = median, lower = lower, upper = upper,
              logtransf = logtransf)
  ##
  res
}
