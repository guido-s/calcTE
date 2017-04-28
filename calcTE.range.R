calcTE.range <- function(median, min, max, n,
                         sm = "",
                         logtransf = meta:::is.relative.effect(sm),
                         sample = "small",
                         mean.is.median = FALSE) {
  
  
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  
  
  ##
  ## Estimate mean and its standard error from median and range
  ## using method by Wan et al. (2014) which improves Hozo et al. (2005)
  ##
  ## References:
  ##
  ## Hozo et al. (2005), BMC Med Res Methodol 2005, 5:13)
  ## Wan et al. (2014), BMC Med Res Methodol 14 (1): 135
  ##
  
  
  ##
  ## Check arguments
  ##
  if (missing(median))
    stop("Mandatory argument 'median' missing")
  if (missing(min))
    stop("Mandatory argument 'min' missing")
  if (missing(max))
    stop("Mandatory argument 'max' missing")
  ##
  if (any (median <= min))
    stop("Median must be larger than minumum")
  if (any (median >= max))
    stop("Median must be smaller than maximum")
  if (any (min >= max))
    stop("Minimum must be smaller than maximum")
  ##
  meta:::chklogical(logtransf)
  ##
  sample <- meta:::setchar(sample, c("small", "medium", "large"))
  ##
  meta:::chklogical(mean.is.median)
  
  
  if (logtransf) {
    m <- log(median)
    a <- log(min)
    b <- log(max)
  }
  else{
    m <- median
    a <- min
    b <- max
    }
  ##
  ## Equation (2)
  ##
  if (mean.is.median)
    TE <- m
  else {
    if (missing(n))
      TE <- (a + 2 * m + b) / 4
    else
      TE <- (a + 2 * m + b) / 4 + (a - 2 * m + b) / (4 * n)
  }
  if (missing(n)) {
    ## Equation (6) in Wan et al. (2014)
    if (sample == "small")
      seTE <- sqrt(1 / 12 * ((b - a)^2 + 0.25 * (a - 2 * m + b)^2))
    else if (sample == "medium")
      seTE <- (b - a) / 4
    else if (sample == "large")
      seTE <- (b - a) / 6
  }
  else {
    ##  Equation (9) in Wan et al. (2014)
    seTE <- (b - a) / (2 * qnorm((n - 0.375) / (n + 0.25)))
  }
  
  
  res <- list(TE = TE, seTE = seTE, sm = sm,
              ##
              median = median, min = min, max = max,
              n = if (missing(n)) NA else n,
              logtransf = logtransf, sample = sample,
              mean.is.median = mean.is.median)
  ##
  res
}
