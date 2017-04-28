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
  
  
  ##
  ## Wan et al. (2014), BMC Med Res Methodol 14 (1): 135
  ## based on Hozo et al. (2005), BMC Med Res Methodol 2005, 5:13
  ##
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
  ##
  ## Equation (5) and formula for S^2
  ## Small sample formula from Hozo et al. (2005), equation (16)
  ##
  if (missing(n)) {
    if (sample == "small")
      seTE <- sqrt(1 / 12 * (0.25 * (a - 2 * m + b)^2 + (b - a)^2))
    else if (sample == "medium")
      seTE <- (b - a) / 4
    else
      seTE <- (b - a) / 6
  }
  else
    seTE <- sqrt(1 / (n - 1) *
                 (a^2 + m^2 + b^2 +
                  0.50 * (n - 3) *
                  0.25 * ((a + m)^2 + (m + b)^2) -
                  n * (a + 2 * m + b)^2 / 16))
  
  
  res <- list(TE = TE, seTE = seTE, sm = sm,
              ##
              median = median, min = min, max = max,
              n = if (missing(n)) NA else n,
              logtransf = logtransf, sample = sample,
              mean.is.median = mean.is.median)
  ##
  res
}
