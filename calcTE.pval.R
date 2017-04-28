calcTE.pval <- function(estimate, p.value,
                        sm = ifelse(!missing(event.e), "HR", ""),
                        logtransf = meta:::is.relative.effect(sm),
                        event.e, n.e, event.c, n.c, above.null = TRUE) {
  
  
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  
  
  ##
  ## Check arguments
  ##
  if (missing(p.value))
    stop("Mandatory argument 'p.value' missing")
  if (any(p.value <= 0) | any(p.value >= 1))
    stop("no valid value for p-value")
  ##
  meta:::chklogical(logtransf)
  
  
  ##
  ## Parmar et al. (1998), Stat Med
  ##
  if (missing(event.e) | missing(event.c)) {
    ##
    ## Check argument
    ##
    if (missing(estimate))
      stop("Argument 'estimate' missing")
    ##
    ## Section 4.1
    ##
    if (logtransf)
      TE <- log(estimate)
    else
      TE <- estimate
    ##
    ## Equation (7)
    ##
    varTE <- (TE / qnorm(p.value / 2, lower.tail = FALSE))^2
    seTE <- sqrt(varTE)
    ##
    res <- list(TE = TE, seTE = seTE, sm = sm,
                ##
                estimate = estimate, p.value = p.value,
                logtransf = logtransf)
  }
  else {
    ##
    ## Check argument
    ##
    if (missing(event.e))
      stop("Argument 'event.e' missing")
    if (missing(event.c))
      stop("Argument 'event.c' missing")
    ##
    meta::chknumeric(event.e, 0)
    meta::chknumeric(event.c, 0)
    meta::chklogical(above.null)
    ##
    ## Section 4.3 Indirect log hazard ratio and variance estimation
    ##
    ## Estimation of inverse of Mantel-Haenszel variance (method 1)
    ##
    if (any(c(missing(n.e), missing(n.c))))
      V1 <- (event.e + event.c) / 4                         # equation (8)
    else {
      meta::chknumeric(n.e, 0, zero = TRUE)
      meta::chknumeric(n.c, 0, zero = TRUE)
      ##
      V1 <- (event.e + event.c) * n.e * n.c / (n.e + n.c)^2 # equation (11)
    }
    ##
    ## Estimation of inverse of Mantel-Haenszel variance (method 2)
    ##
    V2 <- (event.e * event.c) / (event.e + event.c)         # equation (12)
    ##
    ## Combine results for method 1 and 2
    ##
    V <- c(V1, V2)
    p.value <- c(p.value, p.value)
    above.null <- c(above.null, above.null)
    method <- rep(c("method1", "method2"), each = length(V1))
    ##
    ## Equations (10), (13), and (14)
    ##
    OE <- sqrt(V) * qnorm(p.value / 2, lower.tail = FALSE)
    ##
    OE <- ifelse(above.null, OE, -OE)
    ##
    ## Equations
    ##
    res <- list(TE = OE / V,        # equation (4)
                seTE = sqrt(1 / V), # equation (5)
                sm = sm,
                method = method,
                ##
                OE = OE, V = V,
                ##
                event.e = event.e)
    if (!missing(n.e)) res$n.e <- n.e
    res$event.c <- event.c
    if (!missing(n.c)) res$n.c <- n.c
    res$p.value <- p.value
    res$above.null <- above.null
  }
  
  
  res
}
