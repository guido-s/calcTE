calcTE.pval <- function(estimate, p.value,
                        sm = ifelse(!missing(event.e, "HR", "")),
                        logtransf = ifelse(sm %in% c("OR", "RR", "HR"),
                                           TRUE, FALSE),
                        event.e, n.e, event.c, n.c, pos.effect = TRUE) {
  
  
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  if (any(p.value <= 0) | any(p.value >= 1))
    stop("no valid value for p-value")
  
  
  ##
  ## Parmar et al. (1998), Stat Med
  ##
  if (missing(event.e) | missing(event.c)) {
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
    ## Section 4.3 Indirect log hazard ratio and variance estimation
    ##
    ## Estimation of inverse of Mantel-Haenszel variance (method 1)
    ##
    if (any(c(missing(n.e), missing(n.c))))
      V1 <- (event.e + event.c) / 4                         # equation (8)
    else
      V1 <- (event.e + event.c) * n.e * n.c / (n.e + n.c)^2 # equation (11)
    ##
    ## Estimation of inverse of Mantel-Haenszel variance (method 2)
    ##
    V2 <- (event.e * event.c) / (event.e + event.c)         # equation (12)
    ##
    ## Combine results for method 1 and 2
    ##
    V <- c(V1, V2)
    p.value <- c(p.value, p.value)
    pos.effect <- c(pos.effect)
    method <- rep(c("method1", "method2"), each = length(V1))
    ##
    ## Equations (10), (13), and (14)
    ##
    OE <- sqrt(V) * qnorm(p.value / 2, lower.tail = FALSE)
    ##
    OE <- ifelse(pos.effect, OE, -OE)
    ##
    ## Equations
    ##
    res <- list(TE = OE / V,        # equation (4)
                seTE = sqrt(1 / V), # equation (5)
                method = method,
                sm = sm,
                ##
                OE = OE, V = V,
                ##
                event.e = event.e, n.e = n.e,
                event.c = event.c, n.c = n.c,
                p.value = p.value,
                pos.effect = pos.effect)
  }
  
  
  res
}
