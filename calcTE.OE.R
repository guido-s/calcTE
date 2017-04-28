calcTE.OE <- function(obs.e, exp.e, obs.c, exp.c, V, sm = "HR") {
  
  
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  
  
  ##
  ## Parmar et al. (1998), Stat Med
  ##
  ## Section 3 Estimating the log hazard ratio and its variance directly
  ##
  if (missing(V)) {
    ##
    ## Check arguments
    ##
    if (missing(obs.e))
      stop("Mandatory argument 'obs.e'")
    if (missing(exp.e))
      stop("Mandatory argument 'exp.e'")
    if (missing(obs.c))
      stop("Mandatory argument 'obs.c' if argument 'V' is missing")
    if (missing(exp.c))
      stop("Mandatory argument 'exp.c' if argument 'V' is missing")
    ##
    meta::chknumeric(obs.e, 0)
    meta::chknumeric(exp.e, 0)
    meta::chknumeric(obs.c, 0)
    meta::chknumeric(exp.c, 0)
    ##
    ## Equation (3)
    ##
    TE <- log((obs.e / exp.e) / (obs.c / exp.c))
    ##
    ## Equation (5)
    ##
    varTE <- 1 / exp.e + 1 / exp.c
    seTE <- sqrt(varTE)
    ##
    res <- list(TE = TE, seTE = seTE, sm = sm,
                method = "formal definition",
                ##
                obs.e = obs.e, exp.e = exp.e,
                obs.c = obs.c, exp.c = exp.c)
  }
  else {
    ##
    ## Check arguments
    ##
    if (missing(obs.e))
      stop("Mandatory argument 'obs.e'")
    if (missing(exp.e))
      stop("Mandatory argument 'exp.e'")
    ##
    meta::chknumeric(obs.e, 0)
    meta::chknumeric(exp.e, 0)
    meta::chknumeric(V, 0, zero = TRUE)
    ##
    ## Equation (4)
    ##
    TE <- (obs.e - exp.e) / V
    ##
    ## Equation (6)
    ##
    varTE <- 1 / V
    seTE <- sqrt(varTE)
    ##
    res <- list(TE = TE, seTE = seTE, sm = sm,
                method = "MH variance",
                ##
                obs.e = obs.e, exp.e = exp.e,
                V = V)
  }
  
  
  res
}
