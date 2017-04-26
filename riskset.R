riskset <- function(time, S, R0, Fmin, Fmax){
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  ## Parmar et al. (1998), Stat Med
  ##
  ## Section 5
  ## Estimating the log hazard ratio and its variance from survival curves
  ##
  ## Arguments:
  ##
  ## time <=> non-overlapping time points (e.g. c(0, 3, 6, 9))
  ## S    <=> survival probability at t
  ## R0   <=> patients at risk at t = 0
  ## Fmin <=> minimal follow-up
  ## Fmax <=> maximal follow-up
  ##
  Cens <- function(R0, timeS, timeE, Fmin, Fmax){
    ##
    ## Estimate the effective number of censored observations
    ## Equation (26)
    ##
    ## timeS <=> start of time interval
    ## timeE <=>  end  of time interval
    ##
    sel1 <- timeS < Fmin & Fmin <= timeE & timeE <= Fmax
    sel2 <- timeS < Fmin & timeE > Fmax
    sel3 <- timeS > Fmin & timeE > Fmax
    sel4 <- timeS < Fmin & timeE < Fmin
    ##
    if (sel1)
      timeS <- Fmin
    ##
    if (sel2) {
      timeS <- Fmin
      timeE <- Fmax
    }
    ##
    if (sel3)
      timeE <- Fmax
    ##
    C <- (timeS >= Fmin) * R0 * 0.5 * (timeE - timeS) / (Fmax - timeS)
    ##
    if (sel4)
      C <- 0
    ##
    C
  }
  ##
  timeS <- time[1:(length(time) - 1)]
  timeE <- time[2:length(time)]
  ##
  R <- c(R0, rep(NA, length(timeS) - 1))
  D <- c(0,  rep(NA, length(timeS) - 1))
  C <- c(0,  rep(NA, length(timeS) - 1))
  ##
  for (i in 1:length(timeS)) {
    if (i == 1) {
      C[i] <- Cens(R0, timeS[i], timeE[i], Fmin, Fmax)
      R[i] <- R0 - C[i]
    }
    else {
      C[i] <- Cens(R[i - 1] - D[i - 1], timeS[i], timeE[i], Fmin, Fmax)
      ##
      ## Equation (15)
      ##
      R[i] <- R[i - 1] - D[i - 1] - C[i]
    }
    ##
    ## Equation (16)
    ##
    D[i] <- R[i] * (S[i] - S[i + 1]) / S[i]
  }
  ##
  res <- data.frame(timeS, timeE, S[1:length(timeS)], R + C, C, R, D)
  names(res) <- c("timeS", "timeE", "S", "R+C", "C", "R", "D")
  ##
  res
}
