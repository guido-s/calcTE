calcTE.riskset <- function(riskset1, riskset2, keepdata = FALSE){
  ##
  ## R function written by Guido Schwarzer (sc@imbi.uni-freiburg.de)
  ## License: GPL (>= 2)
  ##
  ## Parmar et al. (1998), Stat Med
  ##
  dat <- data.frame(ifelse(riskset1$D == 0, 0.000001, riskset1$D),
                    riskset1$R,
                    ifelse(riskset2$D == 0, 0.000001, riskset2$D),
                    riskset2$R,
                    ##
                    ## Equation (17)
                    ##
                    log((ifelse(riskset1$D == 0, 0.000001, riskset1$D) / riskset1$R) /
                        (ifelse(riskset2$D == 0, 0.000001, riskset2$D) / riskset2$R)),
                    ##
                    ## Equation (18)
                    ##
                    1 / ifelse(riskset1$D == 0, 0.000001, riskset1$D) -
                    1 / riskset1$R + 
                    1 / ifelse(riskset2$D == 0, 0.000001, riskset2$D) -
                    1 / riskset2$R)
  ##
  names(dat) <- c("D.e", "R.e", "D.c", "R.c", "TE", "varTE")
  ##
  dat$TE.varTE <- dat$TE / dat$varTE
  dat$weight <- 1 / dat$varTE
  
  
  ##
  ## Equation (19)
  ##
  TE <- weighted.mean(dat$TE, dat$weight)
  ##
  ## Equation (20)
  ##
  varTE <- 1 / sum(dat$weight)
  ##
  seTE <- sqrt(varTE)
  sm <- "HR"
  
  
  res <- list(TE = TE, seTE = seTE, sm = sm)
  ##
  if (keepdata)
    res$data <- dat
  ##
  res
}
