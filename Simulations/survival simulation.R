  # Anna Moeller
  # Survival simulation 
  # 7/24/2017

  # Source
  source("GitHub/packages.R")
  library(msm)
  source("GitHub/Time2Event_Abundance/Clean code and summaries/Clean IS_STE_TTE code.R")

  # Study area and camera specs
  A <- 50 * 1000^2 # 50 km2
  r <- 50 # camera maximum distance in meters
  theta <- 42 # camera viewshed in degrees
  a <- pi * r^2 * theta/360 # Area of a single camera in square meters
  P <- A/a # Number of possible cameras (grid cells) in study area
  ncam <- 1000
  nocc <- 1000
  
  # Estimate juvenile population size. 
  nper <- 3 # Number of periods to estimate survival in
  njuv <- c(100, rep(NA, nper-1) )
  sjuv <- .8
  for(i in 2:nper){
    njuv[i] <- njuv[i - 1] * sjuv
  }
  lambda <- njuv/P
  
  # Estimate population size for juveniles, using STE
  estN <- rep(NA, length(njuv) )
  surv <- rep(NA, length(njuv) )
  
  for(i in 1:length(njuv)) {
    # Simulate data
    toevent <- ceiling(rexp(nocc, rate = lambda[i]))
    toevent[toevent > ncam] <- NA
    
    # Resimulate if there are no observations
    while(all(is.na(toevent))) {
      toevent <- ceiling(rexp(nocc, rate = lambda[i]))
      toevent[toevent > ncam] <- NA
    }
    
### What to do if it rounds down to 0?
    
    # Estimate abundance
    dat.ste <- list(toevent = matrix(toevent, ncol = nocc),
                    censor = ncam)
    out <- estN.fn(data = dat.ste, P = P)
    
    estN[i] <- out$estN
    
    if(i == 1){
      surv[i] <- NA
    } else {
      surv[i] <- estN[i]/estN[i-1]
    }
  }
  
  njuv
  estN
  
  sjuv
  surv
  # Next...
  # How to propagate uncertainty? 
  # Make it so estN always has to decrease? 
  # Simulation: cows and calves move together
  # Observation error? 
  # Stochastic population?
  
##########################################
  # # Simulate a population
  # ncow <- 100
  # nneo <- 95
  # sneo <- .8
  # 
  # surv <- matrix(c(sneo, 0, 0, 1), ncol = 2)
  # 
  # # Future population
  # npop1 <- matrix(c(nneo, ncow), ncol = 1)
  # npop2 <- surv %*% npop1
  # npop3 <- surv %*% npop2
  # 
  # lambda1 <- npop1/P
  # lambda2 <- npop2/P