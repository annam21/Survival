  # Survival from cameras simulation
  # Anna Moeller
  # 8/17/2017

  # Load packages and models
  source("GitHub/packages.R")
  
  # model "GitHub/Time2Event_Abundance/Clean code and summaries/Bayesian exponential likelihood.txt"
  
  
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
  
  # Simulate data
  toevent <- matrix(NA, nrow = nper, ncol = nocc)
  for(i in 1:nper) {
    # Simulate data
    toevent[i, ] <- ceiling(rexp(nocc, rate = lambda[i]))
    toevent[toevent > ncam] <- NA
### What to do if it rounds down to 0?
    # Resimulate if there are no observations
    while(all(is.na(toevent[i, ]))) {
      toevent[i, ] <- ceiling(rexp(nocc, rate = lambda[i]))
      toevent[toevent > ncam] <- NA
    }
  }
  
  # Estimate abundance with Bayesian model
  # Data
  dat.ste <- list(toevent = toevent,
                  censor = ncam + 1, # Need + 1 for JAGS
                  P = P,
                  rows = nper, 
                  cols = nocc)
  dat.ste$is.censored <- matrix(rep(0, length(dat.ste$toevent)*nper), nrow = nper)
  dat.ste$is.censored[is.na(dat.ste$toevent)] <- 1   
  
  # Initial values for JAGS
  inits <- function(){
    list(beta0 = runif(1, -3, 3),
         S = runif(1, 0, 1)
         )
  }
    
  # Parameters to monitor in JAGS
  params <- c("beta0", "S", "lambda", "N")
  
  # MCMC specifications
  ni <- 10000
  nt <- 1
  nb <- 5000
  nc <- 3
  
  # Call JAGS
  res <- jags( dat.ste, 
               inits,
               params,
               "GitHub/Time2Event_Abundance/Clean code and summaries/Bayesian survival.txt",
               n.chains=nc, 
               n.iter=ni, 
               n.burnin=nb,
               n.thin=nt
  )
  
  njuv
  res$BUGSoutput$mean$N
  c(quantile(res$BUGSoutput$sims.list$N[, 1], probs = c(.025, .975) ),
    quantile(res$BUGSoutput$sims.list$N[, 2], probs = c(.025, .975) ),
    quantile(res$BUGSoutput$sims.list$N[, 3], probs = c(.025, .975) ) )
  
  sjuv
  res$BUGSoutput$mean$S
  quantile(res$BUGSoutput$sims.list$S, probs = c(.025, .975) )
  