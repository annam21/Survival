  # 3-stage ratio estimator 
  # Anna Moeller
  # 8/4/2017

  # Page 218 Skalski et al. 
  # Starting population at occasion 1
  Na <- Nj <- NULL
  Na[1] <- 100
  Nj[1] <- 99
  Sa <- .98
  Sj <- .75
  
  # Real numbers
  Na[2] <- Na[1]*Sa
  Nj[2] <- Nj[1]*Sj
  # Third observation is at the same time as the second
  # Third observation is also supposed to be deaths. 
  Na[3] <- Na[1]*(1 - Sa)
  Nj[3] <- Nj[1]*(1 - Sj)
 
  # Pretend we observe the ratio perfectly
  R1 <- Nj[1]/Na[1]
  R2 <- Nj[2]/Na[2]
  R3 <- Nj[3]/Na[3]

  # MLEs of survival
  Sa.hat <- (R3 - R1)/(R3 - R2)
  Sj.hat <- (R2/R1) * ((R3 - R1)/(R3 - R2))
  
  # OR, if we observed live animals on occasion 3:
  Na[4] <- Na[1]*(Sa)
  Nj[4] <- Nj[1]*(Sj)
  
  R4 <- (Nj[1] - Nj[1]*Sj)/(Na[1] - Na[1]*Sa)
    
  Sa.hat <- (R4 - R1)/(R4 - R2)
  Sj.hat <- (R2/R1) * ((R4 - R1)/(R4 - R2))
  
  
###################################
  # With Josh Nowak
  
  Na <- Nj <- NULL
  Sa <- 0.85
  Sj <- 0.6
  
  Na[1] <- 100
  Nj[1] <- 50
  
  A <- Nj[1]/Na[1]
  
  Na[2] <- Na[1] * Sa
  Nj[2] <- Nj[1] * Sj    
  
  B <- Nj[2]/Na[2]
  
  Na[3] <- Na[1] * (1 - Sa)
  Nj[3] <- Nj[1] * (1 - Sj)
  
  C <- Nj[3]/Na[3]
  D <- (Nj[1] - Nj[1]*Sj)/(Na[1] - Na[1]*Sa)
  
  Sa.h <- (C - A)/(C - B)
  Sj.h <- ((C - A)/(C - B)) * (B/A)
  
  Sa.h <- (D - A)/(D - B)
  Sj.h <- ((D - A)/(D - B)) * (B/A)