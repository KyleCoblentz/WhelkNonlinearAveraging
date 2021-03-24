################################################################################
#### function to run simulations to determine the effects of handling time 
#### variation on feeding rates in cages 
################################################################################

handlevarsim <- function(nsim, nind, prey1dens, prey1attack, prey2dens, prey2attack, prey1meanhandling, prey2meanhandling, sdeviationprey1, sdeviationprey2) {
  
  data_out <- matrix(nrow = nsim, ncol = 2)
  
  ### set parameters to simulate draws from a lognormal distribution
  
  m1desired <- prey1meanhandling
  
  sd1desired <- sdeviationprey1
  
  location1 <- log((m1desired^2) / sqrt(sd1desired^2 + m1desired^2))
  
  shape1 <- sqrt(log(1 + (sd1desired^2 / m1desired^2)))
  
  m2desired <- prey2meanhandling
  
  sd2desired <- sdeviationprey2
  
  location2 <- log((m2desired^2) / sqrt(sd2desired^2 + m2desired^2))
  
  shape2 <- sqrt(log(1 + (sd2desired^2 / m2desired^2)))
  
  for (i in 1:nsim) {
    
    ### draw samples of handling time from normal on log scale exponentiate to a lognormal distribution sample
    
    handlingsample1 <- rnorm(nind, mean = location1, sd = shape1)
    
    handlingsample1 <- exp(handlingsample1)
    
    handlingsample2 <- rnorm(nind, mean = location2, sd = shape2)
  
    handlingsample2 <- exp(handlingsample2)
    
    ### calculate mean handling times on prey
    
    meanhandling1 <- mean(handlingsample1)
    
    meanhandling2 <- mean(handlingsample2)
    
    ### calculate mean feeding rates on both prey using the mean handling times
    
    meanhandlingfr1 <- prey1attack*prey1dens/(1 + prey1attack*meanhandling1*prey1dens + prey2attack*meanhandling2*prey2dens)
    
    meanhandlingfr2 <- prey2attack*prey2dens/(1 + prey1attack*meanhandling1*prey1dens + prey2attack*meanhandling2*prey2dens)
    
    ### calculate mean feeding rates on both prey with variation in handling times
    
    varfeedingrate1 <- prey1attack*prey1dens/(1 + prey1attack*handlingsample1*prey1dens + prey2attack*handlingsample2*prey2dens)
    
    meanvarfeedingrate1 <- mean(varfeedingrate1)
    
    varfeedingrate2 <- prey2attack*prey2dens/(1 + prey1attack*handlingsample1*prey1dens + prey2attack*handlingsample2*prey2dens)
    
    meanvarfeedingrate2 <- mean(varfeedingrate2)
    
    ### save differences in feeding rates
    
    data_out[i, 1] <- meanvarfeedingrate1 - meanhandlingfr1
    
    data_out[i, 2] <- meanvarfeedingrate2 - meanhandlingfr2
    
    }
  
  out <- data.frame(Prey1FRDiffs = data_out[,1], 
                    Prey2FRDiffs = data_out[,2])
  
  return(out)
  
}

