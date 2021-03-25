### "Evaluating and integrating spatial capture-recapture models with data of variable individual identifiability", 
### Ruprecht, et al., Ecological Applications
#
### Program to run the hybrid gSMR+SCR+GPS model for coyotes in NIMBLE using parallel processing.
#
### Steps:
#  1) Embed data, constants, monitors, inits, model code, and call to fit NIMBLE model in a user defined R function
#  2) Make clusters for parallel processing (1 per chain), use parLapply to execute function for number of chains desired
#  3) Combine posteriors from each chain and calculate Rhat using coda package

# sf_camera_deploy = x.cam
# resight_unmarked = n
# resight_marked = y.resight
# sf_trapgrid_centroid = x.marking
# marking_trapname = y.marking
# sample_sf_cat_locations = telemetry.array
# marking_trapname = y.marking


# 1) write function to fit model with 1 chain:

runNimbleParallel <- function(seed){
  
  load("data/SECR_data.RData")
  
  data<- list(n=n, # unmarked animals seen in camera: [J.resight, K.resight]
              y.resight=y.resight, # resighting data frame: [M, J.resight, K.resight]
              y.marking=y.marking, # marking data frame: [M, J.marking]
              # y.genetics=y.genetics, # genetic recapture data frame: [M, J.genetics]
              X.marking=X.marking, # the coordinates of each animals trap
              # X.genetics=X.genetics, # coordinates of the center point of each grid?
              X.cam=X.cam, # coordinates of each camera
              telemetry.array=telemetry.array) # 100 randomly sampled gps points for each animal (from same time as camera period?)
  
  constants<-list(M=M, # augmented population size
                  xlim=xlim, # min - max x values
                  ylim=ylim, # min - max y values
                  J.resight=J.resight, # 94 unbaited remote cameras
                  J.marking=J.marking, # number of traps used
                  # J.genetics=J.genetics, # number of 1 x 1 km grid cells? **
                  K.resight=K.resight, # 10 14-day occasions beginning April 15th
                  # trapOp.cam=trapOp.cam, # proportion of time each camera was operational during an occasion [J.resight, K.resight]
                  # trapOp.genetics=trapOp.genetics, # if the genetic sampling didn't work or no scat was found in that grid? **
                  # surveyDist=surveyDist, # dog scat rate survey distance
                  marked.status=marked.status, # whether the animal is real or imagined augmented data, also zero for real prior to mark
                  nMarked=nMarked, # 9 marked coyotes 
                  nTelemLocs=nTelemLocs) # 100 randomly selected locations per animal
  
  # monitors<- c("N","lam0.resight","lam0.marking","sigma","D","psi","alpha0","alpha1")
  monitors<- c("N","lam0.resight","lam0.marking","sigma","D","psi","A")
  
  inits<- list(z=rep(1,M),
               s=cbind(runif(M,xlim[1],xlim[2]),runif(M,ylim[1],ylim[2])),
               lam0.resight=runif(1,0.004,0.008),
               lam0.marking=runif(1,0.001,0.004),
               sigma=runif(1,3000,4000),
               alpha0=runif(1,-5,-4),
               alpha1=runif(1,0.2,0.5))
  
  library(nimble)
  
  coyote_hybrid_code<-nimbleCode({
    
    psi ~ dbeta(1,1)
    # alpha0 ~ dnorm(0, 0.01)
    # alpha1 ~ dnorm(0, 0.01)
    sigma ~ dunif(0,20000)
    sigma2 <- sigma^2
    lam0.resight ~ dunif(0,5)
    lam0.marking ~ dunif(0,5)
    
    for(i in 1:M) {
      z[i] ~ dbern(psi) # latent state
      s[i,1] ~ dunif(xlim[1], xlim[2]) # prior from a uniform distribution generated from min-max
      s[i,2] ~ dunif(ylim[1], ylim[2]) # prior from a uniform distribution generated from min-max
      
      #genetic scr - DOES NOT EXIST FOR IDFG - being used to inform sigma2 - needs to be genetics of marked individuals
      # for(j in 1:J.genetics){
      #   d2.genetics[i,j] <- (s[i,1]-X.genetics[j,1])^2 + (s[i,2]-X.genetics[j,2])^2
      #   logit(p0.genetics[i,j])<- alpha0 + alpha1*surveyDist[j]
      #   p.genetics[i,j] <- p0.genetics[i,j]*exp(-d2.genetics[i,j]/(2*sigma2))*z[i]*trapOp.genetics[j]
      #   y.genetics[i,j]~dbin(p.genetics[i,j],1) 
      # } 
      
      #marking process - when the cats are collared?
      for(j in 1:J.marking){
        d2.marking[i,j] <- (s[i,1]-X.marking[j,1])^2 + (s[i,2]-X.marking[j,2])^2 # residual error of homerange center from marking area center?
        lam.marking[i,j] <- lam0.marking*exp(-d2.marking[i,j]/(2*sigma2))*z[i] # Each model considered here is a variation of a conventional SCR model (Royle and Young 2008) in which the detection probability decays as a function of the Euclidean distance between the detector j and the activity center s for individual i, di,j, to yield where lam0.marking is the baseline detection probability when the detector is located exactly at the activity center of the home range, and is a spatial scale parameter related to home range size that determines the rate at which detection probability declines with distance from the activity center.
        y.marking[i,j] ~ dpois(lam.marking[i,j])
      }
      
      #resighting process - when the cats are resighted by the cameras?
      for(j in 1:J.resight) {
        d2.resight[i,j] <- (s[i,1]-X.cam[j,1])^2 + (s[i,2]-X.cam[j,2])^2
        lam.resight[i,j] <- lam0.resight*exp(-d2.resight[i,j]/(2*sigma2))*z[i] # Each model considered here is a variation of a conventional SCR model (Royle and Young 2008) in which the detection probability decays as a function of the Euclidean distance between the detector j and the activity center s for individual i, di,j, to yield where lam0.resight is the baseline detection probability when the detector is located exactly at the activity center of the home range, and is a spatial scale parameter related to home range size that determines the rate at which detection probability declines with distance from the activity center.
      }
    }
    
    #marked animals
    for(i in 1:nMarked) {
      for(j in 1:J.resight) {
        for(k in 1:K.resight){
          # y.resight[i,j,k] ~ dpois(lam.resight[i,j]*marked.status[i,k]*trapOp.cam[j,k])
          y.resight[i,j,k] ~ dpois(lam.resight[i,j]*marked.status[i,k]) # lam.resight is estimated from the marked animals we learned about homerange
        }
      }
    }
    
    #unmarked animals
    for(j in 1:J.resight) {
      for(k in 1:K.resight){
        Lam[j,k] <- inprod(lam.resight[1:M,j],(1-marked.status[1:M,k])) # don't get this
        # n[j,k] ~ dpois(Lam[j,k]*trapOp.cam[j,k])
        n[j,k] ~ dpois(Lam[j,k])
      }
    }
    
    ## telemetry data - use telemetry data, assume normally distributed, use to estimate mean of homerange i guess
    for(i in 1:nMarked) {
      for(l in 1:nTelemLocs) {
        telemetry.array[i,l,1] ~ dnorm(s[i,1], 1/sigma2)
        telemetry.array[i,l,2] ~ dnorm(s[i,2], 1/sigma2)
      }
    }
    
    N <- sum(z[1:M])
    A <- (xlim[2]-xlim[1])*(ylim[2]-ylim[1])/100000000 # area of state-space in 100 km^2
    D <- N/A # density per 100 km^2
    
  })
  
  # short run for example purposes:
  coyote_hybrid_nimble<-nimbleMCMC(code=coyote_hybrid_code,constants=constants,data=data,monitors=monitors,inits=inits,
                          niter=1000,nburnin=500,nchains=1,samplesAsCodaMCMC=T,summary=F,setSeed = seed)
  # full run as in manuscript:
  # coyote_hybrid_nimble<-nimbleMCMC(code=coyote_hybrid_code,constants=constants,data=data,monitors=monitors,inits=inits,
  #                                 niter=50000,nburnin=15000,nchains=1,samplesAsCodaMCMC=T,summary=F,setSeed = seed)
  
  return(coyote_hybrid_nimble)
}

# 2) Make clusters and execute function using parLapply:

cluster <- parallel::makeCluster(3) # the 3 here means use 3 clusters, one for each chain
coyote_nimble_parallel <- parallel::parLapply(cl = cluster, X = 1:3, fun = runNimbleParallel) # the X = 1:3 here means execute the function 3 times, each with a different seed              
parallel::stopCluster(cluster)

# 3) use coda package to summarize the 3 chains and calculate Rhat:

library(coda)
coyote_nimble_results <- as.mcmc.list(coyote_nimble_parallel)
options(scipen = 999)
summary(coyote_nimble_results)
gelman.diag(coyote_nimble_results, multivariate = F)
