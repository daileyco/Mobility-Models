# script to tune the distance threshold for the gravity model for different data subsets
## i.e., the distance threshold indicating "long" distances vs "short"



## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)
library(doParallel)
library(foreach)

## helper functions
source("./02-Scripts/02-Helper-Functions/calculateGravity.R")
source("./02-Scripts/02-Helper-Functions/calculateRMSE.R")


## create list with data split by timeperiod*region

### filter out the zero-distance flows
od <- od %>% 
  filter(distance.km!=0)

### and duplicate data for aggregates

od.big <- od %>% 
  bind_rows(mutate(., period = "2011-2020")) %>%
  bind_rows(mutate(., census.region.origin = "All US"))

### split by period and census region

od.list <- od.big %>%
  split(., f = ~period+census.region.origin)




## define objective function to optimize fit (minimize returned value)
### root mean square error 
### optim() needs first argument to be parameters
calculateRMSEforGravityModel <- function(params, observed.data){
  
  require(dplyr)
  
  ### extract the column with the observed flux, log-transformed(?)
  obs <- observed.data %>% 
    select(log.Workers.in.Commuting.Flow) %>%
    unlist()
  
  ### estimate flux with given parameters
  #### helper function takes same arguments
  preds <- calculateGravity(params=params, observed.data=observed.data) %>%
    select(gravity.flow.log) %>%
    unlist()
  
  ### compare values observed and estimated/predicted
  calculateRMSE(obs = obs,
                preds = preds) %>%
    return()
  
}



## run optimizer in parallel for each data subset
### set up parallel computation
cl <- makeCluster(mc <- getOption("cl.cores", 15))

clusterExport(cl=cl, 
              varlist=c("calculateGravity", 
                        "calculateRMSE", 
                        "calculateRMSEforGravityModel"))
clusterEvalQ(cl, 
             {
               
               library(dplyr)
               
             })


registerDoParallel(cl)

### optimizer
#### starting point set using median flow distance
##### test runs using different starting points as percentile=c(0.05,0.25,0.75,0.95) 
###### all yielded same result
### bounds given as 1st and 99th percentiles
#### two methods in optim() allow bounds, "L-BFGS-B" and "Brent"
##### Brent method uses optimize() which is specifically for one-dimensional optimization (just use this)
###### no starting point needed
###### but from playing around with the L-BFGS-B, 
###### it did once return better value, but varied with starting point, 
###### super accuracy not objective so will go with simpler method

dt.optim.list <- foreach(observed.data=iter(od.list)) %dopar% {

  # optim(par = quantile(od$log.distance.km,
  #                      probs = 0.5),
  #       fn = calculateRMSEforGravityModel,
  #       observed.data = observed.data,
  #       method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[6],
  #       lower = quantile(od$log.distance.km,
  #                        probs = 0.01),
  #       upper = quantile(od$log.distance.km,
  #                        probs = 0.99))[1:2] %>%
  #   unlist()
  
  optimize(f = calculateRMSEforGravityModel, 
           interval = quantile(od$log.distance.km, 
                               probs = c(0.01,0.99)), 
           observed.data = observed.data)

}


stopCluster(cl)


## compile results

distance.thresholds <- bind_cols(lapply(od.list, 
                                        function(x){
                                          data.frame(period = unique(x$period), 
                                                     region = unique(x$census.region.origin))
                                        }) %>% bind_rows(), 
                                 dt.optim.list %>% bind_rows())



## save 
save(distance.thresholds, file = "./01-Data/02-Analytic-Data/distance_thresholds.rds")


## clean environment
rm(list=ls())
gc()

