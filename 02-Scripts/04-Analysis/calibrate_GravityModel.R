# script to estimate the "pseudo parameters" for the gravity model
## i.e., the distance threshold indicating "long" distances and
## the population threshold indicating "large" populations



## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)


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


# ## run descriptors
# data.subsets <- expand.grid(period = c("2011-2020", "2011-2015", "2016-2020"), 
#                             region = c("All US", "Midwest", "Northeast", "South", "West"))




## define objective function to optimize fit (minimize returned value)
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




dt.optim <- lapply(my.data.list, 
                   function(observed.data){
                     
                     optim(par = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                          probs = 0.5), 
                           fn = calculateRMSEforGravityModel, 
                           observed.data = observed.data,
                           method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[6], 
                           lower = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                            probs = 0.01), 
                           upper = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                            probs = 0.99))[1:2] %>% unlist()
                     
                   }) %>% bind_rows(.id = "data")



test <- optim(par = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                             probs = 0.5), 
              fn = calculateRMSEforGravityModel, 
              observed.data = od,
              method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[6], 
              lower = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                               probs = 0.01), 
              upper = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                               probs = 0.99))










