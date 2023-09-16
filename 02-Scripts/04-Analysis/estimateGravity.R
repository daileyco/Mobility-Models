# Use a Metropolis random walk to estimate gravity model parameters

## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)
library(tibble)
library(foreach)
library(doParallel)
library(doRNG)


## helper functions
source("./02-Scripts/02-Helper-Functions/RMSE.R")
source("./02-Scripts/02-Helper-Functions/calculateGravity.R")
source("./02-Scripts/02-Helper-Functions/runMetropolisRandomWalk.R")


## more helper functions

### Objective function for Metropolis random walk
#### extracts log of commuter flow volume
#### estimates commuter flow volume with gravity model given parameters
#### calculates root mean square error between observed and estimate values

calculateRMSEforGravityModel <- function(observed.data, params){
  
  require(dplyr)
  
  
  obs <- observed.data %>% 
    select(log.Workers.in.Commuting.Flow) %>%
    unlist()
  
  
  preds <- calculateGravity(observed.data=observed.data, params=params) %>%
    select(gravity.flow.log) %>%
    unlist()
  
  
  RMSE(obs = obs,
       preds = preds) %>%
    return()
  
}



## Posterior function
### calculates exponential likelihood with pre-specified tau parameter and RMSE
#### tau parameter corresponds to exponential decay rate, needs tuning
##### showing profile may help someone smarter than me to choose tau
###### x values set to approximate scale for RMSE after 100 MC
###### curve(exp(-(tau/2)*x), from = 1, to = 20) 

mylikelihoodTimesPrior <- function(observed.data, params){
  tau <- 1
  thelikelihood <- exp(-(tau/2)*calculateRMSEforGravityModel(observed.data=observed.data, params=params));
  theprior <- 1;
  myreturn <- thelikelihood * theprior;
  myreturn
}




## set up params.proposal.tbl

params.proposal.tbl <- tibble(param.names = c(paste0("alpha", 0:1), 
                                              paste0("beta", 0:7),
                                              "dt"), 
                              proposal.function = c(rep("rnorm", 10), 
                                                    "runif"), 
                              proposal.function.args = c(as.pairlist(lapply(1:6, function(x){list(n=1, sd=0.5)})),
                                                         as.pairlist(lapply(1:4, function(x){list(n=1, sd=0.1)})), 
                                                         list(list(n=1, 
                                                                   min = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                                                                  probs = 0.05), 
                                                                   max = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                                                                  probs = 0.95)))), 
                              proposal.function.args.mc.depend = c(rep("mean", 10), 
                                                                   NA), 
                              initial.values = c(0,1,
                                                 0,1,1,-1,
                                                 0,0,0,0,
                                                 quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                                          probs = 0.5)))




## set up list of dataframes for each run

my.data.list <- list(od, 
                     od %>% filter(period == "2011-2015"), 
                     od %>% filter(period == "2016-2020"),
                     od %>% filter(census.region.origin == "Midwest"),
                     od %>% filter(period == "2011-2015" & census.region.origin == "Midwest"),
                     od %>% filter(period == "2016-2020" & census.region.origin == "Midwest"),
                     od %>% filter(census.region.origin == "Northeast"),
                     od %>% filter(period == "2011-2015" & census.region.origin == "Northeast"),
                     od %>% filter(period == "2016-2020" & census.region.origin == "Northeast"),
                     od %>% filter(census.region.origin == "South"),
                     od %>% filter(period == "2011-2015" & census.region.origin == "South"),
                     od %>% filter(period == "2016-2020" & census.region.origin == "South"),
                     od %>% filter(census.region.origin == "West"), 
                     od %>% filter(period == "2011-2015" & census.region.origin == "West"), 
                     od %>% filter(period == "2016-2020" & census.region.origin == "West"))



## set seed for reproducibility

# Sys.Date()
# # "2023-09-16"
# as.numeric(Sys.Date())
# # 19616
set.seed(19616)


## set up parallel computation
cl <- makeCluster(mc <- getOption("cl.cores", 15))

clusterExport(cl=cl, 
              varlist=c("calculateGravity", 
                        "RMSE", 
                        "calculateRMSEforGravityModel", 
                        "mylikelihoodTimesPrior", 
                        "runMetropolisRandomWalk", 
                        "params.proposal.tbl"))
clusterEvalQ(cl, 
             {
               
               library(dplyr)
               library(tibble)
               
             })




## run random walks in parallel

registerDoParallel(cl)



chains.list <- foreach(observed.data=iter(my.data.list)) %dorng% {
  runMetropolisRandomWalk(chain.length = 11000, 
                          observed.data = observed.data, 
                          objective.function = "calculateRMSEforGravityModel", 
                          posterior.function = "mylikelihoodTimesPrior", 
                          params.proposal.tbl = params.proposal.tbl)
}

stopCluster(cl)



## save
save(chains.list, file = "./01-Data/02-Analytic-Data/chains_list.rds")


## clean environment
rm(list=ls())
gc()







