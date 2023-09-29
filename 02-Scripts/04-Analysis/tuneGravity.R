# script to tune the distance threshold for the gravity model for different data subsets
## i.e., the distance threshold indicating "long" distances vs "short"



## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)
library(tidyr)
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
calculateRMSEforGravityModel <- function(params, observed.data, model.extent, objective.scale){
  
  require(dplyr)
  
  if(objective.scale=="identity"){
    
    ### extract the column with the observed flux, log-transformed(?)
    obs <- observed.data %>% 
      select(`Workers in Commuting Flow`) %>%
      unlist()
    
    ### estimate flux with given parameters
    #### helper function takes same arguments
    preds <- calculateGravity(params=params, observed.data=observed.data, model.extent=model.extent) %>%
      select(gravity.flow) %>%
      unlist()
  }
  
  if(objective.scale=="log"){
    
    ### extract the column with the observed flux, log-transformed(?)
    obs <- observed.data %>% 
      select(log.Workers.in.Commuting.Flow) %>%
      unlist()
    
    ### estimate flux with given parameters
    #### helper function takes same arguments
    preds <- calculateGravity(params=params, observed.data=observed.data, model.extent=model.extent) %>%
      select(gravity.flow.log) %>%
      unlist()
  }
  
  
  
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

distance.thresholds <- foreach(observed.data=iter(od.list), 
                         .combine = bind_rows) %dopar% {

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
  
  
  model.settings <- expand.grid(period = unique(observed.data$period), 
                                region = unique(observed.data$census.region.origin), 
                                model.extent = c("base*distance_threshold", "base*distance_threshold*population_categories"), 
                                objective.scale = c("identity", "log")) %>%
    mutate(across(everything(), ~as.character(.x)))
  
  dt.tuning <- lapply(1:nrow(model.settings), 
                      function(index){
                        optimize(f = calculateRMSEforGravityModel, 
                                 interval = quantile(od$log.distance.km, 
                                                     probs = c(0.01,0.99)), 
                                 observed.data = observed.data, 
                                 model.extent = model.settings$model.extent[index], 
                                 objective.scale = model.settings$objective.scale[index])
                      }) %>%
    bind_rows()
  
  
  bind_cols(model.settings, 
            dt.tuning) %>%
    
    group_by(model.extent, objective.scale) %>%
    mutate(complementary.objective = calculateRMSEforGravityModel(params = .data[["minimum"]], 
                                                                  observed.data = observed.data, 
                                                                  model.extent = .data[["model.extent"]], 
                                                                  objective.scale = ifelse(.data[["objective.scale"]]=="identity", 
                                                                                           "log", 
                                                                                           "identity"))) %>%
    ungroup() %>% 
    
    return()
  

}


stopCluster(cl)


## clean
distance.thresholds <- distance.thresholds %>% 
  pivot_longer(c(objective, complementary.objective), 
               names_to = "obj", 
               values_to = "RMSE") %>% 
  mutate(obj = ifelse(obj=="objective", 
                      objective.scale, 
                      ifelse(objective.scale=="log", 
                             "identity", 
                             "log"))) %>% 
  pivot_wider(names_from = obj, 
              values_from = RMSE, 
              names_prefix = "RMSE_") %>% 
  mutate(period = factor(period, 
                         levels = c("2011-2020", "2011-2015", "2016-2020"), 
                         ordered = TRUE)) %>% 
  arrange(period, region, model.extent, objective.scale) %>% 
  mutate(across(where(is.factor), 
                ~as.character(.x))) %>% 
  rename(distance_threshold = minimum)



## save 
save(distance.thresholds, file = "./01-Data/02-Analytic-Data/distance_thresholds.rds")


## clean environment
rm(list=ls())
gc()

