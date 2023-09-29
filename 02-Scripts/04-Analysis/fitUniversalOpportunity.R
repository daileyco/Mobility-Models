# script to fit the universal opportunity model variations for different data subsets


## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/predictions_totalcommuters.rds")
load("./01-Data/02-Analytic-Data/uo_model_params.rds")

## packages
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)


## helper functions
source("./02-Scripts/02-Helper-Functions/calculateUniversalOpportunity.R")
source("./02-Scripts/02-Helper-Functions/calculateRMSE.R")



## clean data

### filter out the zero-distance flows
od <- od %>% 
  filter(distance.km!=0)


## add predictions for total commuters
od <- od %>%
  left_join(., 
            preds.totalcommuters, 
            by = c("period", "County FIPS Residence", "County Residence", "State FIPS Residence", "State Residence")) %>%
  mutate(Total.Commuters_pred = exp(log.Total.Commuters_pred))






## create list with data split by timeperiod*region
### duplicate data for aggregates

od.big <- od %>% 
  bind_rows(mutate(., period = "2011-2020")) %>%
  bind_rows(mutate(., census.region.origin = "All US"))

### split by period and census region

od.list <- od.big %>%
  split(., f = ~period+census.region.origin)


## set up uo params for different model special cases
### special cases
#### alpha = 0 and beta = 0
##### opportunity only model
#### alpha = 1 and beta = 0
##### opportunity priority selection (OPS) model
#### alpha = 0 and beta = 1
##### radiation model

uo.params.list <- uo.params %>%
  select(period, region, svar, tvar, alpha, beta, objective.scale) %>%
  mutate(model = "UO") %>%
  bind_rows(., 
            mutate(., model = "OO", alpha = 0, beta = 0), 
            mutate(., model = "OPS", alpha = 1, beta = 0), 
            mutate(., model = "Radiation", alpha = 0, beta = 1)) %>%
  mutate(across(where(is.factor), ~as.character(.x))) %>%
  split(., f = ~period+region)





## parallel for each data subset

### set up parallel computation
cl <- makeCluster(mc <- getOption("cl.cores", 15))

clusterExport(cl=cl, 
              varlist=c("calculateUniversalOpportunity", 
                        "calculateRMSE"))
clusterEvalQ(cl, 
             {
               
               library(dplyr)
               
             })


registerDoParallel(cl)



### generate estimates

fit.list <- foreach(observed.data=iter(od.list), 
                    paramset=iter(uo.params.list), 
                    .combine = bind_rows) %dopar% {
                      
                      
                      obs <- observed.data$`Workers in Commuting Flow`
                      obs.log <- observed.data$log.Workers.in.Commuting.Flow
                      
                      lapply(1:nrow(paramset), function(index){
                        
                        preds.uo <- calculateUniversalOpportunity(params = unlist(paramset[index, c("alpha", "beta")]),
                                                                  observed.data = observed.data,
                                                                  Svar = unlist(paramset[index, "svar"]), 
                                                                  Tvar = unlist(paramset[index, "tvar"])) %>%
                          select(uo, uo.log)
                        
                        preds <- preds.uo %>%
                          select(uo) %>%
                          unlist()
                        
                        preds.log <- preds.uo %>%
                          select(uo.log) %>%
                          unlist()
                        
                        uo <- paramset[index,] %>% 
                          mutate(RMSE_identity = calculateRMSE(obs = obs,
                                                               preds = preds), 
                                 RMSE_log = calculateRMSE(obs = obs.log, 
                                                          preds = preds.log))
                        
                      })  %>% 
                        bind_rows() %>% 
                        return()
                    }


stopCluster(cl)


## rename

uo.fits <- fit.list


## save 
save(uo.fits, file = "./01-Data/02-Analytic-Data/uo_model_fits.rds")


## clean environment
rm(list=ls())
gc()

