# script to fit the universal opportunity model variations for different data subsets


## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/predictions_totalcommuters.rds")
load("./01-Data/02-Analytic-Data/parameters_uo_alphabeta.rds")

## packages
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
library(purrr)


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
            mutate(., model = "OO", alpha = 0, beta = 0, objective.scale="")%>%
              filter(!duplicated(.)), 
            mutate(., model = "OPS", alpha = 1, beta = 0, objective.scale="")%>%
              filter(!duplicated(.)), 
            mutate(., model = "Radiation", alpha = 0, beta = 1, objective.scale="")%>%
              filter(!duplicated(.))) %>%
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
               library(tidyr)
               library(purrr)
               
             })


registerDoParallel(cl)



### generate estimates

fit.list <- foreach(observed.data=iter(od.list),
                    obs.name = names(od.list),
                    paramset=iter(uo.params.list)) %dopar% {
                      
                      
                      obs <- observed.data$`Workers in Commuting Flow`
                      obs.log <- observed.data$log.Workers.in.Commuting.Flow
                      
                      lapply(1:nrow(paramset), function(index){
                        
                        preds.uo <- calculateUniversalOpportunity(params = unlist(paramset[index, c("alpha", "beta")]),
                                                                  observed.data = observed.data,
                                                                  Svar = unlist(paramset[index, "svar"]), 
                                                                  Tvar = unlist(paramset[index, "tvar"])) %>%
                          select(uo, uo.log)
                        
                        preds.id <- preds.uo %>%
                          select(uo) %>%
                          unlist()
                        
                        preds.log <- preds.uo %>%
                          select(uo.log) %>%
                          unlist()
                        
                        uo <- paramset[index,] %>% 
                          mutate(RMSE_identity = calculateRMSE(obs = obs,
                                                               preds = preds.id), 
                                 RMSE_log = calculateRMSE(obs = obs.log, 
                                                          preds = preds.log))
                        
                        
                        preds <- bind_cols(observed.data%>%
                                             select(period, fips.ij, census.region.origin, POPESTIMATE.origin, POPESTIMATE.destination, `Workers in Commuting Flow`, log.Workers.in.Commuting.Flow) %>%
                                             mutate(id = paste0("uo_", obs.name), 
                                                    alpha = unlist(paramset[index, c("alpha")]), 
                                                    beta = unlist(paramset[index, c("beta")]),
                                                    Svar = unlist(paramset[index, "svar"]), 
                                                    Tvar = unlist(paramset[index, "tvar"]), 
                                                    model = unlist(paramset[index, "model"]),
                                                    objective.scale = unlist(paramset[index, "objective.scale"])), 
                                           preds.uo)
                        
                        list(uo=uo, 
                             preds=preds) %>%
                          return()
                        
                      })  %>% 
                        (\(x) {
                          uo.fits <- lapply(x, (\(y) pluck(y, "uo"))) %>%
                            bind_rows()
                          uo.preds <- lapply(x, (\(y) pluck(y, "preds"))) %>%
                            bind_rows() %>%
                            select(-alpha, -beta, -period, -census.region.origin) %>%
                            pivot_wider(names_from = c(id, model, objective.scale, Svar, Tvar ), values_from = c(uo, uo.log), names_prefix = "preds", names_sep = "__")
                          # 
                          list(uo.fits=uo.fits, 
                               uo.preds=uo.preds)%>%
                            return()
                        }) %>% 
                        return()
                    }


stopCluster(cl)


## compile

uo.fits <- lapply(fit.list, 
                  (\(x) pluck(x, "uo.fits"))) %>%
  bind_rows()


for(i in 1:length(fit.list)){
  
  preds <- pluck(fit.list[[i]], "uo.preds")
  
  if(i==1){
    uo.preds <<- full_join(od, preds, by = c("fips.ij", "POPESTIMATE.origin", "POPESTIMATE.destination", "Workers in Commuting Flow", "log.Workers.in.Commuting.Flow"))
  }else{
    uo.preds <<- full_join(uo.preds, preds, by = c("fips.ij", "POPESTIMATE.origin", "POPESTIMATE.destination", "Workers in Commuting Flow", "log.Workers.in.Commuting.Flow"))
  }
  
}

## save 
save(uo.fits, file = "./01-Data/02-Analytic-Data/fits_uo_model.rds")
save(uo.preds, file = "./01-Data/02-Analytic-Data/predictions_flow_uo.rds")

## clean environment
rm(list=ls())
gc()

