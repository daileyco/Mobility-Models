# script to tune the parameters for the universal opportunity model for different data subsets
## i.e., alpha, aka the exploratory tendency, and beta, aka the cautious tendency


## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/predictions_totalcommuters.rds")

## packages
library(dplyr)
library(foreach)
library(doParallel)
library(doRNG)
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




## define objective function to optimize fit (minimize returned value)
### root mean square error 
### optim() needs first argument to be parameters
calculateRMSEforUOModel <- function(params, observed.data, Svar = "sij", Tvar = "Total.Workers.Residence.Domestic", objective.scale = "log"){
  
  require(dplyr)
  
  if(objective.scale=="identity"){
    
    ### extract the column with the observed flux, log-transformed(?)
    obs <- observed.data %>% 
      select(`Workers in Commuting Flow`) %>%
      unlist()
    
    ### estimate flux with given parameters
    #### helper function takes same arguments
    preds <- calculateUniversalOpportunity(params=params, observed.data=observed.data, Svar = Svar, Tvar = Tvar) %>%
      select(uo) %>%
      unlist()
    
  }
  
  if(objective.scale=="log"){
    
    ### extract the column with the observed flux, log-transformed(?)
    obs <- observed.data %>% 
      select(log.Workers.in.Commuting.Flow) %>%
      unlist()
    
    ### estimate flux with given parameters
    #### helper function takes same arguments
    preds <- calculateUniversalOpportunity(params=params, observed.data=observed.data, Svar = Svar, Tvar = Tvar) %>%
      select(uo.log) %>%
      unlist()
    
  }
  
  ### compare values observed and estimated/predicted
  calculateRMSE(obs = obs,
                preds = preds) %>%
    return()
  
}





## constraints
### constraints needed 
#### alpha and beta both non-negative 
#### alpha + beta <= 1

### constrOptim() uses constraint matrix and constraint vector
#### ui %*% theta - ci >=0


### indicated in order
#### alpha must be non-negative
#### alpha must be greater than or equal to one
#### beta must be non-negative
#### beta must be greater than or equal to one
#### alpha + beta must be less than or equal to one
constraint.matrix <- matrix(data = c(1,-1,0,0,-1, 
                                     0,0,1,-1,-1), 
                            ncol = 2)


constraint.vector <- c(0,-1,0,-1,-1)

### sanity check
# test.grid <- expand.grid(alpha = runif(100, min = -1, max = 2), 
#                          beta = runif(100, min = -1, max = 2)) %>%
#   mutate(satisfies.constraints = alpha>=0 & alpha<=1 & beta>=0 & beta<=1 & alpha+beta<=1)
# 
# test.grid$satisfies.constraints.w.matrix.vector <- apply(test.grid, 
#                                                          MARGIN = 1, 
#                                                          function(x){
#                                                            sum(!(constraint.matrix%*%c(x[1], x[2])-constraint.vector >= 0))==0
#                                                          })
# 
# all.equal(test.grid$satisfies.constraints, test.grid$satisfies.constraints.w.matrix.vector)





## run optimizer in parallel for each data subset
### set seed for reproducibility
# Sys.Date()
# # "2023-09-25"
# as.numeric(Sys.Date())
# # 19625
set.seed(19625)


### set up parallel computation
cl <- makeCluster(mc <- getOption("cl.cores", 15))

clusterExport(cl=cl, 
              varlist=c("calculateUniversalOpportunity", 
                        "calculateRMSE", 
                        "calculateRMSEforUOModel", 
                        "constraint.matrix", 
                        "constraint.vector"))
clusterEvalQ(cl, 
             {
               
               library(dplyr)
               library(tidyr)
               
             })


registerDoParallel(cl)



### optimizer
#### starting point set using alpha = 0.25 and beta = 0.25
##### must be interior of feasible region, not on boundary, 
###### but function can still return minimum on boundary
#### method choice
##### no provided gradient function (idk where to even start with that) = Nelder-Mead 


uo.params <- foreach(observed.data=iter(od.list), 
                     .combine = bind_rows) %dorng% {

  svars <- c("sij", "sij_within")
  tvars <- c("Total.Workers.Residence.Domestic", "Total.Commuters_pred")
  objective.scale <- c("identity", "log")
  
  combos <- expand.grid(svar=svars, 
                        tvar=tvars, 
                        objective.scale=objective.scale) %>%
    mutate(across(everything(), ~as.character(.x)))
  
  
  lapply(1:nrow(combos), function(index){
    
    constrOptim(theta = c(0.25,0.25), 
                f = calculateRMSEforUOModel, 
                observed.data = observed.data,
                Svar = combos$svar[index],
                Tvar = combos$tvar[index],
                objective.scale = combos$objective.scale[index],
                ui = constraint.matrix, 
                ci = constraint.vector, 
                grad = NULL)
    
  })  %>% bind_rows() %>% 
    mutate(parameter = rep(c("alpha", "beta"),8)) %>% 
    select(-counts,-outer.iterations, -barrier.value) %>% 
    pivot_wider(names_from = parameter, values_from = par) %>% 
    bind_cols(., combos) %>%
    mutate(period = unique(observed.data$period), 
           region = unique(observed.data$census.region.origin))%>%
    select(period, region, svar, tvar, convergence, alpha, beta, objective.scale, RMSE=value) %>%
    group_by(svar, tvar, objective.scale) %>%
    mutate(complementary.objective = calculateRMSEforUOModel(params = c(.data[["alpha"]], .data[["beta"]]),
                                                             observed.data = observed.data,
                                                             Svar = .data[["svar"]],
                                                             Tvar = .data[["tvar"]],
                                                             objective.scale = ifelse(.data[["objective.scale"]]=="identity",
                                                                                      "log",
                                                                                      "identity"))) %>%
    ungroup() %>%
    return()
  
}


stopCluster(cl)


## clean

uo.params <- uo.params %>% 
  pivot_longer(c(RMSE, complementary.objective), 
               names_to = "obj", 
               values_to = "RMSE") %>% 
  mutate(obj = ifelse(obj=="RMSE", 
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
  arrange(period, region, svar, tvar, objective.scale) %>% 
  mutate(across(where(is.factor), 
                ~as.character(.x)))


## save 
save(uo.params, file = "./01-Data/02-Analytic-Data/uo_model_params.rds")


## clean environment
rm(list=ls())
gc()

