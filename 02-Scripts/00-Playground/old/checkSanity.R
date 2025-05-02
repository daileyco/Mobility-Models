# sanity check 
## compare lm() fit values to manually calculated in calculateGravity()



## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)


## helper functions
source("./02-Scripts/02-Helper-Functions/calculateGravity.R")
source("./02-Scripts/02-Helper-Functions/RMSE.R")






## objective function for Metropolis random walk
### extracts log of commuter flow volume
### estimates commuter flow volume with gravity model given parameters
### calculates root mean square error between observed and estimate values

calibrateGravity <- function(params, oddf){
  
  require(dplyr)
  
  
  flow.obs <- oddf %>% 
    select(log.Workers.in.Commuting.Flow) %>%
    unlist()
  
  
  flow.preds.gravity <- calculateGravity(params=params, oddf=oddf) %>%
    select(gravity.flow.log) %>%
    unlist()
  
  
  RMSE(obs = flow.obs,
       preds = flow.preds.gravity) %>%
    return()
  
}





## Metropolis Random Walk setup


# as.numeric(Sys.time())
# 1693781865
set.seed(1693781865)
tau <- 10
mcruns <- 100


param.names <- c(paste0("beta", 0:13), 
                 "dt", 
                 "Pt", 
                 "accept", 
                 "alpha", 
                 "rmse")

chains.df <- matrix(c(NA), nrow = mcruns, ncol = length(param.names)) %>% 
  as.data.frame() %>% 
  setNames(., nm = param.names)



### initial values

chains.df[1,] <- c(1,
                   1,1,
                   -1,
                   rep(0,10),
                   log(100), 
                   quantile(od$log.POPESTIMATE.destination[which(!duplicated(od[,1:3]))], 
                            probs = 0.75), 
                   NA, 
                   NA, 
                   NA)


chains.df$rmse[1] <- calibrateGravity(params = chains.df[1,1:16], oddf = od)





## set up df, alter the values for internal flow observations

temp.od <- od %>%
  mutate(log.POPESTIMATE.destination = ifelse(distance.km==0, 0, log.POPESTIMATE.destination),
         log.distance.km = ifelse(distance.km==0, 0, log.distance.km))


## fit basic gravity model

### as log linear model using lm()
grav.fit1 <- lm(log.Workers.in.Commuting.Flow ~
                  log.POPESTIMATE.origin +
                  log.POPESTIMATE.destination +
                  log.distance.km,
                data = temp.od)
# summary(grav.fit1)


### use lm() coef estimates with custom function
#### zeros for other parameters for simple model
temp.params <- c(coef(grav.fit1), rep(0,12))

temp.gravity <- calculateGravity(temp.params, temp.od)

## sanity check
### should be equal values

#### visual
View(cbind(temp.od, predict(grav.fit1, temp.od), temp.gravity))

#### computational
all.equal.numeric(as.numeric(unlist(predict(grav.fit1, temp.od))),
                  as.numeric(unlist(temp.gravity$gravity.flow.log)))
# TRUE



## clean environment
rm(list=ls())
gc()







## old code to fit with logistic transformation of proportions
### dropped as not directly comparable to true gravity models (?)
# # # 
# # # temp.od <- od %>%
# # #   mutate(flow.proportion = `Workers in Commuting Flow` / POPESTIMATE.origin) %>%
# # #   mutate(flow.logistic = log(flow.proportion / (1-flow.proportion))) %>%
# # #   mutate(log.POPESTIMATE.destination = ifelse(distance.km==0, 0, log.POPESTIMATE.destination), 
# # #          log.distance.km = ifelse(distance.km==0, 0, log.distance.km))
# # # 
# # # 
# # # ##### fit basic gravity model
# # # grav.fit1 <- lm(flow.logistic ~ 
# # #                   log.POPESTIMATE.origin + 
# # #                   log.POPESTIMATE.destination + 
# # #                   log.distance.km, 
# # #                 data = temp.od)
# # # summary(grav.fit1)
# # # 
# # # temp.params <- c(coef(grav.fit1), rep(0,12))
# # # 
# # # temp.gravity <- calculateGravity(temp.params, temp.od) # wont give gravity.logistic, changed, calibrateGravity() changed too
# # # 
# # # 
# # # # View(cbind(temp.od, predict(grav.fit1, temp.od), temp.gravity))
# # # # all.equal.numeric(as.numeric(unlist(predict(grav.fit1, temp.od))), 
# # # #                   as.numeric(unlist(temp.gravity$gravity.logistic)))
# # # # # TRUE


















