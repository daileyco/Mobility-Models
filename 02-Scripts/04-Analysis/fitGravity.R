# fit gravity models


## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/distance_thresholds.rds")


## packages
library(dplyr)
library(doParallel)
library(foreach)
library(purrr)
library(tibble)
library(tidyr)

## helper functions
source("./02-Scripts/02-Helper-Functions/createDummies.R")
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






## fit models in parallel
### set up parallel computation
cl <- makeCluster(mc <- getOption("cl.cores", 15))

clusterExport(cl=cl, 
              varlist=c("createDummies", "calculateRMSE"))
clusterEvalQ(cl, 
             {
               
               library(dplyr)
               library(tidyr)
               library(tibble)
               
             })


registerDoParallel(cl)




fit.list <- foreach(observed.data=iter(od.list),
                    distance.threshold = distance.thresholds$minimum) %dopar% {
  
  observed.data <- createDummies(data=observed.data, dt = distance.threshold)
  
  fit <- lm(as.formula(paste0("log.Workers.in.Commuting.Flow~", paste0(names(observed.data)[c(21,36:90)], collapse = "+"))), 
            data = observed.data)
  
  # fit.ests <- cbind(coef(fit), confint(fit)) %>% 
  #   as.data.frame() %>%
  #   setNames(., nm = c("Beta", "LL", "UL")) %>% 
  #   
  #   rownames_to_column(., var = "Parameter") %>%
  #   mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  #   select(Parameter, Estimate) %>%
  #   add_row(., 
  #           .after = nrow(.), 
  #           Parameter = "AIC", Estimate = as.character(AIC(fit))) %>%
  #   add_row(., 
  #           .after = nrow(.), 
  #           Parameter = "BIC", Estimate = as.character(BIC(fit))) %>%
  #   pivot_wider(names_from = "Parameter", values_from = "Estimate") %>% 
  #   bind_cols(., 
  #             data.frame("RMSE" = calculateRMSE(observed.data$log.Workers.in.Commuting.Flow,
  #                                               predict(fit, 
  #                                                       observed.data, 
  #                                                       type = "response")), 
  #                        period = unique(observed.data$period), 
  #                        region = unique(observed.data$census.region.origin))) %>%
  #   select(period, region, everything())
  
  fit.ests <- cbind(coef(fit), confint(fit)) %>% 
    as.data.frame() %>%
    setNames(., nm = c("Beta", "LL", "UL")) %>% 
    
    rownames_to_column(., var = "Parameter") 
  
  fit.stats <- c(AIC = AIC(fit), 
                 BIC = BIC(fit), 
                 RMSE = calculateRMSE(observed.data$log.Workers.in.Commuting.Flow,
                                      predict(fit, 
                                              observed.data, 
                                              type = "response")))
  
  fit.data <- c(period = unique(observed.data$period), 
                region = unique(observed.data$census.region.origin))
  
  list(model = fit.data, 
       gof = fit.stats, 
       ests = fit.ests) %>% 
    return()
  
}


stopCluster(cl)


## compile results

gravity.fits <- lapply(fit.list, 
                       function(fit){
                         bind_cols(as.data.frame(t(pluck(fit, "model"))), 
                                   as.data.frame(t(pluck(fit, "gof")))) %>%
                           as_tibble() %>%
                           add_column(coefs = list(pluck(fit, "ests")))
                       }) %>%
  bind_rows()

gravity.fits$coefs <- lapply(gravity.fits$coefs, 
                             function(coefs){
                               coefs %>% 
                                 mutate(base = ifelse(grepl("[_]", Parameter), sub("^(.+)[_]indicator[.]long[.]distance[0-1]{1}[_].+$", "\\1", Parameter), NA), 
                                        long.distance = ifelse(grepl("[_]", Parameter), sub("^.+[_]indicator[.]long[.]distance([0-1]{1})[_].+$", "\\1", Parameter), NA), 
                                        pop.cat.origin = ifelse(grepl("[_]", Parameter), substr(Parameter, nchar(Parameter)-1, nchar(Parameter)-1), NA), 
                                        pop.cat.destination = ifelse(grepl("[_]", Parameter), substr(Parameter, nchar(Parameter), nchar(Parameter)), NA))
                             })

gravity.fits$grav.powers <- lapply(gravity.fits$coefs, 
                                   function(coefs){
                                     coefs %>%
                                       filter(!is.na(base)) %>% 
                                       select(-Parameter) 
                                       })

gravity.powers <- lapply(1:nrow(gravity.fits), 
                         function(index){
                           bind_cols(gravity.fits[index,1:2], 
                                     gravity.fits[[index,"grav.powers"]])
                         }) %>%
  bind_rows() %>%
  select(period, region, base, long.distance, pop.cat.origin, pop.cat.destination, Beta, LL, UL) %>%
  mutate(period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE), 
         region = factor(region, levels = c("All US", "Midwest", "Northeast", "South", "West"), ordered = TRUE), 
         base = factor(base, levels = c("log.POPESTIMATE.origin", "log.POPESTIMATE.destination", "log.distance.km"), labels = c("Population Size, Origin", "Population Size, Destination", "Distance (km)"), ordered = TRUE), 
         long.distance = factor(long.distance, levels = 0:1, labels = c("Short", "Long"), ordered = TRUE), 
         across(matches("pop.cat"), ~factor(.x, levels = c("s", "m", "l"), labels = c("Small", "Medium", "Large"), ordered = TRUE))) %>%
  arrange(across(1:6))


## save 
save(gravity.fits, gravity.powers, file = "./01-Data/02-Analytic-Data/gravity_model_estimates.rdata")


## clean environment
rm(list=ls())
gc()


