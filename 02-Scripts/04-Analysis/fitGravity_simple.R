# fit gravity models


## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")


## packages
library(dplyr)
library(doParallel)
library(foreach)
library(purrr)
library(tibble)
library(tidyr)

## helper functions
source("./02-Scripts/02-Helper-Functions/createDummies_simple.R")
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



## subset distance thresholds to match length and ordering of data list
distance.thresholds <- distance.thresholds %>%
  filter(model.extent == "base*distance_threshold*large_populations" & 
           objective.scale == "log") #%>%
  # arrange(region, period)
distance.thresholds <- distance.thresholds[match(names(od.list), 
                                                 distance.thresholds %>%
                                                   mutate(pdt = paste0(period,
                                                                       ".",
                                                                       region))%>%
                                                   select(pdt)%>%
                                                   unlist()),]

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
                    obs.name=names(od.list),
                    distance.threshold = distance.thresholds$distance_threshold) %dopar% {
  
  observed.data <- createDummies(data=observed.data, dt = distance.threshold)
  
  fit <- lm(as.formula(paste0("log.Workers.in.Commuting.Flow~indicator.long.distance+indicator.large.population+", 
                              paste0(names(observed.data)[which(grepl("^.+[_]indicator[.]long[.]distance[0-1][_]indicator[.]large[.]population[0-1]$", 
                                                                      names(observed.data)))], 
                                     collapse = "+"))), 
            data = observed.data)
  

  fit.ests <- cbind(coef(fit), confint(fit)) %>% 
    as.data.frame() %>%
    setNames(., nm = c("Beta", "LL", "UL")) %>% 
    
    rownames_to_column(., var = "Parameter") 
  
  fit.preds <- bind_cols(observed.data %>%
                           select(period, fips.ij, census.region.origin, POPESTIMATE.origin, POPESTIMATE.destination, `Workers in Commuting Flow`, log.Workers.in.Commuting.Flow) %>%
                           mutate(id = paste0("gravitysimple_", obs.name)), 
                         preds = predict(fit, 
                                         observed.data, 
                                         type = "response"))
  
  
  
  fit.stats <- c(AIC = AIC(fit), 
                 BIC = BIC(fit), 
                 RMSE_log = calculateRMSE(observed.data$log.Workers.in.Commuting.Flow,
                                          fit.preds$preds), 
                 RMSE_identity = calculateRMSE(observed.data$`Workers in Commuting Flow`,
                                               exp(fit.preds$preds)))
  
  fit.data <- c(period = unique(observed.data$period), 
                region = unique(observed.data$census.region.origin), 
                dt = distance.threshold)
  
  groups.n <- observed.data %>% 
    rename(region = census.region.origin) %>%
    group_by(period, region, indicator.long.distance, indicator.large.population) %>% 
    summarise(n=n()) %>% 
    ungroup()
  
  list(model = fit.data, 
       gof = fit.stats, 
       ests = fit.ests, 
       groups.n = groups.n, 
       preds = fit.preds) %>% 
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
                                        large.populations = ifelse(grepl("[_]", Parameter), substr(Parameter, nchar(Parameter), nchar(Parameter)), NA))
                             })

gravity.fits$grav.powers <- lapply(gravity.fits$coefs, 
                                   function(coefs){
                                     coefs %>%
                                       filter(!is.na(base)) %>% 
                                       select(-Parameter) 
                                       })


groups.n <- lapply(fit.list, 
                   function(x){
                     pluck(x, "groups.n")
                   }) %>% 
  bind_rows() %>% 
  mutate(long.distance = as.character(indicator.long.distance), 
         large.populations = as.character(indicator.large.population)) %>% 
  select(-indicator.long.distance, -indicator.large.population)



gravity.powers <- lapply(1:nrow(gravity.fits), 
                         function(index){
                           bind_cols(gravity.fits[index,1:2], 
                                     gravity.fits[[index,"grav.powers"]])
                         }) %>%
  bind_rows() %>%
  left_join(., 
            groups.n, 
            by = c("period", "region", "long.distance", "large.populations")) %>%
  select(period, region, base, long.distance, large.populations, n, Beta, LL, UL) %>%
  mutate(period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE), 
         region = factor(region, levels = c("All US", "Midwest", "Northeast", "South", "West"), ordered = TRUE), 
         base = factor(base, levels = c("log.POPESTIMATE.origin", "log.POPESTIMATE.destination", "log.distance.km"), labels = c("Population Size, Origin", "Population Size, Destination", "Distance (km)"), ordered = TRUE), 
         long.distance = factor(long.distance, levels = 0:1, labels = c("Short", "Long"), ordered = TRUE), 
         large.populations = factor(large.populations, levels = 0:1, labels = c("Other", "Large-to-Large"), ordered = TRUE)) %>%
  arrange(across(1:5))



gravity.preds <- lapply(fit.list, 
                        (\(x) pluck(x, "preds"))) %>%
  bind_rows() %>%
  select(-period, -census.region.origin) %>%
  pivot_wider(names_from = id, values_from = preds)



## rename
gravity.fits.simple <- gravity.fits
gravity.powers.simple <- gravity.powers
gravity.preds.simple <- gravity.preds


## save 
save(gravity.fits.simple, gravity.powers.simple, file = "./01-Data/02-Analytic-Data/estimates_gravity_model_simple.rdata")
save(gravity.preds.simple, file = "./01-Data/02-Analytic-Data/predictions_flow_gravitysimple.rds")


## clean environment
rm(list=ls())
gc()


