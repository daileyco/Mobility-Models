# Compare variations of gravity models

## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/distance_thresholds.rds")


## packages
library(dplyr)
library(tibble)

## helper functions
source("./02-Scripts/02-Helper-Functions/calculateRMSE.R")


## set up data for model fits

### filter out the zero-distance flows
od <- od %>% 
  filter(distance.km!=0)


### extract single distance threshold for use in all models
this.distance.threshold <- distance.thresholds %>%
  filter(period == "2011-2020" & 
           region == "All US" & 
           model.extent == "base*distance_threshold*population_categories" & 
           objective.scale == "log") %>%
  select(distance_threshold) %>%
  unlist()


### calculate indicators for long distance flows and large population pairings
od <- od %>%
  mutate(indicator.long.distance = ifelse(log.distance.km>this.distance.threshold, 1, 0), 
         indicator.large.population = ifelse(pop.cats=="ll", 1, 0))






## fit models

### base gravity model
fit1 <- lm(log.Workers.in.Commuting.Flow~
             log.POPESTIMATE.origin 
           + log.POPESTIMATE.destination 
           + log.distance.km, 
           data = od)
# summary(fit1)

### add interaction terms for an indicator for long distance flows
fit2 <- update(fit1, ~.*indicator.long.distance)
# summary(fit2)

### add interaction terms for large population pairings
fit3 <- update(fit2, ~.*indicator.large.population)
# summary(fit3)

### add interaction terms for population size categories of origin destination pairing
fit4 <- update(fit2, ~.*pop.cats)
# summary(fit4)

### add interaction terms for the census region of the origin
fit5 <- update(fit4, ~.*census.region.origin)
# summary(fit5)
# car::Anova(fit5, type=3)

### add interaction terms for the census region of the origin
fit6 <- update(fit5, ~.*period)
# summary(fit6)
# car::Anova(fit6, type=3)


## compare model fits
fit.anovas <- do.call(anova, unname(mget(ls(pattern="fit[0-9]{1}"))))


### add other fit stats and make pretty
fit.anovas <- lapply(mget(ls(pattern="fit[0-9]{1}")), 
                     function(fit){
                       c(AIC = AIC(fit), 
                         BIC = BIC(fit), 
                         RMSE = calculateRMSE(obs = od$log.Workers.in.Commuting.Flow, 
                                              preds = predict(fit, 
                                                              od, 
                                                              type = "response"))) %>%
                         return()
                     }) %>% 
  bind_rows() %>%
  bind_cols(fit.anovas, 
            .) %>%
  mutate(Model = row_number(), 
         Terms = c("~log(Origin Population)+log(Destination Population)+log(Distance)", 
                   "*I(Long Distance)", 
                   "*(Large-Large Population Size Pairings)",
                   "*(Population Size Category Pairings)", 
                   "*(Origin Census Region)", 
                   "*(Time Period)")) %>%
  select(Model, Terms, everything()) %>%
  mutate(`Pr(>F)` = ifelse(`Pr(>F)`<0.001, "<0.001", round(`Pr(>F)`, 3))) %>%
  mutate(across(c(RSS, all_of("Sum of Sq"), `F`, AIC, BIC, RMSE), ~round(.x, 3)))


## rename
fit.anovas.gravity <- fit.anovas


## save 
save(fit.anovas.gravity, file = "./03-Output/01-Tables/model_anovas_gravity.rds")


## clean environment
rm(list=ls())
gc()


