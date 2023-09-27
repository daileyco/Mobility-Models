# script to estimate total commuters (workers) based on population size, time period, and census region


## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)
library(tibble)
library(tidyr)

## helper functions
source("./02-Scripts/02-Helper-Functions/calculateRMSE.R")



## summarise data to get total commuters for each residence location
od <- od %>% 
  group_by(period, `County FIPS Residence`, `County Residence`, `State FIPS Residence`, `State Residence`, POPESTIMATE.origin, census.region.origin, Total.Workers.Residence, Total.Workers.Residence.Domestic) %>%
  summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`)) %>%
  ungroup()


## transform to log scale
od <- od %>%
  mutate(log.Workers.in.Commuting.Flow = log(`Workers in Commuting Flow`), 
         log.POPESTIMATE.origin = log(POPESTIMATE.origin))



## regress
### simple, only origin population
fit1 <- lm(log.Workers.in.Commuting.Flow ~ 
             log.POPESTIMATE.origin, 
           data = od)
# summary(fit1)

### add time period
fit2 <- update(fit1, ~.+period)
# summary(fit2)

### add census region
fit3 <- update(fit2, ~.+census.region.origin)
# summary(fit3)

### add time period interaction
fit4 <- update(fit3, ~.+period*log.POPESTIMATE.origin)
# summary(fit4)

### add census region interaction
fit5 <- update(fit4, ~.+census.region.origin*log.POPESTIMATE.origin)
# summary(fit5)
# car::Anova(fit5)



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
         Terms = c("~log(Origin Population)", 
                   "+ Time Period", 
                   "+ Origin Census Region", 
                   "+ (Time Period)*log(Origin Population)", 
                   "+ (Origin Census Region)*log(Origin Population)")) %>%
  select(Model, Terms, everything()) %>%
  mutate(`Pr(>F)` = ifelse(`Pr(>F)`<0.001, "<0.001", round(`Pr(>F)`, 3))) %>%
  mutate(across(c(RSS, all_of("Sum of Sq"), `F`, AIC, BIC, RMSE), ~round(.x, 3)))


### rename
fit.anovas.totalcommuters <- fit.anovas



## extract fit coefficients and confidence intervals

fit.ests <- lapply(mget(ls(pattern="fit[0-9]{1}")), 
                   function(fit){
                     fit.ests <- cbind(coef(fit), confint(fit)) %>%
                       as.data.frame() %>%
                       setNames(., nm = c("Beta", "LL", "UL")) %>%
                       
                       rownames_to_column(., var = "Parameter") %>%
                       mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
                       select(Parameter, Estimate) %>%
                       pivot_wider(names_from = "Parameter", values_from = "Estimate") %>%
                       return()
                   }) %>%
  bind_rows(.id = "fit") %>%
  mutate(fit = sub("fit", "Model ", fit)) %>%
  pivot_longer(!fit, names_to = "Parameter", values_to = "Beta (95%CI)") %>%
  pivot_wider(names_from = "fit", values_from = "Beta (95%CI)")

### make pretty table
fit.ests <- fit.ests %>%
  mutate(Parameter = factor(c("(Intercept)", 
                              "log(Origin Population)", 
                              "Time Period", 
                              "Origin Census Region", 
                              "Origin Census Region", 
                              "Origin Census Region", 
                              "(Time Period)*log(Origin Population)", 
                              "(Origin Census Region)*log(Origin Population)", 
                              "(Origin Census Region)*log(Origin Population)", 
                              "(Origin Census Region)*log(Origin Population)"), 
                            levels = c("(Intercept)", 
                                       "log(Origin Population)", 
                                       "Time Period", 
                                       "Origin Census Region", 
                                       "(Time Period)*log(Origin Population)", 
                                       "(Origin Census Region)*log(Origin Population)"),
                            ordered = TRUE), 
         Level = c("", 
                   "", 
                   "2016-2020", 
                   "Northeast", 
                   "South", 
                   "West", 
                   "2016-2020", 
                   "Northeast", 
                   "South", 
                   "West")) %>%
  select(Parameter, Level, everything())

#### add rows for reference levels
fit.ests[nrow(fit.ests)+1,] <- t(c("Time Period", "2011-2015 (REF)", rep(NA, 5)))
fit.ests[nrow(fit.ests)+1,] <- t(c("(Time Period)*log(Origin Population)", "2011-2015 (REF)", rep(NA, 5)))
fit.ests[nrow(fit.ests)+1,] <- t(c("Origin Census Region", "Midwest (REF)", rep(NA, 5)))
fit.ests[nrow(fit.ests)+1,] <- t(c("(Origin Census Region)*log(Origin Population)", "Midwest (REF)", rep(NA, 5)))

fit.ests <- fit.ests %>%
  arrange(Parameter, Level)

### rename
fit.ests.totalcommuters <- fit.ests



## generate predictions for best fit model
od$log.Total.Commuters_pred <- predict(fit5, 
                                       od, 
                                       type = "response")

preds.totalcommuters <- od %>%
  select(c(1:5,13))



## save 
### tables
save(fit.anovas.totalcommuters, file = "./03-Output/01-Tables/model_anovas_totalcommuters.rds")
save(fit.ests.totalcommuters, file = "./03-Output/01-Tables/model_coefs_totalcommuters.rds")
### predictions
save(preds.totalcommuters, file = "./01-Data/02-Analytic-Data/predictions_totalcommuters.rds")


## clean environment
rm(list=ls())
gc()

