# script to make summary tables for gravity model tuning for distance threshold

## load data
load("./01-Data/02-Analytic-Data/parameters_uo_alphabeta.rds")

## packages
library(dplyr)
library(tidyr)




## table distance thresholds summaries

table.uo.params <- uo.params %>%
  select(-convergence) %>%
  pivot_longer(c(alpha, beta, RMSE_identity, RMSE_log), names_to = "Parameter", values_to = "value") %>%
  mutate(value = round(value, 3), 
         svar = factor(svar, levels = c("sij", "sij_within"), ordered=TRUE, labels = c("Comprehensive, Census Data", "Limited, Commuting Data")), 
         tvar = factor(tvar, levels = c("Total.Workers.Residence.Domestic", "Total.Commuters_pred"), ordered=TRUE, labels = c("Ti, Observed", "Ti, Predicted"))) %>%
  rename(Period = period, 
         `Surrounding Population, s_{ij}` = svar, 
         `Total Commuters, T_{i}` = tvar,
         `Objective Scale for Tuning` = objective.scale) %>%
  pivot_wider(names_from = region, values_from = value) %>%
  arrange(Period, `Surrounding Population, s_{ij}`, `Total Commuters, T_{i}`)




## save
save(table.uo.params, file = "./03-Output/01-Tables/table_uo_ab_tuning.rds")



## clean environment
rm(list=ls())
gc()


