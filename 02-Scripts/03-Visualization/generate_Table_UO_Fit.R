# script to make summary tables for gravity model fits

## load data
load("./01-Data/02-Analytic-Data/fits_uo_model.rds")

## packages
library(dplyr)
library(tidyr)



## uo fit stats table
table.uo.fits <- uo.fits %>% 
  mutate(objective.scale = ifelse(model=="UO", objective.scale, "")) %>%
  filter(!duplicated(.)) %>%
  pivot_longer(c(alpha, beta, RMSE_identity, RMSE_log), names_to = "Parameter", values_to = "value") %>%
  mutate(value = format(round(value, 3), drop0trailing = TRUE, trim = TRUE), 
         svar = factor(svar, levels = c("sij", "sij_within"), ordered=TRUE, labels = c("Comprehensive, Census Data", "Limited, Commuting Data")), 
         tvar = factor(tvar, levels = c("Total.Workers.Residence.Domestic", "Total.Commuters_pred"), ordered=TRUE, labels = c("Observed", "Predicted"))) %>%
  pivot_wider(names_from = region, values_from = value) %>%
  rename(Period = period, 
         `Surrounding Population, s_{ij}` = svar, 
         `Total Commuters, T_{i}` = tvar, 
         `Objective Scale for Tuning` = objective.scale, 
         Model = model) %>%
  mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  arrange(`Surrounding Population, s_{ij}`, `Total Commuters, T_{i}`, Period)




## save
save(table.uo.fits, file = "./03-Output/01-Tables/table_fits_uo.rds")



## clean environment
rm(list=ls())
gc()


