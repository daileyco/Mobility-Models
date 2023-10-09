# script to make summary tables for gravity model tuning for distance threshold

## load data
load("./01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")

## packages
library(dplyr)
library(tidyr)




## table distance thresholds summaries

table.gravity.distance.thresholds <- distance.thresholds %>%
  pivot_longer(c(distance_threshold, RMSE_identity, RMSE_log), names_to = "Parameter", values_to = "value") %>%
  mutate(value = round(value, 3), 
         Parameter = ifelse(Parameter == "distance_threshold", "Distance Threshold", Parameter)) %>%
  rename(Period = period, 
         `Model Variation` = model.extent, 
         `Objective Scale for Tuning` = objective.scale) %>%
  pivot_wider(names_from = region, values_from = value)




## save
save(table.gravity.distance.thresholds, file = "./03-Output/01-Tables/table_gravity_dt_tuning.rds")



## clean environment
rm(list=ls())
gc()


