# script to make summary tables for gravity model fits

## load data
load("./01-Data/02-Analytic-Data/estimates_gravity_model.rdata")
load("./01-Data/02-Analytic-Data/estimates_gravity_model_simple.rdata")

## packages
library(dplyr)
library(tidyr)




## gravity fit stats table
table.gravity.fits <- gravity.fits %>% 
  select(-coefs, -grav.powers) %>%
  mutate(`Distance Threshold` = round(as.numeric(dt), 3), 
         across(c(AIC, BIC, RMSE_log, RMSE_identity), ~round(.x, 3))) %>%
  select(-dt) %>%
  pivot_longer(c(`Distance Threshold`, AIC, BIC, RMSE_log, RMSE_identity), names_to = "Parameter", values_to = "value") %>%
  pivot_wider(names_from = region, values_from = value) %>%
  rename(Period = period) %>%
  filter(!Parameter%in%c("AIC", "BIC")) #%>%
  # mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  # arrange(Period) %>%
  # mutate(Period = ifelse(duplicated(Period), "", as.character(Period)))
  




## gravity power parameters table
table.gravity.powers <- gravity.powers %>%
  mutate(`Power (95%CI)` = paste0(round(Beta, 3), " (", round(LL,3), ", ", round(UL,3), ")"), 
         n = paste0("n=", n), 
         `Power (95%CI)` = ifelse(is.na(Beta), "--", `Power (95%CI)`)) %>%
  select(-Beta, -LL, -UL) %>%
  pivot_longer(c(n,`Power (95%CI)`), names_to = "Parameter", values_to = "value") %>%
  mutate(Parameter = ifelse(Parameter=="n", "", 
                            ifelse(substr(base,1,1)=="P", 
                                   paste0(sub("^.+, (.+)$", "\\1", base), " ", sub(",.+", "", base), " ", Parameter), 
                                   paste0(base, " ", Parameter)))) %>%
  arrange(period, long.distance, pop.cat.origin, pop.cat.destination) %>%
  rename(Period = period, 
         `Flow Distance Category` = long.distance, 
         `Population Size Category, Origin` = pop.cat.origin, 
         `Population Size Category, Destination` = pop.cat.destination) %>%
  pivot_wider(names_from = region, values_from = value) %>%
  select(-base) %>%
  filter(!duplicated(.))




## simpler gravity fit stats table
table.gravity.fits.simple <- gravity.fits.simple %>% 
  select(-coefs, -grav.powers) %>%
  mutate(`Distance Threshold` = round(as.numeric(dt), 3), 
         across(c(AIC, BIC, RMSE_log, RMSE_identity), ~round(.x, 3))) %>%
  select(-dt) %>%
  pivot_longer(c(`Distance Threshold`, AIC, BIC, RMSE_log, RMSE_identity), names_to = "Parameter", values_to = "value") %>%
  pivot_wider(names_from = region, values_from = value) %>%
  rename(Period = period) %>%
  filter(!Parameter%in%c("AIC", "BIC")) #%>%
  # mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  # arrange(Period) %>%
  # mutate(Period = ifelse(duplicated(Period), "", as.character(Period)))





## simpler gravity power parameters table
table.gravity.powers.simple <- gravity.powers.simple %>%
  mutate(`Power (95%CI)` = paste0(round(Beta, 3), " (", round(LL,3), ", ", round(UL,3), ")"), 
         n = paste0("n=", n)) %>%
  select(-Beta, -LL, -UL) %>%
  pivot_longer(c(n,`Power (95%CI)`), names_to = "Parameter", values_to = "value") %>%
  mutate(Parameter = ifelse(Parameter=="n", "", 
                            ifelse(substr(base,1,1)=="P", 
                                   paste0(sub("^.+, (.+)$", "\\1", base), " ", sub(",.+", "", base), " ", Parameter), 
                                   paste0(base, " ", Parameter)))) %>%
  arrange(period, long.distance, large.populations) %>%
  rename(Period = period, 
         `Flow Distance Category` = long.distance, 
         `Population Size Categories Pairing` = large.populations) %>%
  pivot_wider(names_from = region, values_from = value) %>%
  select(-base) %>%
  filter(!duplicated(.))


## save
save(table.gravity.fits, 
     table.gravity.fits.simple, file = "./03-Output/01-Tables/tables_fits_gravity.rdata")
save(table.gravity.powers, 
     table.gravity.powers.simple, file = "./03-Output/01-Tables/tables_gravity_power.rdata")



## clean environment
rm(list=ls())
gc()


