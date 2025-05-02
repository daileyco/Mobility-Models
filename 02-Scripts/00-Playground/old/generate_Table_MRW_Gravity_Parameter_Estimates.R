# script to summarize Metropolis random walk results for gravity model parameter estimates
## as loop around all saved MRW dataframes



## packages
library(dplyr)
library(tidyr)

## get filepaths for saved MRW dataframes
mrw.dfs <- list.files("./01-Data/02-Analytic-Data", 
                      pattern = "mrw_gravity", 
                      full.names = TRUE)

### extract suffixes from names
mrw.ids <- sub("^.+[/]mrw_gravity_(.*)[.]rds$", 
               "\\1", 
               mrw.dfs)




## loop through files

for(ii in 1:length(mrw.dfs)){
  
  ## load data
  load(mrw.dfs[ii])
  
  
  assign(paste0("param.ests", ii), 
         chains.df %>%
           select(1:16) %>%
           summarise(across(1:16, 
                            list(mean = ~mean(.x), 
                                 lb = ~quantile(.x, probs = 0.025),  
                                 ub = ~quantile(.x, probs = 0.975)))) %>%
           pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>%
           mutate(type = sub("^.+[_](.+)$", "\\1", parameter), 
                  parameter = sub("^(.+)[_].+$", "\\1", parameter)) %>%
           pivot_wider(names_from = type, values_from = estimate) %>% 
           mutate(estimate = paste0(round(mean,3), " (", round(lb,3), ", ", round(ub,3), ")")) %>% 
           select(parameter, estimate) %>% 
           pivot_wider(names_from = parameter, values_from = estimate) %>%
           mutate(tau = sub("[_]", ".", sub("^tau(.+)[_]mc.+$", "\\1", mrw.ids[ii])), 
                  mcruns = sub("k", "000", sub("^tau.+[_]mc(.+)[_].+$", "\\1", mrw.ids[ii])), 
                  proposal = sub("^tau.+[_]mc.+[_](.+)$", "\\1", mrw.ids[ii]))
  )
  
}


param.ests <- bind_rows(mget(ls(pattern = "param.ests"))) %>%
  mutate(file = mrw.dfs) %>% 
  select(file, proposal, tau, mcruns, everything())










