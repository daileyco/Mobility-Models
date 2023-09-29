# generate parameter estimates for gravity model






od <- od %>%
  mutate(po.cat = findInterval(log.POPESTIMATE.origin, 
                               quantile(od$log.POPESTIMATE.origin[which(!duplicated(od[,1:3]))], probs = 1:2/3)), 
         pd.cat = findInterval(log.POPESTIMATE.destination, 
                               quantile(od$log.POPESTIMATE.origin[which(!duplicated(od[,1:3]))], probs = 1:2/3))) %>%
  mutate(p.cats = case_when(po.cat == 0 & pd.cat == 0 ~ "ss", 
                            po.cat == 0 & pd.cat == 1 ~ "sm",
                            po.cat == 0 & pd.cat == 2 ~ "sl",
                            po.cat == 1 & pd.cat == 0 ~ "ms", 
                            po.cat == 1 & pd.cat == 1 ~ "mm",
                            po.cat == 1 & pd.cat == 2 ~ "ml",
                            po.cat == 2 & pd.cat == 0 ~ "ls", 
                            po.cat == 2 & pd.cat == 1 ~ "lm",
                            po.cat == 2 & pd.cat == 2 ~ "ll")) %>%
  mutate(indicator.long.distance = ifelse(log.distance.km>median(od$log.distance.km), 1, 0)) %>%
  mutate(p.cats = relevel(factor(p.cats), ref = "ll"))


od <- od %>%
  mutate(across(c(log.POPESTIMATE.origin, log.POPESTIMATE.destination, log.distance.km), ~ifelse(indicator.long.distance==0, .x, 0), .names = "{.col}_short"), 
         across(c(log.POPESTIMATE.origin, log.POPESTIMATE.destination, log.distance.km), ~ifelse(indicator.long.distance==1, .x, 0), .names = "{.col}_long"))








