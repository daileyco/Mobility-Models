## load data
load("./01-Data/02-Analytic-Data/predictions_flow_gravity.rds")
load("./01-Data/02-Analytic-Data/predictions_flow_gravitysimple.rds")
load("./01-Data/02-Analytic-Data/predictions_flow_uo.rds")

## packages
library(dplyr)
library(tidyr)

## clean

### gravity
predictions.gravity <- full_join(gravity.preds, 
                                 gravity.preds.simple) %>%
  mutate(across(matches("gravity"), ~exp(.x), .names = "{col}_identity"))


gravity.pred.vars <- bind_cols(var = names(predictions.gravity)[which(grepl("gravity", names(predictions.gravity)))], 
                               names(predictions.gravity)[which(grepl("gravity", names(predictions.gravity)))] %>% 
                                 strsplit(., split = "_|[.]") %>% 
                                 lapply(., (\(x) setNames(x, nm = c("model", "period", "region", "scale")[1:length(x)]))) %>% 
                                 bind_rows() %>% 
                                 mutate(scale = ifelse(is.na(scale), "log", scale))
) %>%
  mutate(modelshort = case_when(model=="gravity" ~ "G2", 
                                TRUE ~ "G1"))


### uo
uo.pred.vars <- bind_cols(var = names(uo.preds)[which(grepl("predsuo", names(uo.preds)))], 
                          names(uo.preds)[which(grepl("predsuo", names(uo.preds)))] %>% 
                            strsplit(., split = "__") %>% 
                            lapply(., 
                                   (\(x){
                                     setNames(x, nm = c("scale", "periodregion", "model", "objective.scale", "svar", "tvar"))
                                   })) %>% 
                            bind_rows() %>% 
                            mutate(scale = ifelse(grepl("log", scale), "log", "identity"), 
                                   period = sub("^predsuo_([0-9]{4}[-][0-9]{4})[.].+$", "\\1", periodregion), 
                                   region = sub("^predsuo_[0-9]{4}[-][0-9]{4}[.](.+)$", "\\1", periodregion)
                                   # , 
                                   # objective.scale = ifelse(objective.scale == "", NA, objective.scale)
                            ) %>%
                            select(-periodregion)
) %>%
  mutate(modelshort = case_when(model=="Radiation" ~ "Rad", 
                                TRUE ~ model))




## merge 

predictions.models <- full_join(predictions.gravity, uo.preds)

### pred variables

predvars <- bind_rows(gravity.pred.vars, 
                      uo.pred.vars)



varsets <- predvars%>%
  split(., f=grepl("gravity", .$model))%>%
  (\(x){
    c(gravity = x$`TRUE`%>%
        split(., f=~modelshort+scale),
      uo = x$`FALSE`%>%
        split(., f=~model=="UO")%>%
        (\(y){
          c(UO = y$`TRUE`%>%
              split(., f=~modelshort+scale+svar+tvar+objective.scale), 
            y$`FALSE`%>%
              split(., f=~modelshort+scale+svar+tvar))
        }))%>%
      return()
  })


names(varsets)

predictions.all <- predictions.models

i=1

for(i in 1:length(varsets)){
  
  this.varset <- varsets[[i]]$var
  
  
  full <- this.varset[which(grepl("2011-2020.All US", this.varset))]
  
  
  
  byPeriod <- sub("[0-9]{4}[-][0-9]{4}[.]All US", "byPeriod.All US", full)
  all15 <- this.varset[which(grepl("2011-2015.All US", this.varset))]
  all20 <- this.varset[which(grepl("2016-2020.All US", this.varset))]
  
  predictions.all[,byPeriod] <- rowSums(predictions.all[,c(all15,all20)], na.rm = TRUE)
  
  predictions.all <- predictions.all %>% 
    select(-all_of(c(all15,all20)))
  
  
  
  
  
  byRegion <- sub("[0-9]{4}[-][0-9]{4}[.]All US", "2011-2020.byRegion", full)
  mw = this.varset[which(grepl("2011-2020.Midwest", this.varset))]
  ne = this.varset[which(grepl("2011-2020.Northeast", this.varset))]
  s = this.varset[which(grepl("2011-2020.South", this.varset))]
  w = this.varset[which(grepl("2011-2020.West", this.varset))]
  
  
  predictions.all[,byRegion] <- rowSums(predictions.all[,c(mw,ne,s,w)], na.rm = TRUE)
  
  predictions.all <- predictions.all %>% 
    select(-all_of(c(mw,ne,s,w)))
  
  
  
  
  
  
  
  byPeriodbyRegion <- sub("[0-9]{4}[-][0-9]{4}[.]All US", "byPeriod.byRegion", full)
  mw15 = this.varset[which(grepl("2011-2015.Midwest", this.varset))]
  ne15 = this.varset[which(grepl("2011-2015.Northeast", this.varset))]
  s15 = this.varset[which(grepl("2011-2015.South", this.varset))]
  w15 = this.varset[which(grepl("2011-2015.West", this.varset))]
  mw20 = this.varset[which(grepl("2016-2020.Midwest", this.varset))]
  ne20 = this.varset[which(grepl("2016-2020.Northeast", this.varset))]
  s20 = this.varset[which(grepl("2016-2020.South", this.varset))]
  w20 = this.varset[which(grepl("2016-2020.West", this.varset))]
  
  predictions.all[,byPeriodbyRegion] <- rowSums(predictions.all[,c(mw15,ne15,s15,w15,mw20,ne20,s20,w20)], na.rm = TRUE)
  
  predictions.all <- predictions.all %>% 
    select(-all_of(c(mw15,ne15,s15,w15,mw20,ne20,s20,w20)))
  
  # cat(round(i/length(varsets)*100), "\n")
  
}





predictions.all <- predictions.all %>%
  select(period, 
         `State Residence`, `County Residence`, 
         `State Work`, `County Work`, 
         fips.ij, 
         census.region.origin, 
         `Workers in Commuting Flow`, 
         POPESTIMATE.origin, POPESTIMATE.destination, pop.cats,
         distance.km, 
         sij, sij_within, 
         Total.Workers.Residence.Domestic, Total.Commuters_pred, 
         log.Workers.in.Commuting.Flow,
         log.POPESTIMATE.origin, log.POPESTIMATE.destination, 
         log.distance.km, 
         log.sij, log.sij_within, 
         log.Total.Workers.Residence.Domestic, log.Total.Commuters_pred, 
         
         matches("2011[-]2020[.]All US")&!matches("identity"), 
         matches("byPeriod[.]All US")&!matches("identity"), 
         matches("2011[-]2020[.]byRegion")&!matches("identity"), 
         matches("byPeriod[.]byRegion")&!matches("identity"), 
         
         matches("2011[-]2020[.]All US")&matches("identity"), 
         matches("byPeriod[.]All US")&matches("identity"), 
         matches("2011[-]2020[.]byRegion")&matches("identity"), 
         matches("byPeriod[.]byRegion")&matches("identity"))





gravity.pred.vars <- bind_cols(var = names(predictions.all)[which(grepl("gravity", names(predictions.all)))], 
                               names(predictions.all)[which(grepl("gravity", names(predictions.all)))] %>% 
                                 strsplit(., split = "_|[.]") %>% 
                                 lapply(., (\(x) setNames(x, nm = c("model", "period", "region", "scale")[1:length(x)]))) %>% 
                                 bind_rows() %>% 
                                 mutate(scale = ifelse(is.na(scale), "log", scale))
) %>%
  mutate(modelshort = case_when(model=="gravity" ~ "G2", 
                                TRUE ~ "G1"))


### uo
uo.pred.vars <- bind_cols(var = names(predictions.all)[which(grepl("predsuo", names(predictions.all)))], 
                          names(predictions.all)[which(grepl("predsuo", names(predictions.all)))] %>% 
                            strsplit(., split = "__") %>% 
                            lapply(., 
                                   (\(x){
                                     setNames(x, nm = c("scale", "periodregion", "model", "objective.scale", "svar", "tvar"))
                                   })) %>% 
                            bind_rows() %>% 
                            mutate(scale = ifelse(grepl("log", scale), "log", "identity"), 
                                   period = sub("^predsuo_(.+)[.].+$", "\\1", periodregion), 
                                   region = sub("^predsuo_.+[.](.+)$", "\\1", periodregion)
                                   # , 
                                   # objective.scale = ifelse(objective.scale == "", NA, objective.scale)
                            ) %>%
                            select(-periodregion)
) %>%
  mutate(modelshort = case_when(model=="Radiation" ~ "Rad", 
                                TRUE ~ model))











predvars <- uo.pred.vars %>% 
  split(., f = ~model) %>%
  (\(x) {
    id <- c(x[!grepl("UO", names(x))],
            list(UO = x[["UO"]]%>%
                   filter(objective.scale=="identity")))%>%
      as.list()%>%
      bind_rows()%>%
      split(., f=~svar+tvar)%>%
      lapply(.,
             (\(y) {
               bind_rows(y,
                         gravity.pred.vars)
             }))%>%
      bind_rows(., .id = "id_sijti") %>%
      list()
    
    
    log = c(x[!grepl("UO", names(x))],
            list(UO = x[["UO"]]%>%
                   filter(objective.scale=="log")))%>%
      as.list()%>%
      bind_rows()%>%
      split(., f=~svar+tvar)%>%
      lapply(.,
             (\(y) {
               bind_rows(y,
                         gravity.pred.vars)
             }))%>%
      bind_rows(., .id = "id_sijti") %>%
      list()
    
    
    c(id=id, 
      log=log) %>% 
      bind_rows(., 
                .id = "id_tuningscale")%>%
      
      return()
  })%>%
  select(id_tuningscale, id_sijti, scale, period, region, model, modelshort, var) %>%
  # mutate(period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  arrange(id_tuningscale, id_sijti, scale, period, region, modelshort)
  


##save
save(predvars, predictions.all, file = "./01-Data/02-Analytic-Data/predictions_flows_all.rdata")


##clean environment
rm(list=ls())
gc()







# getVarset <- function(.tuning.scale, 
#                       .svar, 
#                       .tvar,
#                       .scale, 
#                       .period, 
#                       .region, 
#                       .modelshort){
#   
#   
#   predvars %>% 
#     filter(id_tuningscale%in%.tuning.scale &
#              id_sijti%in%paste0(.svar, ".", .tvar) &
#              scale%in%.scale &
#              period%in%.period & 
#              region%in%.region &
#              modelshort%in%.modelshort) %>%
#     select(modelshort, var) %>%
#     return()
#   
# }
# 
# 
# 
# 
# 
# combos <- expand.grid(tscale = c("identity", "log"),
#                       svar = c("sij", "sij_within"), 
#                       tvar = c("Total.Workers.Residence.Domestic", "Total.Commuters_pred"), 
#                       scale = c("identity", "log"), 
#                       period = c("2011-2020", "2011-2015", "2016-2020"), 
#                       region = c(c("All US", "Midwest", "Northeast", "South", "West")), 
#                       modelshort = c("G1", "G2", "OO", "OPS", "Rad", "UO")) %>%
#   as_tibble()
# 
# combos$varset <- apply(combos, 
#                        MARGIN=1, 
#                        (\(x){
#                          getVarset(unlist(x))
#                        }))
# 
# 
# predictions.gravity[,c(1:5,6,9,12,15,18)] %>% 
#   setNames(., nm = c(names(.)[1:6], names(.)%>%
#                        .[7:10] %>% 
#                        sub("^.+_[0-9]{4}[-][0-9]{4}[.](.+)$", "\\1", .))) %>% 
#   mutate(mpr = sub("^(.+_[0-9]{4}[-][0-9]{4})[.].+$", "\\1", names(.)[6])) %>% 
#   select(!matches("All US")) %>% 
#   pivot_longer(c("Midwest", "Northeast", "South", "West"), names_to = "region", values_to = "pbr") %>% 
#   filter(!is.na(pbr)) %>% 
#   mutate(mpr = paste0(mpr, "_byRegion")) %>% 
#   select(-region) %>% 
#   pivot_wider(names_from = mpr, values_from = pbr) %>% View()
# 
# 
# ## set up combinations
# observed.period <- c("2011-2020", "2011-2015", "2016-2020")
# observed.region <- c("All US", "Midwest", "Northeast", "South", "West")
# 
# 
# observed.svar <- c("sij", "sij_within")
# observed.tvar <- c("Total.Workers.Residence.Domestic", "Total.Commuters_pred")
# observed.tuning.objective.scale <- c("identity", "log")
# 
# 
# observed.var <- c("Workers in Commuting Flow", "log.Workers.in.Commuting.Flow")
# observed.scale <- c("identity", "log")
# 
# 
# 
# 
# g12 <- gravity.pred.vars %>%
#   filter(modelshort%in%c("G1", "G2") & 
#            period == observed.period & 
#            region == observed.region & 
#            scale == observed.scale) %>%
#   select(modelshort, var)
# 
# 
# uo <- uo.pred.vars %>%
#   filter(period == observed.period & 
#            region == observed.region & 
#            scale == observed.scale &
#            svar == observed.svar &
#            tvar == observed.tvar & 
#            objective.scale %in% c("", observed.tuning.objective.scale)) %>%
#   select(modelshort, var)
# 
# 
# 
# 
# my.predvars <- bind_rows(g12, uo) %>%
#   arrange(modelshort) %>%
#   mutate(col = brewer.pal(6, "Set2"),
#          col2 = col2rgb(brewer.pal(6, "Set2"))%>%
#            t()%>%
#            as.data.frame()%>%
#            mutate(across(everything(), ~.x/255), 
#                   alpha = 0.1)%>%
#            apply(.,
#                  MARGIN=1, 
#                  (\(x) {
#                    do.call(what = "rgb", args = as.list(x))
#                  })), 
#          col3 = col2rgb(brewer.pal(6, "Set2"))%>%
#            t()%>%
#            as.data.frame()%>%
#            mutate(across(everything(), ~.x/255), 
#                   alpha = 0.3)%>%
#            apply(.,
#                  MARGIN=1, 
#                  (\(x) {
#                    do.call(what = "rgb", args = as.list(x))
#                  })), 
#          pch = c(16), 
#          lty = c(1)
#   )

