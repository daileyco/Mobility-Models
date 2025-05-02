# Gravity fit to commuter flows



## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)
library(tibble)
library(tidyr)
library(sf)


## helper function

source("./02-Scripts/02-Helper-Functions/RMSE.R")


od.flow.logistic <- od %>%
  mutate(flow.proportion = log.Workers.in.Commuting.Flow / log.Total.Workers.Residence) %>%
  mutate(flow.logistic = flow.proportion / (1-flow.proportion)) %>%
  mutate(flow.logistic = ifelse(is.infinite(flow.logistic), max(flow.logistic[which(!is.infinite(flow.logistic))]), flow.logistic))




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


fit1 <- lm(log.Workers.in.Commuting.Flow ~ 
             indicator.long.distance
           + log.POPESTIMATE.origin_short 
           + log.POPESTIMATE.destination_short
           + log.distance.km_short
           + log.POPESTIMATE.origin_long 
           + log.POPESTIMATE.destination_long
           + log.distance.km_long
           , 
           data = od)

fit2 <- lm(log.Workers.in.Commuting.Flow ~ 
             indicator.long.distance*log.POPESTIMATE.origin
                + indicator.long.distance*log.POPESTIMATE.destination
                + indicator.long.distance*log.distance.km, 
                data = od)



## fit basic gravity model
grav.fit1 <- lm(log.Workers.in.Commuting.Flow ~ 
                  log.POPESTIMATE.origin 
                + log.POPESTIMATE.destination
                + log.distance.km
                
                + indicator.long.distance*log.POPESTIMATE.origin
                + indicator.long.distance*log.POPESTIMATE.destination
                + indicator.long.distance*log.distance.km
                
                + p.cats*log.POPESTIMATE.origin
                + p.cats*log.POPESTIMATE.destination
                + p.cats*log.distance.km
                
                + p.cats*indicator.long.distance*log.POPESTIMATE.origin
                + p.cats*indicator.long.distance*log.POPESTIMATE.destination
                + p.cats*indicator.long.distance*log.distance.km, 
                data = od)

grav.fit1 <- lm(log.Workers.in.Commuting.Flow ~ 
                  p.cats*indicator.long.distance*log.POPESTIMATE.origin
                + p.cats*indicator.long.distance*log.POPESTIMATE.destination
                + p.cats*indicator.long.distance*log.distance.km, 
                data = od)
summary(grav.fit1)




grav.fit1.simple.slopes.po <- simple_slopes(grav.fit1, 
                                         levels = list(log.POPESTIMATE.origin = "sstest", 
                                                       indicator.long.distance = c(0,1), 
                                                       p.cats = paste0(rep(c("s", "m", "l"), each=3), 
                                                                       c("s", "m", "l"))), 
                                         confint = TRUE)
grav.fit1.simple.slopes.pd <- simple_slopes(grav.fit1, 
                                         levels = list(log.POPESTIMATE.destination = "sstest", 
                                                       indicator.long.distance = c(0,1), 
                                                       p.cats = paste0(rep(c("s", "m", "l"), each=3), 
                                                                       c("s", "m", "l"))), 
                                         confint = TRUE)
grav.fit1.simple.slopes.d <- simple_slopes(grav.fit1, 
                                         levels = list(log.distance.km = "sstest", 
                                                       indicator.long.distance = c(0,1), 
                                                       p.cats = paste0(rep(c("s", "m", "l"), each=3), 
                                                                       c("s", "m", "l"))), 
                                         confint = TRUE)






## fit basic gravity model
grav.fit1 <- lm(log.Workers.in.Commuting.Flow ~ 
                  log.POPESTIMATE.origin + 
                  log.POPESTIMATE.destination + 
                  log.distance.km, 
                data = od)
# summary(grav.fit1)
# plot(grav.fit1)

### extract parameter estimates and fit stats
ests.fit1 <- cbind(coef(grav.fit1), confint(grav.fit1)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "AIC", Estimate = as.character(AIC(grav.fit1))) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "BIC", Estimate = as.character(BIC(grav.fit1))) %>%
  pivot_wider(names_from = "Parameter", values_from = "Estimate") %>% 
  bind_cols(., 
            tibble(`Internal flows included?` = "Yes"), 
            data.frame("RMSE" = RMSE(od$log.Workers.in.Commuting.Flow,
                                     predict(grav.fit1, 
                                             od, 
                                             type = "response"))))

### without internal flows
grav.fit2 <- lm(log.Workers.in.Commuting.Flow ~ 
                  log.POPESTIMATE.origin + 
                  log.POPESTIMATE.destination + 
                  log.distance.km, 
                data = od %>%
                  filter(distance.km!=0))
# summary(grav.fit1)
# plot(grav.fit1)

### extract parameter estimates and fit stats
ests.fit2 <- cbind(coef(grav.fit2), confint(grav.fit2)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "AIC", Estimate = as.character(AIC(grav.fit2))) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "BIC", Estimate = as.character(BIC(grav.fit2))) %>%
  pivot_wider(names_from = "Parameter", values_from = "Estimate") %>% 
  bind_cols(., 
            tibble(`Internal flows included?` = "No"), 
            data.frame("RMSE" = RMSE(od %>% 
                                       filter(distance.km!=0) %>%
                                       select(log.Workers.in.Commuting.Flow) %>%
                                       unlist(),
                                     predict(grav.fit2, 
                                             od %>%
                                               filter(distance.km!=0), 
                                             type = "response"))))








## gravity with distance threshold interaction variable



calibrateGravity <- function(threshold.distance){
  
  require(dplyr)
  
  od.fit <- od %>%
    mutate(ind.threshold.distance = ifelse(distance.km<threshold.distance, 0, 1))
  
  
  fit.lm <- lm(log.Workers.in.Commuting.Flow ~ 
                 log.POPESTIMATE.origin*ind.threshold.distance + 
                 log.POPESTIMATE.destination*ind.threshold.distance + 
                 log.distance.km*ind.threshold.distance, 
               data = od.fit)
  
  
  RMSE(od.fit$log.Workers.in.Commuting.Flow, 
       predict(fit.lm, 
               od.fit, 
               type = "response")) %>%
    return()
  
}

proposeValue <- function(par){
  rnorm(1, mean = par, sd = 10)
}



gravity.calibration <- optim(par = c(threshold.distance = 119), 
                             fn = calibrateGravity, 
                             method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[4], 
                             lower = c(min(od$distance.km)), 
                             upper = c(max(od$distance.km)), 
                             control = list(parscale = c(10)))



basic.gravity <- optim(par = c(internal = 1, 
                               proportionality.constant = 1, 
                               beta1 = 1, 
                               beta2 = 1, 
                               beta3 = -1), 
                       fn = calibrateGravity, 
                       method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[5])










grav.fit2 <- lm(log.Workers.in.Commuting.Flow ~ 
                  log.POPESTIMATE.origin + 
                  log.POPESTIMATE.destination + 
                  log.distance.km, 
                data = od %>%
                  filter(distance.km!=0))
# summary(grav.fit1)
# plot(grav.fit1)

### extract parameter estimates and fit stats
ests.fit2 <- cbind(coef(grav.fit2), confint(grav.fit2)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "AIC", Estimate = as.character(AIC(grav.fit2))) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "BIC", Estimate = as.character(BIC(grav.fit2))) %>%
  pivot_wider(names_from = "Parameter", values_from = "Estimate") %>% 
  bind_cols(., 
            tibble(`Internal flows included?` = "No"), 
            data.frame("RMSE" = RMSE(od %>% 
                                       filter(distance.km!=0) %>%
                                       select(log.Workers.in.Commuting.Flow) %>%
                                       unlist(),
                                     predict(grav.fit2, 
                                             od %>%
                                               filter(distance.km!=0), 
                                             type = "response"))))
















basic.gravity


distance.threshold.tuning <- optim(par = c(dist.threshold = 82),
                                   fn = tune_Distance_param_SSE,
                                   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[6],
                                   lower = c(0),
                                   upper = c(1000),
                                   control = list(parscale = c(1)))

























grav.fit2 <- lm(log.Workers.in.Commuting.Flow ~ 
                  log.POPESTIMATE.origin*ind.nonzero.dist + 
                  log.POPESTIMATE.destination*ind.nonzero.dist + 
                  log.distance.km*ind.nonzero.dist, 
                data = od)
# summary(grav.fit1)
# plot(grav.fit1)

ests.fit2 <- cbind(coef(grav.fit2), confint(grav.fit2)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "AIC", Estimate = as.character(AIC(grav.fit2))) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "BIC", Estimate = as.character(BIC(grav.fit2))) %>%
  pivot_wider(names_from = "Parameter", values_from = "Estimate")







grav.fit3 <- lm(log.Workers.in.Commuting.Flow ~ 
                  log.Total.Workers.Work + 
                  log.POPESTIMATE.origin + 
                  log.POPESTIMATE.destination + 
                  log.distance.km, 
                data = od)
# summary(grav.fit1)
# plot(grav.fit1)

ests.fit3 <- cbind(coef(grav.fit3), confint(grav.fit3)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "AIC", Estimate = as.character(AIC(grav.fit3))) %>%
  add_row(., 
          .after = nrow(.), 
          Parameter = "BIC", Estimate = as.character(BIC(grav.fit3))) %>%
  pivot_wider(names_from = "Parameter", values_from = "Estimate")






bind_rows(mget(ls(pattern = "^ests[.]fit[0-9]$")), .id = "model") %>% View()




tune_Distance_param_SSE <- function(thedistance){
  
  thisdata <- thedata_flows %>%
    mutate(dist.threshold=ifelse(dist.origin2dest_km<thedistance, 1,0))
  
  fit1 <- lm(flow.observed_log ~ mass.origin_log*dist.threshold + mass.dest_log*dist.threshold + log(dist.origin2dest_km)*dist.threshold, data = thisdata)
  
  sse <- sum((thisdata$flow.observed_log-predict(fit1))^2)
  
}



distance.threshold.tuning <- optim(par = c(dist.threshold = 82),
                                   fn = tune_Distance_param_SSE,
                                   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[6],
                                   lower = c(0),
                                   upper = c(1000),
                                   control = list(parscale = c(1)))



thedata_flows <- thedata_flows %>%
  mutate(dist.threshold=ifelse(dist.origin2dest_km<distance.threshold.tuning$par, 1,0))


fit1 <- lm(flow.observed_log ~ mass.origin_log*dist.threshold + mass.dest_log*dist.threshold + log(dist.origin2dest_km)*dist.threshold, data = thedata_flows)






all_flows <- expand.grid(origin = unique(c(thedata_flows$fp.from, thedata_flows$fp.to)), 
                         dest = unique(c(thedata_flows$fp.from, thedata_flows$fp.to))) %>%
  full_join(., 
            thedata_pop%>%setNames(., nm=paste0("origin.", names(.))), 
            by = c("origin" = "origin.fips")) %>%
  full_join(., 
            thedata_pop%>%setNames(., nm=paste0("dest.", names(.))), 
            by = c("dest" = "dest.fips")) 

all_flows$origin2dest <- distHaversine(all_flows[,c("origin.X", "origin.Y")], 
                                       all_flows[,c("dest.X", "dest.Y")])

all_flows <- all_flows %>%
  mutate(origin2dest_km = origin2dest/1000, 
         dist.threshold=ifelse(origin2dest_km<distance.threshold.tuning$par, 1,0), 
         mass.origin_log = log(origin.POPULATION), 
         mass.dest_log = log(dest.POPULATION)) %>%
  select(from = origin.STNAME, 
         to = dest.STNAME, 
         mass.origin_log, 
         mass.dest_log,
         dist.origin2dest_km = origin2dest_km, 
         dist.threshold
         )



all_flows$gravity_pred_log <- predict(fit1, newdata = all_flows)

all_flows_stateg <- all_flows %>% 
  filter(complete.cases(.) & !is.infinite(gravity_pred_log)) %>%
  mutate(gravity_pred = exp(gravity_pred_log)) %>%
  group_by(from, to) %>%
  summarise(weight = sum(gravity_pred)) %>%
  ungroup()

# summary(fit1)
# 
# 
# thedata_flows$gravity_pred_log <- predict(fit1)
# thedata_flows$gravity_pred <- exp(thedata_flows$gravity_pred_log)
# 
# # sum frequencies to state level
# thedata_flows_state <- thedata_flows %>% 
#   group_by(State.Residence, State.Work) %>% 
#   summarise(weight = sum(gravity_pred)) %>% 
#   ungroup() %>%
#   select(from = State.Residence, to = State.Work, weight)
# 





# set up network
## attach lat/lon coordinates to network vertices
### use centers of population
vlist <- cbind(pop.centers.state, st_coordinates(pop.centers.state)) %>% 
  as.data.frame() %>% 
  select(State = STNAME, x=X, y=Y, pop = POPULATION) 
# %>%
#   filter(State%in%unlist(state.combos[,c("from", "to")]))
### alternatively, spatial polygon centroid
# library(geosphere)
# centroids <- st_as_sf(as.data.frame(centroid(as_Spatial(us.shape.state.contig))), coords = c(1,2)) %>%
#   cbind(., State = us.shape.state.contig$NAME, st_coordinates(.))
# vlist <- left_join(vlist, centroids, by = "State") %>% select(State, x=X, y=Y, pop)

# gravity.state.network <- graph_from_data_frame(thedata_flows_state,
#                                                directed = TRUE, 
#                                                vertices = vlist)
gravity.state.network <- graph_from_data_frame(all_flows_stateg,
                                               directed = TRUE, 
                                               vertices = vlist)
# flows.state.graph <- subgraph(flows.state.graph, vids = which(!names(V(flows.state.graph))%in%c("Alaska", "Hawaii", "Puerto Rico")))


save(all_flows_stateg, gravity.state.network, file = "./01-Data/02-Analytic-Data/acs_state_gravity_network.rdata")


rm(list = ls())
gc()
























# sum frequencies to state level
acs.state <- acs %>% 
  group_by(State.Residence, State.Work) %>% 
  summarise(Workers.in.Commuting.Flow = sum(Workers.in.Commuting.Flow)) %>% 
  ungroup() %>%
  select(from = State.Residence, to = State.Work, weight = Workers.in.Commuting.Flow)


# compute_Gravity <- function(theparams, theoriginmassvar="mass.origin", thedestmassvar="mass.dest", thedistvar="dist.origin2dest_km", thedata, thescale = "id"){
#   
#   prop.constant <- theparams[1]
#   origin.exp <- theparams[2]
#   dest.exp <- theparams[3]
#   
#   if(length(theparams)<4){
#     dist.threshold <- 1
#   }else{
#     dist.threshold <- theparams[4]
#   }
#   
#   mass.origin <- thedata[theoriginmassvar]
#   mass.dest <- thedata[thedestmassvar]
#   dist.origin2dest <- thedata[thedistvar]
# 
#   if(thescale=="id"){
#     gravity <- prop.constant*((mass.origin^origin.exp)*(mass.dest^dest.exp))/(exp(dist.origin2dest/dist.threshold))
#   }
#   if(thescale=="log"){
#     gravity <- log(mass.origin)*origin.exp + log(mass.dest)*dest.exp - dist.origin2dest/dist.threshold + log(prop.constant)
#   }
#   
#   return(gravity)
# }
# 
# 
# compute_Gravity_SSE <- function(theparams, theobservedvar="flow.observed", thedata, thescale = "id"){
#   
#   theobserved <- thedata[theobservedvar]
#   theexpected <- compute_Gravity(theparams=theparams, thedata=thedata, thescale=thescale)
#   
#   sse <- sum((theobserved-theexpected)^2)
#   
#   return(sse)
# }





# # State Level
# 
# thedata_pop <- cbind(st_drop_geometry(pop.centers.state),
#                      st_coordinates(st_geometry(pop.centers.state$geometry)))
# 
# 
# thedata_flows <- left_join(acs.state, 
#                            thedata_pop, 
#                            by=c("from"="STNAME"))%>%
#   select(mass.origin = POPULATION, 
#          x.origin = X, 
#          y.origin = Y, 
#          everything())%>%
#   left_join(., 
#             thedata_pop, 
#             by = c("to"="STNAME"))%>%
#   select(mass.dest = POPULATION, 
#          x.dest = X, 
#          y.dest = Y, 
#          flow.observed = weight,
#          everything())
# 
# 
# 
# thedata_flows$dist.origin2dest <- distHaversine(thedata_flows[,c("x.origin", "y.origin")], 
#                                                 thedata_flows[,c("x.dest", "y.dest")])
# 
# thedata_flows <- thedata_flows %>% 
#   filter(dist.origin2dest>0)
# 
# 
#   
#   
# 
# 
# optim(par = c(origin.exp = 0.46,
#               dest.exp = 0.64,
#               dist.threshold = 82000), 
#       fn = compute_Gravity_SSE, 
#       thedata = thedata_flows[which(thedata_flows$dist.origin2dest<=300000),],
#       method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[1],
#       lower = c(-Inf, -Inf), 
#       upper = c(Inf, Inf), 
#       control = list(parscale = c(0.05,0.05,2000)))
# 
# optim(par = c(origin.exp = 0.35,
#               dest.exp = 0.37), 
#       fn = compute_Gravity_SSE, 
#       thedata = thedata_flows[which(thedata_flows$dist.origin2dest>300000),],
#       method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[4],
#       lower = c(0, 0), 
#       upper = c(Inf, Inf),
#       control = list(parscale = c(0.05,0.05)))














