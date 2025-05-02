# gravity





internal <- 1
proportionality.constant <- 1
beta1 <- 1
beta2 <- 1
beta3 <- -1



calibrateGravity <- function(internal = 1, 
                             proportionality.constant = 1, 
                             beta1 = 1, 
                             beta2 = 1, 
                             beta3 = -1){
  
  require(dplyr)
  
  
  od.fit.rmse <- od %>%
    mutate(gravity = case_when(distance.km==0 ~ proportionality.constant + 
                                 beta1*log.POPESTIMATE.origin + 
                                 beta2*log.POPESTIMATE.destination + 
                                 internal, 
                               TRUE ~ proportionality.constant + 
                                 beta1*log.POPESTIMATE.origin + 
                                 beta2*log.POPESTIMATE.destination + 
                                 beta3*log.distance.km)) %>%
    mutate(sqerr = (log.Workers.in.Commuting.Flow-gravity)^2) %>%
    summarise(rmse = sqrt(mean(sqerr))) %>%
    select(rmse) %>%
    unlist() %>%
    return()
  
}


basic.gravity <- optim(par = c(internal = 1, 
                               proportionality.constant = 1, 
                               beta1 = 1, 
                               beta2 = 1, 
                               beta3 = -1), 
                       fn = calibrateGravity, 
                       method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[5])

basic.gravity


distance.threshold.tuning <- optim(par = c(dist.threshold = 82),
                                   fn = tune_Distance_param_SSE,
                                   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[6],
                                   lower = c(0),
                                   upper = c(1000),
                                   control = list(parscale = c(1)))






















grav.fit1 <- lm(log.Workers.in.Commuting.Flow ~ 
                  log.POPESTIMATE.origin + 
                  log.POPESTIMATE.destination + 
                  log.distance.km, 
                data = od)
# summary(grav.fit1)
# plot(grav.fit1)

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
  pivot_wider(names_from = "Parameter", values_from = "Estimate")




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












