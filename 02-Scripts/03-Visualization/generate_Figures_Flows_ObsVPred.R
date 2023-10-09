# script to plot model predictions against observed values of commuting flow. 


## load data
load("./01-Data/02-Analytic-Data/predictions_flows_all.rdata")

## packages
library(dplyr)
library(tidyr)
library(RColorBrewer)
# library(foreach)
# library(doParallel)

## clean

getVarset <- function(.tuning.scale,
                      .svar,
                      .tvar,
                      .scale,
                      .period,
                      .region,
                      .modelshort){


  predvars %>%
    filter(id_tuningscale%in%.tuning.scale &
             id_sijti%in%paste0(.svar, ".", .tvar) &
             scale%in%.scale &
             period%in%.period &
             region%in%.region &
             modelshort%in%.modelshort) %>%
    select(modelshort, var) %>%
    return()

}





combos <- expand.grid(.tuning.scale = c("id", "log"),
                      .svar = c("sij", "sij_within"),
                      .tvar = c("Total.Workers.Residence.Domestic", "Total.Commuters_pred"),
                      .scale = c("identity", "log"),
                      .period = c("2011-2020", "byPeriod"),
                      .region = c("All US", "byRegion"),
                      .modelshort = c("G1", "G2", "OO", "OPS", "Rad", "UO")) %>%
  as_tibble() %>%
  mutate(across(everything(), ~as.character(.x)))





# c("2011-2020, All US, Models", "ByPeriod, ByRegion, Models")


observed.var <- c("Workers in Commuting Flow", "log.Workers.in.Commuting.Flow")[2]


# varset <- apply(combos%>%
#                   filter(.tuning.scale=="log", 
#                          .svar=="sij", 
#                          .tvar=="Total.Workers.Residence.Domestic", 
#                          .scale=="log",
#                          .period=="2011-2020", 
#                          .region=="All US"),
#                 MARGIN=1,
#                 (\(x){
#                   do.call("getVarset", x %>% as.list())
#                 })) %>% 
#   bind_rows()


varsets <- list("flow_20112020_AllUS" = apply(combos%>%
                                                  filter(.tuning.scale=="log", 
                                                         .svar=="sij", 
                                                         .tvar=="Total.Workers.Residence.Domestic", 
                                                         .scale=="log",
                                                         .period=="2011-2020", 
                                                         .region=="All US"),
                                                MARGIN=1,
                                                (\(x){
                                                  do.call("getVarset", x %>% as.list())
                                                })) %>% 
                  bind_rows() %>%
                  list(vars=.) %>%
                  c(., 
                    list(overall.region = c("Midwest", "Northeast", "South", "West"))), 
                "flow_byPeriod_AllUS" = apply(combos%>%
                                                 filter(.tuning.scale=="log", 
                                                        .svar=="sij", 
                                                        .tvar=="Total.Workers.Residence.Domestic", 
                                                        .scale=="log",
                                                        .period=="byPeriod", 
                                                        .region=="All US"),
                                               MARGIN=1,
                                               (\(x){
                                                 do.call("getVarset", x %>% as.list())
                                               })) %>% 
                  bind_rows() %>%
                  list(vars=.) %>%
                  c(., 
                    list(overall.region = c("Midwest", "Northeast", "South", "West"))), 
                "flow_20112020_byRegion" = apply(combos%>%
                                                    filter(.tuning.scale=="log", 
                                                           .svar=="sij", 
                                                           .tvar=="Total.Workers.Residence.Domestic", 
                                                           .scale=="log",
                                                           .period=="2011-2020", 
                                                           .region=="byRegion"),
                                                  MARGIN=1,
                                                  (\(x){
                                                    do.call("getVarset", x %>% as.list())
                                                  })) %>% 
                  bind_rows() %>%
                  list(vars=.) %>%
                  c(., 
                    list(overall.region = c("Midwest", "Northeast", "South", "West"))), 
                "flow_byPeriod_byRegion" = apply(combos%>%
                                                   filter(.tuning.scale=="log", 
                                                          .svar=="sij", 
                                                          .tvar=="Total.Workers.Residence.Domestic", 
                                                          .scale=="log",
                                                          .period=="byPeriod", 
                                                          .region=="byRegion"),
                                                 MARGIN=1,
                                                 (\(x){
                                                   do.call("getVarset", x %>% as.list())
                                                 })) %>% 
                  bind_rows() %>%
                  list(vars=.) %>%
                  c(., 
                    list(overall.region = c("Midwest", "Northeast", "South", "West")))) %>%
  (\(x) {
    
    c(x,
      lapply(x, 
             (\(y) {
               lapply(c("Midwest", "Northeast", "South", "West"), 
                      (\(z) {
                        list(vars=y[["vars"]], 
                             overall.region=z)
                      }))%>%
                 c()%>%
                 setNames(., 
                          nm = c("Midwest", "Northeast", "South", "West"))
             })) %>% 
      unlist(., recursive=FALSE)%>%
        setNames(.,
                 nm = sub("[.]", "_Regional_", names(.)))
    )
    
  })


for(i in 1:length(varsets)){
  
  varset <- varsets[[i]][["vars"]]
  
  my.predvars <- varset %>%
    arrange(modelshort) %>%
    mutate(col = brewer.pal(6, "Set2"), 
           pch = c(16), 
           lty = c(1)
    )
  
  
  
  
  
  
  preddf <- predictions.all %>%
    select(all_of(c("census.region.origin", observed.var, unlist(my.predvars$var)))) %>%
    arrange(.data[[observed.var]])
  
  
  q <- c(0:200/200)
  qvec <- quantile(preddf[[observed.var]],
                   probs = q)
  qvec <- unique(qvec)
  
  # qvec <- unique(preddf[[observed.var]])
  # qvec <- qvec[order(qvec)]
  
  
  
  
  qpred0 <- preddf %>%
    mutate(obs.interval = findInterval(.data[[observed.var]], 
                                       vec = qvec, 
                                       all.inside = TRUE),
           obs.interval.range = factor(obs.interval, 
                                       levels = 1:{length(qvec)-1}, 
                                       labels = paste0(round(qvec[-length(qvec)],2),
                                                       ",",
                                                       round(qvec[-1],2))), 
           obs.nominal = (qvec[-length(qvec)] + diff(qvec)/2)[obs.interval])%>%
    filter(census.region.origin%in%varsets[[i]][["overall.region"]])
  
  
  
  
  
  png(filename = paste0("./03-Output/02-Figures/figure_flows_predVobs_", 
                        names(varsets)[i],  
                        ".png"), 
      width = 8, 
      height = 12, 
      units = "in", 
      res = 300, 
      pointsize = 10)
  
  par(mfrow=c(4,2), mar = c(1,1,1,0))
  
  
  for(this.model in 1:6){
    
    qpred <- qpred0 %>%
      group_by(obs.interval, obs.interval.range, obs.nominal) %>%
      summarise(across(all_of(c(observed.var, my.predvars$var[this.model])), 
                       ~quantile(.x, 
                                 probs = c(0:4/4)) %>% 
                         t() %>% 
                         as.data.frame(), 
                       .unpack = TRUE)) %>%
      ungroup() %>%
      arrange(.data[[paste0(observed.var,"_50%")]])
    
    
    plot(x=qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist(), 
         # x=qpred$obs.nominal,
         y=qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist(),
         type = "n",
         # xlim = c(0, max(qpred$obs.nominal)*1.1),
         xlim = c(0, sum(qvec[{length(qvec)-1}:length(qvec)])/2),
         ylim = c(0, sum(qvec[{length(qvec)-1}:length(qvec)])/2),
         # xlim = c(0, max(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))])*1.1),
         # ylim = c(0, max(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))])*1.1),
         axes = FALSE,
         xlab = "",
         ylab = "")
    title(xlab = "Observed", ylab = "Predicted", line = 0)
    box()
    
    abline(a=0,b=1,lty=5,col="grey50",lwd=1)
    
    polygon(
      c(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist(), 
        rev(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist())), 
      
      c(qpred[,grepl("25%", names(qpred)) & !grepl(observed.var, names(qpred))] %>% unlist(),
        rev(qpred[,grepl("75%", names(qpred)) & !grepl(observed.var, names(qpred))] %>% unlist())),
      
      col = adjustcolor(my.predvars$col[this.model], alpha.f = 0.75), 
      border = NA)
    
    polygon(
      c(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist(), 
        rev(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist())), 
      
      c(qpred[,grepl("_0%", names(qpred)) & !grepl(observed.var, names(qpred))] %>% unlist(),
        rev(qpred[,grepl("100%", names(qpred)) & !grepl(observed.var, names(qpred))] %>% unlist())),
      
      col = adjustcolor(my.predvars$col[this.model], alpha.f = 0.25), 
      border = NA)
    
    legend("topleft", 
           col = my.predvars$col[this.model], 
           pch = 15, 
           pt.cex = 2,
           cex = 0.8,
           legend = my.predvars$modelshort[this.model])
    
  }
  
  
  
  
  plot(x=qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist(), 
       # x=qpred$obs.nominal,
       y=qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist(),
       type = "n",
       # xlim = c(0, max(qpred$obs.nominal)*1.1),
       xlim = c(0, sum(qvec[{length(qvec)-1}:length(qvec)])/2),
       ylim = c(0, sum(qvec[{length(qvec)-1}:length(qvec)])/2),
       
       # xlim = c(0, max(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))])*1.1),
       # ylim = c(0, max(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))])*1.1),
       axes = FALSE,
       xlab = "",
       ylab = "")
  title(xlab = "Observed", ylab = "Predicted", line = 0)
  box()
  
  
  
  for(this.model in 1:6){
    
    qpred <- qpred0 %>%
      group_by(obs.interval, obs.interval.range, obs.nominal) %>%
      summarise(across(all_of(c(observed.var, my.predvars$var[this.model])), 
                       ~quantile(.x, 
                                 probs = c(0:4/4)) %>% 
                         t() %>% 
                         as.data.frame(), 
                       .unpack = TRUE)) %>%
      ungroup() %>%
      arrange(.data[[paste0(observed.var,"_50%")]])
    
    
    
    abline(a=0,b=1,lty=5,col="grey50",lwd=1)
    
    polygon(
      c(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist(), 
        rev(qpred[,grepl("50%", names(qpred)) & grepl(observed.var, names(qpred))]%>%unlist())), 
      
      c(qpred[,grepl("25%", names(qpred)) & !grepl(observed.var, names(qpred))] %>% unlist(),
        rev(qpred[,grepl("75%", names(qpred)) & !grepl(observed.var, names(qpred))] %>% unlist())),
      
      col = adjustcolor(my.predvars$col[this.model], alpha.f = 0.5), 
      border = NA)
    
    
  }
  
  
  
  plot.new()
  legend("center", 
         legend = c(as.expression(bquote("G"[1])), 
                    as.expression(bquote("G"[2])), 
                    "OO", "OPS", "Rad", "UO"), 
         pch = 15,
         pt.cex = 4,
         col = my.predvars$col, 
         cex = 0.8,
         title = "Model", 
         title.cex = 0.9,
         bty = "n", 
         y.intersp = 2, 
         x.intersp = 2)
  
  dev.off()
  
  
  
  
}



## save

## clean environment
rm(list=ls())
gc()