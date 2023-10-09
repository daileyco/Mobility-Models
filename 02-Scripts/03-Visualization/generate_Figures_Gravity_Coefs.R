# script to generate figures of gravity model coefficients

## load data
load("./01-Data/02-Analytic-Data/estimates_gravity_model.rdata")


## packages
library(dplyr)


## helper functions
source("./02-Scripts/02-Helper-Functions/plotCoefs.R")


## split data by base parameter, 
### nest split by period, 
#### nest split by origin pop cat and destination pop cat
gp.list <- split(gravity.powers, ~base) %>%
  lapply(., split, f=~period) %>%
  lapply(., 
         function(x){
           lapply(x, split, f=~pop.cat.origin+pop.cat.destination)
         })


## plot origin pop coefs

for(i in 1:length(gp.list$`Population Size, Origin`)){
  
  name.string <- names(gp.list$`Population Size, Origin`)[i]
  
  png(filename = paste0("./03-Output/02-Figures/figure_gravity_originpoppower_", name.string, ".png"), units = "in", height = 16, width = 16, res = 300, pointsize = 10)
  
  par(mfcol = c(3,3))
  
  for(j in 1:length(gp.list$`Population Size, Origin`[[i]])){
    
    coef.df <- gp.list$`Population Size, Origin`[[i]][[j]]
    
    plotCoefs(coef.df = coef.df, 
              xlims = c(-0.25,1), 
              left = {unique(coef.df$pop.cat.destination)=="Small"}, 
              top = {unique(coef.df$pop.cat.origin)=="Small"}, 
              leftmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Small" & unique(coef.df$pop.cat.origin)=="Medium", 
                                  "Origin Population Size Category", 
                                  NA), 
              topmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Medium" & unique(coef.df$pop.cat.origin)=="Small", 
                                 "Destination Population Size Category", 
                                 NA))
    
  }
  
  dev.off()
  
}




## plot destination pop coefs

for(i in 1:length(gp.list$`Population Size, Destination`)){
  
  name.string <- names(gp.list$`Population Size, Destination`)[i]
  
  png(filename = paste0("./03-Output/02-Figures/figure_gravity_destpoppower_", name.string, ".png"), units = "in", height = 16, width = 16, res = 300, pointsize = 10)
  
  par(mfcol = c(3,3))
  
  for(j in 1:length(gp.list$`Population Size, Destination`[[i]])){
    
    coef.df <- gp.list$`Population Size, Destination`[[i]][[j]]
    
    plotCoefs(coef.df = coef.df, 
              xlims = c(-0.25,1), 
              left = {unique(coef.df$pop.cat.destination)=="Small"}, 
              top = {unique(coef.df$pop.cat.origin)=="Small"}, 
              leftmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Small" & unique(coef.df$pop.cat.origin)=="Medium", 
                                  "Origin Population Size Category", 
                                  NA), 
              topmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Medium" & unique(coef.df$pop.cat.origin)=="Small", 
                                 "Destination Population Size Category", 
                                 NA))
    
  }
  
  dev.off()
  
}






## plot destination pop coefs

for(i in 1:length(gp.list$`Distance (km)`)){
  
  name.string <- names(gp.list$`Distance (km)`)[i]
  
  png(filename = paste0("./03-Output/02-Figures/figure_gravity_distancepower_", name.string, ".png"), units = "in", height = 16, width = 16, res = 300, pointsize = 10)
  
  par(mfcol = c(3,3))
  
  for(j in 1:length(gp.list$`Distance (km)`[[i]])){
    
    coef.df <- gp.list$`Distance (km)`[[i]][[j]]
    
    plotCoefs(coef.df = coef.df, 
              xlims = c(-4,0.25), 
              left = {unique(coef.df$pop.cat.destination)=="Small"}, 
              top = {unique(coef.df$pop.cat.origin)=="Small"}, 
              leftmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Small" & unique(coef.df$pop.cat.origin)=="Medium", 
                                  "Origin Population Size Category", 
                                  NA), 
              topmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Medium" & unique(coef.df$pop.cat.origin)=="Small", 
                                 "Destination Population Size Category", 
                                 NA))
    
  }
  
  dev.off()
  
}



## save



## clean environment
rm(list=ls())
gc()

