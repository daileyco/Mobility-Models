# script to generate figures of gravity model coefficients

## load data
load("./01-Data/02-Analytic-Data/estimates_gravity_model_simple.rdata")


## packages
library(dplyr)


## helper functions
source("./02-Scripts/02-Helper-Functions/plotCoefs_simple.R")


## split data by period, 
### nest split by base parameter, 
#### nest split by large populations pairings or not
gp.list <- split(gravity.powers.simple, ~period) %>%
  lapply(., split, f=~base)



for(i in 1:length(gp.list)){
  
  
  filename.string <- paste0("./03-Output/02-Figures/figure_gravitysimple_powers_", names(gp.list)[i], ".png")
  
  png(filename = filename.string, bg = "white", units = "in", height = 13, width = 8,5, res = 300, pointsize = 10, family = "sans")
  
  par(mfrow = c(3,1))
  
  ## plot origin pop coefs
  
  
  coef.df <- gp.list[[i]]$`Population Size, Origin`
  
  plotCoefs(coef.df = coef.df, 
            xlims = c(0,1))
  
  axis(1, at = mean(c(0,1)), labels = "Origin Population Power Parameter", line = 0.75, tick = FALSE)
  # axis(3, at = mean(c(-0.025,0.55)), labels = "Not Large Population Pairings", line = 0, tick = FALSE)
  # 
  # axis(1, at = par('usr')[2]/par('plt')[2], labels = "Origin Population Power Parameter", line = 1, tick = FALSE, xpd = TRUE)
  
  
  
  legend("topleft", 
         cex = 9/10, 
         # inset = -0.05,
         xpd = TRUE,
         # ncol = 2,
         lty = c(3, 1, NA, rep(NA,5), NA, rep(NA,2)), 
         pt.bg = c(NA, NA, NA, c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), NA, rep("grey80", 2)),
         col = c("black", "black", NA, rep(NA,5), NA, rep("black", 2)), 
         pch = c(0, 15, NA, rep(22,5), NA, 23,21), 
         pt.cex = c(1, 1, NA, rep(1.75,5), NA, rep(1.25,2)),
         legend = c("Short Distance", "Long Distance", 
                    "", 
                    "All US", "Midwest", "Northeast", "South", "West", 
                    "", 
                    "Flows between Large Populations", "All Other Flows"), 
         bty = "o")
  
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)
  
  
  
  
  
  ## plot destination pop coefs
  
  coef.df <- gp.list[[i]]$`Population Size, Destination`
  
  plotCoefs(coef.df = coef.df, 
            xlims = c(0,1))
  
  axis(1, at = mean(c(0,1)), labels = "Destination Population Power Parameter", line = 0.75, tick = FALSE)
  
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)
  
  
  
  ## plot distance coefs
  
  coef.df <- gp.list[[i]]$`Distance (km)`
  
  plotCoefs(coef.df = coef.df, 
            xlims = c(-3.5,0))
  
  axis(1, at = mean(c(-3.5,0)), labels = "Distance Power Parameter", line = 0.75, tick = FALSE)
  
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "C", adj = c(0,1), xpd = T, cex = 1, font = 2)
  
  
  dev.off()
  
}




## save



## clean environment
rm(list=ls())
gc()









# #old
# 
# 
# for(i in 1:length(gp.list$`Population Size, Origin`)){
#   
#   name.string <- names(gp.list$`Population Size, Origin`)[i]
#   
#   png(filename = paste0("./03-Output/02-Figures/gravity_originpoppower_", name.string, ".png"), units = "in", height = 16, width = 16, res = 300, pointsize = 10)
#   
#   par(mfcol = c(3,3))
#   
#   for(j in 1:length(gp.list$`Population Size, Origin`[[i]])){
#     
#     coef.df <- gp.list$`Population Size, Origin`[[i]][[j]]
#     
#     plotCoefs(coef.df = coef.df, 
#               xlims = c(-0.25,1), 
#               left = {unique(coef.df$pop.cat.destination)=="Small"}, 
#               top = {unique(coef.df$pop.cat.origin)=="Small"}, 
#               leftmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Small" & unique(coef.df$pop.cat.origin)=="Medium", 
#                                   "Origin Population Size Category", 
#                                   NA), 
#               topmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Medium" & unique(coef.df$pop.cat.origin)=="Small", 
#                                  "Destination Population Size Category", 
#                                  NA))
#     
#   }
#   
#   dev.off()
#   
# }
# 
# 
# 
# 
# ## plot destination pop coefs
# 
# for(i in 1:length(gp.list$`Population Size, Destination`)){
#   
#   name.string <- names(gp.list$`Population Size, Destination`)[i]
#   
#   png(filename = paste0("./03-Output/02-Figures/gravity_destpoppower_", name.string, ".png"), units = "in", height = 16, width = 16, res = 300, pointsize = 10)
#   
#   par(mfcol = c(3,3))
#   
#   for(j in 1:length(gp.list$`Population Size, Destination`[[i]])){
#     
#     coef.df <- gp.list$`Population Size, Destination`[[i]][[j]]
#     
#     plotCoefs(coef.df = coef.df, 
#               xlims = c(-0.25,1), 
#               left = {unique(coef.df$pop.cat.destination)=="Small"}, 
#               top = {unique(coef.df$pop.cat.origin)=="Small"}, 
#               leftmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Small" & unique(coef.df$pop.cat.origin)=="Medium", 
#                                   "Origin Population Size Category", 
#                                   NA), 
#               topmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Medium" & unique(coef.df$pop.cat.origin)=="Small", 
#                                  "Destination Population Size Category", 
#                                  NA))
#     
#   }
#   
#   dev.off()
#   
# }
# 
# 
# 
# 
# 
# 
# ## plot destination pop coefs
# 
# for(i in 1:length(gp.list$`Distance (km)`)){
#   
#   name.string <- names(gp.list$`Distance (km)`)[i]
#   
#   png(filename = paste0("./03-Output/02-Figures/gravity_distancepower_", name.string, ".png"), units = "in", height = 16, width = 16, res = 300, pointsize = 10)
#   
#   par(mfcol = c(3,3))
#   
#   for(j in 1:length(gp.list$`Distance (km)`[[i]])){
#     
#     coef.df <- gp.list$`Distance (km)`[[i]][[j]]
#     
#     plotCoefs(coef.df = coef.df, 
#               xlims = c(-4,0.25), 
#               left = {unique(coef.df$pop.cat.destination)=="Small"}, 
#               top = {unique(coef.df$pop.cat.origin)=="Small"}, 
#               leftmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Small" & unique(coef.df$pop.cat.origin)=="Medium", 
#                                   "Origin Population Size Category", 
#                                   NA), 
#               topmiddle = ifelse(unique(coef.df$pop.cat.destination)=="Medium" & unique(coef.df$pop.cat.origin)=="Small", 
#                                  "Destination Population Size Category", 
#                                  NA))
#     
#   }
#   
#   dev.off()
#   
# }


