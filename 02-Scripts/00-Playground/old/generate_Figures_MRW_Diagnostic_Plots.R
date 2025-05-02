# script to generate figures for diagnostics of Metropolis random walk

## load


## packages



## get filepaths for saved MRW dataframes
mrw.dfs <- list.files("./01-Data/02-Analytic-Data", 
                      pattern = "mrw", 
                      full.names = TRUE)

### extract suffixes from names
mrw.ids <- sub("^.+[/]mrw_(.*)[.]rds$", 
               "\\1", 
               mrw.dfs)



## loop through files
### change names of saved figures

for(ii in 1:length(mrw.dfs)){
  
  ## load data
  load(mrw.dfs[ii])
  
  
  ## Trace plots
  
  ### RMSE against MCMC run index
  
  # svg(filename = paste0("./03-Output/02-Figures/trace_rmse_", mrw.ids[ii],".svg"), width = 8, height = 4.5, pointsize = 10)
  png(filename = paste0("./03-Output/02-Figures/trace_rmse_", mrw.ids[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
  
  par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
  plot(chains.df$rmse, type = "l", 
       ylab="RMSE", 
       xlab="MC Index",
       main="Root Mean Square Error Trace")
  points(y=chains.df$rmse[which(chains.df$rmse==chains.df$rmse[which.min(chains.df$rmse)])], 
         x=which(chains.df$rmse==chains.df$rmse[which.min(chains.df$rmse)]), 
         pch = 16, 
         col = "red")
  legend("topright", legend=c(paste0("Minimum = ", round(chains.df$rmse[which.min(chains.df$rmse)],2))), col=c("red"), pch=16)
  
  dev.off()
  
  
  
  
  ### Parameter estimates against MCMC run index
  
  
  # svg(filename = paste0("./03-Output/02-Figures/trace_parameter_estimates_", mrw.ids[ii],".svg"), width = 16, height = 16, pointsize = 10)
  png(filename = paste0("./03-Output/02-Figures/trace_parameter_estimates_", mrw.ids[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
  
  par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,1.1))
  
  for(i in 1:16){
    plot(unlist(chains.df[,i]), type = "l", main = names(chains.df)[i])
  }
  
  dev.off()
  
  
  
  ### Alpha and acceptance
  
  
  # svg(filename = paste0("./03-Output/02-Figures/trace_alpha_", mrw.ids[ii],".svg"), width = 8, height = 4.5, pointsize = 10)
  png(filename = paste0("./03-Output/02-Figures/trace_alpha_", mrw.ids[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
  
  par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
  {
    # par(mfrow=c(2,1))
    plot(chains.df$alpha, 
         type="l", 
         main = paste0("Alpha Trace\n", paste0("Acceptance = ", round(sum(chains.df$accept[2:length(chains.df$accept)])/nrow(chains.df)*100,1), "%")), 
         ylab = "Alpha", 
         xlab = "Index")
    # plot.new()
    # text(0.5, 0.5, paste("Acceptance Proportion = ", sum(chains.df$accept[2:length(chains.df$accept)])/nrow(chains.df)))
  }#~20-30%
  
  dev.off()
  
  
  
  
  
  
  ## Histograms
  
  
  # svg(filename = paste0("./03-Output/02-Figures/histograms_parameter_estimates_", mrw.ids[ii],".svg"), width = 16, height = 16, pointsize = 10)
  png(filename = paste0("./03-Output/02-Figures/histograms_parameter_estimates_", mrw.ids[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
  
  par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,0))
  
  for(i in 1:16){
    hist(unlist(chains.df[,i]), main = names(chains.df)[i])
  }
  
  
  dev.off()
  
  
  
  
  ## Running Means
  
  # svg(filename = paste0("./03-Output/02-Figures/runningmeans_parameter_estimates_", mrw.ids[ii],".svg"), width = 16, height = 16, pointsize = 10)
  png(filename = paste0("./03-Output/02-Figures/runningmeans_parameter_estimates_", mrw.ids[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
  
  par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,0))
  
  for(i in 1:16){
    this.running.mean <- cumsum(unlist(chains.df[,i]))/1:nrow(chains.df)
    
    plot(this.running.mean, type="l", main = names(chains.df)[i])
  }
  
  dev.off()
  
  
  
  ## Autocorrelation
  
  
  # svg(filename = paste0("./03-Output/02-Figures/autocorrelation_parameter_estimates_", mrw.ids[ii],".svg"), width = 16, height = 16, pointsize = 10)
  
  png(filename = paste0("./03-Output/02-Figures/autocorrelation_parameter_estimates_", mrw.ids[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
  
  
  par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,0))
  
  for(i in 1:16){
    acf(this.running.mean, main = "")
    title(main = names(chains.df)[i], line = 0.5)
  }
  
  dev.off()
  
  
  
}



## clean environment
rm(list=ls())
gc()





### old single df script

# ## load data
# load("./01-Data/02-Analytic-Data/mrw.rds")
# 
# 
# ## packages
# library(dplyr)
# library(zoo)
# 
# 
# 
# ## Trace plots
# 
# ### RMSE against MCMC run index
# 
# svg(filename = "./03-Output/02-Figures/trace_rmse.svg", width = 8, height = 4.5, pointsize = 10)
# 
# plot(chains.df$rmse, type = "l", 
#      ylab="RMSE", 
#      main="Root Mean Square Error Trace across Metropolis Random Walk")
# points(y=chains.df$rmse[which(chains.df$rmse==chains.df$rmse[which.min(chains.df$rmse)])], 
#        x=which(chains.df$rmse==chains.df$rmse[which.min(chains.df$rmse)]), 
#        pch = 16, 
#        col = "red")
# legend("topright", legend=c("Minimum"), col=c("red"), pch=16)
# 
# dev.off()
# 
# 
# 
# 
# ### Parameter estimates against MCMC run index
# 
# 
# svg(filename = "./03-Output/02-Figures/trace_parameter_estimates.svg", width = 16, height = 16, pointsize = 10)
# 
# par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,0))
# 
# for(i in 1:16){
#   plot(unlist(chains.df[,i]), type = "l", main = names(chains.df)[i])
# }
# 
# dev.off()
# 
# 
# 
# ### Alpha and acceptance
# 
# 
# svg(filename = "./03-Output/02-Figures/trace_alpha.svg", width = 8, height = 4.5, pointsize = 10)
# 
# par(mar=c(5.1,4.1,4.1,2.1))
# {
#   # par(mfrow=c(2,1))
#   plot(chains.df$alpha, 
#        type="l", 
#        main = paste0("Alpha Trace\n", paste0("Acceptance =", round(sum(chains.df$accept[2:length(chains.df$accept)])/nrow(chains.df)*100,1), "%")), 
#        ylab = "Alpha", 
#        xlab = "Index")
#   # plot.new()
#   # text(0.5, 0.5, paste("Acceptance Proportion = ", sum(chains.df$accept[2:length(chains.df$accept)])/nrow(chains.df)))
# }#~20-30%
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# ## Histograms
# 
# 
# svg(filename = "./03-Output/02-Figures/histograms_parameter_estimates.svg", width = 16, height = 16, pointsize = 10)
# 
# par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,0))
# 
# for(i in 1:16){
#   hist(unlist(chains.df[,i]), main = names(chains.df)[i])
# }
# 
# 
# dev.off()
# 
# 
# 
# 
# ## Running Means
# 
# svg(filename = "./03-Output/02-Figures/runningmeans_parameter_estimates.svg", width = 16, height = 16, pointsize = 10)
# 
# par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,0))
# 
# for(i in 1:16){
#   this.running.mean <- cumsum(unlist(chains.df[,i]))/1:nrow(chains.df)
#   
#   plot(this.running.mean, type="l", main = names(chains.df)[i])
# }
# 
# dev.off()
# 
# 
# 
# ## Autocorrelation
# 
# 
# svg(filename = "./03-Output/02-Figures/autocorrelation_parameter_estimates.svg", width = 16, height = 16, pointsize = 10)
# 
# par(mfrow=c(4,4), mar=c(2.1,2.1,2.1,0))
# 
# for(i in 1:16){
#   acf(this.running.mean, main = names(chains.df)[i])
# }
# 
# dev.off()




