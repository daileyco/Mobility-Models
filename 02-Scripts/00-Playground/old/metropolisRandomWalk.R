




# Objective function for Metropolis Random Walk

calibrateGravity <- function(params){
  
  require(dplyr)
  
  
  od %>%
    mutate(gravity = case_when(distance.km==0 ~ 
                                 params[1]*log.POPESTIMATE.origin, 
                               distance.km < params[2] ~ 
                                 params[3] + 
                                 params[4]*log.POPESTIMATE.origin + 
                                 params[5]*log.POPESTIMATE.destination + 
                                 params[6]*log.distance.km)) %>%
    mutate(sqerr = (log.Workers.in.Commuting.Flow-gravity)^2) %>%
    summarise(rmse = sqrt(mean(sqerr))) %>%
    select(rmse) %>%
    unlist() %>%
    return()
  
}







# Posterior function
## calculates exponential likelihood with pre-specified tau parameter and RMSE (?)
### tau parameter corresponds to exponential decay rate
mylikelihoodTimesPrior <- function(metrics){
  thelikelihood <- exp(-(tau/2)*distance.to.corner(metrics));
  theprior <- 1;
  myreturn <- thelikelihood * theprior;
  myreturn
}




# Metropolis Random Walk
## Random seed set for reproducibility
## tau parameter set so that acceptance rate =~20-30%
## 10000 Monte Carlo chosen to ensure convergence
## Initial values for cutoffs used in detect.Change() chosen for congruity with Method 1
## Proposal distributions for cutoffs are Uniform with min and max parameters corresponding to min / max of possible motion logger values
### Acceleration cutoffs ~ U(0,1)
### Tilt cutoffs ~ U(0,180)
## Results compiled and saved to dataframe




# as.numeric(Sys.time())
# 1693781865
set.seed(1693781865)
tau <- 10
mcruns <- 100


k.chain <- rep(NA, mcruns)
beta1.chain <- rep(NA, mcruns)
beta2.chain <- rep(NA, mcruns)
beta3.chain <- rep(NA, mcruns)


accept <- rep(NA,mcruns)
alphachain <- rep(NA, mcruns)

rmse <- rep(NA,mcruns)


k.chain[1] <- 0.75
beta1.chain[1] <- 1
beta2.chain[1] <- 1
beta3.chain[1] <- -1


metrics <- wrapper(test.list, c(cutoff.accel.chain.x[1], cutoff.accel.chain.y[1], cutoff.accel.chain.z[1], 
                                cutoff.tilt.chain.x[1],  cutoff.tilt.chain.y[1],  cutoff.tilt.chain.z[1]))

sensitivity[1] <- metrics[1]
specificity[1] <- metrics[2]





st <- Sys.time()
for(mc in 2:mcruns){
  current.cutoffs = c(cutoff.accel.chain.x[mc-1], cutoff.accel.chain.y[mc-1], cutoff.accel.chain.z[mc-1],
                      cutoff.tilt.chain.x[mc-1],  cutoff.tilt.chain.y[mc-1],  cutoff.tilt.chain.z[mc-1])
  
  prop.cutoffs = c(runif(1, 0, 1),  
                   runif(1, 0, 1),
                   runif(1, 0, 1),
                   runif(1, 0, 180),
                   runif(1, 0, 180),
                   runif(1, 0, 180))
  
  current.metrics <- wrapper(test.list, current.cutoffs)
  prop.metrics <- wrapper(test.list, prop.cutoffs)
  
  alphanum = mylikelihoodTimesPrior(prop.metrics)
  alphaden = mylikelihoodTimesPrior(current.metrics)
  
  alpha = min(1, alphanum/alphaden)
  alphachain[mc] = alpha
  myunif = runif(1)
  
  if(myunif < alpha){
    accept[mc] = 1
    cutoff.accel.chain.x[mc] = prop.cutoffs[1]
    cutoff.accel.chain.y[mc] = prop.cutoffs[2]
    cutoff.accel.chain.z[mc] = prop.cutoffs[3]
    cutoff.tilt.chain.x[mc] = prop.cutoffs[4]
    cutoff.tilt.chain.y[mc] = prop.cutoffs[5]
    cutoff.tilt.chain.z[mc] = prop.cutoffs[6]
    sensitivity[mc] = prop.metrics[1]
    specificity[mc] = prop.metrics[2]
  }
  if(myunif > alpha){
    accept[mc] = 0
    cutoff.accel.chain.x[mc] = current.cutoffs[1]
    cutoff.accel.chain.y[mc] = current.cutoffs[2]
    cutoff.accel.chain.z[mc] = current.cutoffs[3]
    cutoff.tilt.chain.x[mc] = current.cutoffs[4]
    cutoff.tilt.chain.y[mc] = current.cutoffs[5]
    cutoff.tilt.chain.z[mc] = current.cutoffs[6]
    sensitivity[mc] = current.metrics[1]
    specificity[mc] = current.metrics[2]
  }
  cat(round(mc/mcruns*100,2), "% \n")
}
et <- Sys.time()

# 40.69618 secs for 100 mc
# 40.39888 s 100 mc
# 1.105228 h for 10000 mc

mhrw <- data.frame(
  cutoff.accel.chain.x=cutoff.accel.chain.x, cutoff.accel.chain.y=cutoff.accel.chain.y, cutoff.accel.chain.z=cutoff.accel.chain.z, 
  cutoff.tilt.chain.x=cutoff.tilt.chain.x,   cutoff.tilt.chain.y=cutoff.tilt.chain.y,   cutoff.tilt.chain.z=cutoff.tilt.chain.z,
  
  alphachain=alphachain, accept=accept, mcruns=mcruns,
  
  sensitivity=sensitivity, specificity=specificity)

# save(mhrw, file="mhrw_v3.rdata")



load("mhrw_v3.rdata")
mhrw$one.minus.spec <- 1 - mhrw$specificity
mhrw$sens.spec <- mhrw$sensitivity*mhrw$specificity
mhrw$d2c <- as.numeric(unlist(distance.to.corner(mhrw[,c("sensitivity","specificity")])))




plot(mhrw$d2c, ylab="Distance to Corner", main="Values of Objective Function across Random Walk")
points(mhrw[which(mhrw$d2c==min(mhrw$d2c)),"d2c"], pch = 16, col = "red")




{
  plot(x=mhrw$one.minus.spec, y=mhrw$sensitivity, xlab = "1-Specificity", ylab = "Sensitivity")#, main = "ROC Curve")
  points(x=mhrw[which(mhrw$d2c==min(mhrw$d2c)),"one.minus.spec"], y=mhrw[which(mhrw$d2c==min(mhrw$d2c)),"sensitivity"], pch = 16, col = "red")
}

roc <- unique(mhrw[,c("one.minus.spec", "sensitivity")])

roc <- roc[order(roc$one.minus.spec, roc$sensitivity, decreasing = TRUE),]
roc <- roc[!duplicated(roc$one.minus.spec),]

roc <- rbind(roc, data.frame(one.minus.spec = c(0,1), sensitivity = c(0,1)))
roc <- roc[order(roc$one.minus.spec),]

{
  plot(x=roc$one.minus.spec, y=roc$sensitivity, type = "o")
  abline(a=c(0,0), b=c(1,1), lty=3)
}
library(pracma)
trapz(roc$one.minus.spec, roc$sensitivity)

library(zoo)
sum(diff(roc$one.minus.spec)*rollmean(roc$sensitivity,2))








#"Random Walk Trace Plots"
{
  par(mfrow=c(3,2), mar=c(2.1,2.1,2.1,0))
  plot(mhrw$cutoff.accel.chain.x, type="l", main = "X-axis Acceleration")
  plot(mhrw$cutoff.tilt.chain.x, type="l", main = "X-axis Tilt")
  plot(mhrw$cutoff.accel.chain.y, type="l", main = "Y-axis Acceleration")
  plot(mhrw$cutoff.tilt.chain.y, type="l", main = "Y-axis Tilt")
  plot(mhrw$cutoff.accel.chain.z, type="l", main = "Z-axis Acceleration")
  plot(mhrw$cutoff.tilt.chain.z, type="l", main = "Z-axis Tilt")
}



par(mar=c(5.1,4.1,4.1,2.1))
{
  par(mfrow=c(2,1))
  plot(mhrw$alphachain, type="l")
  plot.new()
  text(0.5, 0.5, paste("Acceptance Proportion = ", sum(mhrw$accept[2:length(mhrw$accept)])/mhrw$mcruns[1]))
}#~20-30%

{
  par(mfrow=c(3,2), mar=c(2.1,2.1,2.1,0))
  hist(mhrw$cutoff.accel.chain.x)
  hist(mhrw$cutoff.tilt.chain.x)
  hist(mhrw$cutoff.accel.chain.y)
  hist(mhrw$cutoff.tilt.chain.y)
  hist(mhrw$cutoff.accel.chain.z)
  hist(mhrw$cutoff.tilt.chain.z)
}


#"Running Mean Plots"
{
  accel.run.mean.x <- cumsum(mhrw$cutoff.accel.chain.x)/1:mhrw$mcruns[1]
  tilt.run.mean.x <- cumsum(mhrw$cutoff.tilt.chain.x)/1:mhrw$mcruns[1]
  accel.run.mean.y <- cumsum(mhrw$cutoff.accel.chain.y)/1:mhrw$mcruns[1]
  tilt.run.mean.y <- cumsum(mhrw$cutoff.tilt.chain.y)/1:mhrw$mcruns[1]
  accel.run.mean.z <- cumsum(mhrw$cutoff.accel.chain.z)/1:mhrw$mcruns[1]
  tilt.run.mean.z <- cumsum(mhrw$cutoff.tilt.chain.z)/1:mhrw$mcruns[1]
  par(mfrow=c(3,2), mar=c(2.1,2.1,2.1,0))
  plot(accel.run.mean.x, type="l", main = "X-axis Acceleration")
  plot(tilt.run.mean.x, type="l", main = "X-axis Tilt")
  plot(accel.run.mean.y, type="l", main = "Y-axis Acceleration")
  plot(tilt.run.mean.y, type="l", main = "Y-axis Tilt")
  plot(accel.run.mean.z, type="l", main = "Z-axis Acceleration")
  plot(tilt.run.mean.z, type="l", main = "Z-axis Tilt")
}




{
  acf(accel.run.mean.x) 
  acf(tilt.run.mean.x)
  acf(accel.run.mean.y) 
  acf(tilt.run.mean.y)
  acf(accel.run.mean.z) 
  acf(tilt.run.mean.z)
}
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))





cutoff.accel.x <- mean(mhrw$cutoff.accel.chain.x)
cutoff.tilt.x <- mean(mhrw$cutoff.tilt.chain.x)
cutoff.accel.y <- mean(mhrw$cutoff.accel.chain.y)
cutoff.tilt.y <- mean(mhrw$cutoff.tilt.chain.y)
cutoff.accel.z <- mean(mhrw$cutoff.accel.chain.z)
cutoff.tilt.z <- mean(mhrw$cutoff.tilt.chain.z)

rw.cutoffs <- c(cutoff.accel.x, cutoff.accel.y, cutoff.accel.z, 
                cutoff.tilt.x, cutoff.tilt.y, cutoff.tilt.z)

rw.cutoffs <- c(0.75, 0.75, 0.75, 135, 135, 135)