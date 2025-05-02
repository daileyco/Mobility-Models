# Script to create figure showing internal flows vs population at origin

## load data
load("./01-Data/02-Analytic-Data/od.rds")

## packages
library(RColorBrewer)

## remove zero distance flows
od <- od[which(od$distance.km==0),]


## plot
### save figure
png("./03-Output/02-Figures/flows_internal_regions.png", units = "in", res = 300, width = 16, height = 18, pointsize = 10)

par(mfrow = c(2,1), mar = c(5.1,5.1,1.1,1.1))


### population origin
plot(x=od$log.POPESTIMATE.origin, 
     y=od$log.Workers.in.Commuting.Flow, 
     pch = 16, 
     col = rgb(0.1,0.1,0.1,alpha = 0.05), 
     axes = F, 
     xlab = "", 
     ylab = "", 
     xlim = log(c(50,11000000)), 
     ylim = c(3,max(od$log.Workers.in.Commuting.Flow)+0.01*max(od$log.Workers.in.Commuting.Flow)))


axis(1, 
     at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
     # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
     labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
     las = 1,
     line = 0, 
     tcl = -0.2)

axis(2, 
     at = log(c(50,100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000)), 
     # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
     labels = c(50,100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m"),
     las = 1,
     line = 0, 
     tcl = -0.2)

title(xlab = "Resident Population")
title(ylab = "Workers who Work in Resident Location", line = 3.5)

box()



### add loess lines
loess.fit <- loess(od$log.Workers.in.Commuting.Flow~od$log.POPESTIMATE.origin)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
      col = "black", 
      lty = 1, 
      lwd = 2)

#### for each census region
for(i in 1:4){
  loess.fit <- loess(od$log.Workers.in.Commuting.Flow[which(od$census.region.origin==unique(od$census.region.origin)[i])]~od$log.POPESTIMATE.origin[which(od$census.region.origin==unique(od$census.region.origin)[i])])
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
        lty = c(2:5)[i], 
        lwd = 3)
}

### legend
legend("topleft", 
       legend = c("All US", unique(od$census.region.origin)), 
       col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
       lty = c(1, c(2:5)), 
       lwd = 3, 
       seg.len = 4)
### add letter to top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)



### population origin proportion
plot(x=od$log.POPESTIMATE.origin, 
     y=od$`Workers in Commuting Flow`/od$POPESTIMATE.origin, 
     pch = 16, 
     col = rgb(0.1,0.1,0.1,alpha = 0.1), 
     axes = F, 
     xlab = "", 
     ylab = "", 
     xlim = log(c(50,11000000)), 
     ylim = c(0,1.05))


axis(1, 
     at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
     # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
     labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
     las = 1,
     line = 0, 
     tcl = -0.2)

axis(2, 
     at = 0:10/10, 
     las = 1,
     line = 0, 
     tcl = -0.2)

title(xlab = "Resident Population")
title(ylab = "Proportion of Resident Population who Work in Resident Location", line = 3.5)

box()



### add loess lines
loess.fit <- loess(od$`Workers in Commuting Flow`/od$POPESTIMATE.origin~od$log.POPESTIMATE.origin)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
      col = "black", 
      lty = 1, 
      lwd = 2)

#### for each census region
for(i in 1:4){
  loess.fit <- loess(od$`Workers in Commuting Flow`[which(od$census.region.origin==unique(od$census.region.origin)[i])]/od$POPESTIMATE.origin[which(od$census.region.origin==unique(od$census.region.origin)[i])]~od$log.POPESTIMATE.origin[which(od$census.region.origin==unique(od$census.region.origin)[i])])
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
        lty = c(2:5)[i], 
        lwd = 3)
}

### legend
legend("topleft", 
       legend = c("All US", unique(od$census.region.origin)), 
       col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
       lty = c(1, c(2:5)), 
       lwd = 3, 
       seg.len = 4)
### add letter to top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)



## close out
dev.off()


## clean environment
rm(list = ls())
gc()









