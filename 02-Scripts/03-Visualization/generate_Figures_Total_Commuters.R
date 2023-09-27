# Script to create figure showing total commuters vs population at origin

## load data
load("./01-Data/02-Analytic-Data/od.rds")

## packages
library(dplyr)
library(RColorBrewer)




## create flow type variable
od <- od %>% 
  mutate(flow.type = case_when(distance.km==0 ~ "Intracounty", 
                               `State FIPS Residence`==`State FIPS Work` ~ "Intrastate", 
                               census.region.origin==census.region.destination ~ "Intraregion", 
                               TRUE ~ "Interregion"))

## summarise by flow type and origin location
od.ft <- od %>%
  group_by(period, `County FIPS Residence`, `County Residence`, `State FIPS Residence`, `State Residence`, POPESTIMATE.origin, census.region.origin, flow.type) %>%
  summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`)) %>%
  ungroup() %>%
  group_by(period, `County FIPS Residence`, `County Residence`, `State FIPS Residence`, `State Residence`) %>%
  mutate(Total.Commuters = sum(`Workers in Commuting Flow`)) %>%
  ungroup() %>%
  arrange(census.region.origin)

### summarise all the way by origin location
od.commuters <- od %>%
  group_by(period, `County FIPS Residence`, `County Residence`, `State FIPS Residence`, `State Residence`, POPESTIMATE.origin, census.region.origin, Total.Workers.Residence, Total.Workers.Residence.Domestic) %>%
  summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`)) %>%
  ungroup() %>%
  arrange(census.region.origin)



## plot
### save figure
png("./03-Output/02-Figures/commuters_vs_population_by_flowtype.png", units = "in", res = 300, width = 8.5, height = 15, pointsize = 10)

par(mfrow = c(5,2), mar = c(5.1,5.1,1.1,1.1))


### total commuters vs population origin
plot(x=log(od.commuters$POPESTIMATE.origin), 
     y=log(od.commuters$`Workers in Commuting Flow`), 
     pch = 16, 
     col = rgb(0.1,0.1,0.1,alpha = 0.05), 
     axes = F, 
     xlab = "", 
     ylab = "", 
     xlim = log(c(50,11000000)), 
     ylim = c(4,log(max(od.commuters$`Workers in Commuting Flow`))))


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
title(ylab = "Total Commuters", line = 3.5)

box()



### add loess lines
loess.fit <- loess(log(od.commuters$`Workers in Commuting Flow`)~log(od.commuters$POPESTIMATE.origin))

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
      col = "black", 
      lty = 1, 
      lwd = 2)

#### for each census region
for(i in 1:4){
  loess.fit <- loess(log(od.commuters$`Workers in Commuting Flow`)[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])]~log(od.commuters$POPESTIMATE.origin)[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])])
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
        lty = c(2:5)[i], 
        lwd = 3)
}

### legend
legend("topleft", 
       legend = c("All US", unique(od.commuters$census.region.origin)), 
       col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
       lty = c(1, c(2:5)), 
       lwd = 3, 
       seg.len = 4)
### add letter to top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)



### population origin proportion
plot(x=log(od.commuters$POPESTIMATE.origin), 
     y=od.commuters$`Workers in Commuting Flow`/od.commuters$POPESTIMATE.origin, 
     pch = 16, 
     col = rgb(0.1,0.1,0.1,alpha = 0.1), 
     axes = F, 
     xlab = "", 
     ylab = "", 
     xlim = log(c(50,11000000)), 
     ylim = c(0,1))


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
title(ylab = "Proportion of Resident Population who Commute", line = 3.5)

box()



### add loess lines
loess.fit <- loess(od.commuters$`Workers in Commuting Flow`/od.commuters$POPESTIMATE.origin~log(od.commuters$POPESTIMATE.origin))

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
      col = "black", 
      lty = 1, 
      lwd = 2)

#### for each census region
for(i in 1:4){
  loess.fit <- loess(od.commuters$`Workers in Commuting Flow`[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])]/od.commuters$POPESTIMATE.origin[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])]~log(od.commuters$POPESTIMATE.origin)[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])])
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
        lty = c(2:5)[i], 
        lwd = 3)
}

# ### legend
# legend("topright", 
#        legend = c("All US", unique(od.commuters$census.region.origin)), 
#        col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
#        lty = c(1, c(2:5)), 
#        lwd = 3, 
#        seg.len = 4)
### add letter to top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)









fts <- c("Intracounty", "Intrastate", "Intraregion", "Interregion")
fts.who <- c("within Their Resident County", "within Their Resident State", "within Their Resident Census Region", "outside Their Resident Census Region")

for(ii in 1:4){
  
  od.sub <- od.ft %>%
    filter(flow.type==fts[ii])
  
  ### total commuters vs population origin
  plot(x=log(od.sub$POPESTIMATE.origin), 
       y=log(od.sub$`Workers in Commuting Flow`), 
       pch = 16, 
       col = rgb(0.1,0.1,0.1,alpha = 0.05), 
       axes = F,
       xlab = "", 
       ylab = "", 
       xlim = log(c(50,11000000)),
       ylim = log(range(od.sub$`Workers in Commuting Flow`))
       )
  
  
  axis(1, 
       at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
       # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
       labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
       las = 1,
       line = 0, 
       tcl = -0.2)
  
  axis(2, 
       at = log(c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000)), 
       # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
       labels = c(1,2,5,10,20,50,100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m"),
       las = 1,
       line = 0, 
       tcl = -0.2)
  
  title(xlab = "Resident Population")
  title(ylab = paste0("Total ", fts[ii], " Commuters"), line = 3.5)
  
  box()
  
  
  
  ### add loess lines
  loess.fit <- loess(log(od.sub$`Workers in Commuting Flow`)~log(od.sub$POPESTIMATE.origin))
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = "black", 
        lty = 1, 
        lwd = 2)
  
  #### for each census region
  for(i in 1:4){
    loess.fit <- loess(log(od.sub$`Workers in Commuting Flow`)[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])]~log(od.sub$POPESTIMATE.origin)[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])])
    
    lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
          col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
          lty = c(2:5)[i], 
          lwd = 3)
  }
  
  # ### legend
  # legend("topleft", 
  #        legend = c("All US", unique(od.sub$census.region.origin)), 
  #        col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
  #        lty = c(1, c(2:5)), 
  #        lwd = 3, 
  #        seg.len = 4)
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = LETTERS[c(-1,-2)][1+(ii-1)*2], adj = c(0,1), xpd = T, cex = 1, font = 2)
  
  
  
  ### population origin proportion
  plot(x=log(od.sub$POPESTIMATE.origin), 
       y=od.sub$`Workers in Commuting Flow`/od.sub$POPESTIMATE.origin, 
       pch = 16, 
       col = rgb(0.1,0.1,0.1,alpha = 0.1), 
       axes = F, 
       xlab = "", 
       ylab = "", 
       xlim = log(c(50,11000000)), 
       ylim = c(0,max((od.sub$`Workers in Commuting Flow`/od.sub$POPESTIMATE.origin)[which(od.sub$`Workers in Commuting Flow`<od.sub$POPESTIMATE.origin)])))
  
  
  axis(1, 
       at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
       # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
       labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
       las = 1,
       line = 0, 
       tcl = -0.2)
  
  axis(2, 
       at = 0:20/20, 
       las = 1,
       line = 0, 
       tcl = -0.2)
  
  title(xlab = "Resident Population")
  title(ylab = paste0("Proportion Commuting ", fts.who[ii]), line = 3.5)
  
  box()
  
  
  
  ### add loess lines
  loess.fit <- loess(od.sub$`Workers in Commuting Flow`/od.sub$POPESTIMATE.origin~log(od.sub$POPESTIMATE.origin))
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = "black", 
        lty = 1, 
        lwd = 2)
  
  #### for each census region
  for(i in 1:4){
    loess.fit <- loess(od.sub$`Workers in Commuting Flow`[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])]/od.sub$POPESTIMATE.origin[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])]~log(od.sub$POPESTIMATE.origin)[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])])
    
    lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
          col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
          lty = c(2:5)[i], 
          lwd = 3)
  }
  
  # ### legend
  # legend("topright", 
  #        legend = c("All US", unique(od.sub$census.region.origin)), 
  #        col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
  #        lty = c(1, c(2:5)), 
  #        lwd = 3, 
  #        seg.len = 4)
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = LETTERS[c(-1,-2)][2+(ii-1)*2], adj = c(0,1), xpd = T, cex = 1, font = 2)
  
  
}





## close out
dev.off()


## clean environment
rm(list = ls())
gc()









