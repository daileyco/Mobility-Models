




load("./01-Data/02-Analytic-Data/od.rds")

library(dplyr)

od.internal <- od %>%
  filter(distance.km==0)

od.external <- od %>%
  filter(distance.km!=0)

odess <- od.external[sample(1:nrow(od.external), round(0.2*nrow(od.external))),]

plot(odess$log.Workers.in.Commuting.Flow~odess$log.distance.km)
loess.fit <- loess(odess$log.Workers.in.Commuting.Flow~odess$log.distance.km)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")



plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.distance.km<5)]~odess$log.distance.km[which(odess$log.distance.km<5)])

plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.distance.km>=5)]~odess$log.distance.km[which(odess$log.distance.km>=5)])
loess.fit <- loess(odess$log.Workers.in.Commuting.Flow[which(odess$log.distance.km>=5)]~odess$log.distance.km[which(odess$log.distance.km>=5)])

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")


plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.distance.km>=5)]~odess$log.POPESTIMATE.origin[which(odess$log.distance.km>=5)])
plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.distance.km>=5)]~odess$log.POPESTIMATE.destination[which(odess$log.distance.km>=5)])

plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.distance.km<5)]~odess$log.POPESTIMATE.origin[which(odess$log.distance.km<5)])
plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.distance.km<5)]~odess$log.POPESTIMATE.destination[which(odess$log.distance.km<5)])







plot(odess$log.Workers.in.Commuting.Flow[which(!(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75)))]
     ~odess$log.distance.km[which(!(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75)))])


plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75))]
     ~odess$log.distance.km[which(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75))])






plot(odess$log.Workers.in.Commuting.Flow[which(!(odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75)))]
     ~odess$log.distance.km[which(!(odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75)))])


plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75))]
     ~odess$log.distance.km[which(odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75))])




plot(odess$log.Workers.in.Commuting.Flow[which(!(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75) &
                                                 odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75)))]
     ~odess$log.distance.km[which(!(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75) &
                                    odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75)))])


plot(odess$log.Workers.in.Commuting.Flow[which(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75) &
                                                 odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75))]
     ~odess$log.distance.km[which(odess$log.POPESTIMATE.origin>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75) &
                                    odess$log.POPESTIMATE.destination>quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 0.75))])









odess <- odess %>%
  mutate(po.cat = findInterval(log.POPESTIMATE.origin, 
                               quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 1:2/3)), 
         pd.cat = findInterval(log.POPESTIMATE.destination, 
                               quantile(odess$log.POPESTIMATE.origin[which(!duplicated(odess[,1:3]))], probs = 1:2/3))) %>%
  mutate(p.cats = case_when(po.cat == 0 & pd.cat == 0 ~ "ss", 
                            po.cat == 0 & pd.cat == 1 ~ "sm",
                            po.cat == 0 & pd.cat == 2 ~ "sl",
                            po.cat == 1 & pd.cat == 0 ~ "ms", 
                            po.cat == 1 & pd.cat == 1 ~ "mm",
                            po.cat == 1 & pd.cat == 2 ~ "ml",
                            po.cat == 2 & pd.cat == 0 ~ "ls", 
                            po.cat == 2 & pd.cat == 1 ~ "lm",
                            po.cat == 2 & pd.cat == 2 ~ "ll"))

cats <- paste0(rep(c("s","m","l"), each = 3), c("s","m","l"))

library(RColorBrewer)

cats.pal <- brewer.pal()

library(viridis)

col.grid <- cbind(lty = rep(c(3,5,1), each = 3), col = viridis(3, direction = -1))





plot(odess$log.Workers.in.Commuting.Flow~odess$log.distance.km, 
     col = rgb(0,0,0,0.0125), 
     pch = 16, 
     ylim = c(1.5,8), 
     xlim = c(3,6.5))

for(i in 1:length(cats)){
  loess.fit <- loess(odess$log.Workers.in.Commuting.Flow[which(odess$p.cats==cats[i])]~odess$log.distance.km[which(odess$p.cats==cats[i])])
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = col.grid[i,2], 
        lty = as.numeric(col.grid[i,1]), 
        lwd = 2)
}

legend("topright", 
       legend = cats, 
       col = col.grid[,2], 
       lty = as.numeric(col.grid[,1]), 
       ncol = 3, 
       title = "Pop Cats O & D")





plot(odess$log.Workers.in.Commuting.Flow~odess$log.distance.km, 
     col = rgb(0,0,0,0.0125), 
     pch = 16, 
     ylim = c(1.5,8), 
     xlim = c(3,6.5))

for(i in 1:3){
  loess.fit <- loess(odess$log.Workers.in.Commuting.Flow[which(odess$po.cat==i-1)]~odess$log.distance.km[which(odess$po.cat==i-1)])
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = viridis(3, direction = -1)[i], 
        lty = c(3,5,1)[i], 
        lwd = 2)
}

legend("topright", 
       legend = c("s", "m", "l"), 
       col = viridis(3, direction = -1), 
       lty = c(3,5,1), 
       ncol = 1, 
       title = "Pop Cats O")





plot(odess$log.Workers.in.Commuting.Flow~odess$log.distance.km, 
     col = rgb(0,0,0,0.0125), 
     pch = 16, 
     ylim = c(1.5,8), 
     xlim = c(3,6.5))

for(i in 1:3){
  loess.fit <- loess(odess$log.Workers.in.Commuting.Flow[which(odess$pd.cat==i-1)]~odess$log.distance.km[which(odess$pd.cat==i-1)])
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
        col = viridis(3, direction = -1)[i], 
        lty = c(3,5,1)[i], 
        lwd = 2)
}

legend("topright", 
       legend = c("s", "m", "l"), 
       col = viridis(3, direction = -1), 
       lty = c(3,5,1), 
       ncol = 1, 
       title = "Pop Cats D")







plot(od.internal$log.Workers.in.Commuting.Flow~od.internal$log.POPESTIMATE.origin, 
     col = rgb(0,0,0,0.025), 
     pch = 16)

plot(od.internal$`Workers in Commuting Flow`/od.internal$POPESTIMATE.origin~od.internal$log.POPESTIMATE.origin, 
     col = rgb(0,0,0,0.025), 
     pch = 16)



plot(x=odess$log.distance.km, 
     y=odess$log.Workers.in.Commuting.Flow,
     type="n")

for(i in 1:4){
  points(x=odess$log.distance.km[which(odess$census.region.origin==unique(od$census.region.origin)[i])], 
         y=odess$log.Workers.in.Commuting.Flow[which(odess$census.region.origin==unique(od$census.region.origin)[i])],
         col=rainbow(4,alpha = 0.1)[i],
         pch=i)
}

legend("topright", 
       legend = unique(od$census.region.origin), 
       col = rainbow(4), 
       pch = 1:4)



