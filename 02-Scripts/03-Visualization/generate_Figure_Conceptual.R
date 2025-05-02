# Script to create figure showing gravity model basics


## load data
# load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")
load("./01-Data/02-Analytic-Data/estimates_gravity_model_simple.rdata")


## packages
library(dplyr)
library(RColorBrewer)



## helper functions


drawCircle <- function(cx,cy,r,ccol="black",nv=100,clwd=1){
  # cx <- 2
  # cy <- 2
  # 
  # r <- 1
  # 
  x <- seq(cx-r, cx+r, length.out = 50)
  
  y <- (sqrt((r^2)-(x-cx)^2)+cy)
  y <- c(y, cy+(cy-y))
  
  x <- c(x, x[length(x):1])
  
  polygon(x,y,border="black",col=ccol,xpd=T,lwd=clwd)
}









## gravity model concept




### undirected, gravity equation

png("./03-Output/02-Figures/concept_gravity1_undirected.png", width = 5, height = 5, units = "in", res = 300, pointsize = 10)

plot.new()
# par(mar=c(0,4,0,0))
plot.window(xlim = c(0,3), ylim = c(0,3))


### origins
cx <- c(0.5,2.5)
cy <- c(1.5,1.5)
r <- c(0.5,0.5)

for(i in 1:2){
  
  drawCircle(cx[i],cy[i],r[i], ccol=NA, clwd=3)
  
}

arrows(x0=1,y0=1.5,x1=2,y1=1.5,
       length=0.1,angle=45,
       code=3, 
       lwd=3)

text(x=cx, y=cy,
     labels = c("A", "B"), 
     cex = 3, 
     font = 2)



axis(1, at = cx, labels = F, tcl = 0.3, pos = 0.75, lwd = 1, padj = 0.5)
axis(1, at = mean(cx), labels = bquote(distance[list(A,B)]), tick = F, pos=0.75,padj = 0.5, font=3)



axis(3, at = mean(cx), labels = bquote(T[AB]~"="~frac(P[A]%*%P[B],d[AB])), tick=F, pos=2.25, padj=0.5, hadj=0.5)

dev.off()






### directed, gravity equation
png("./03-Output/02-Figures/concept_gravity2_directed.png", width = 5, height = 5, units = "in", res = 300, pointsize = 10)

plot.new()
# par(mar=c(0,4,0,0))
plot.window(xlim = c(0,3), ylim = c(0,3))


### origins
cx <- c(0.5,2.5)
cy <- c(1.5,1.5)
r <- c(0.5,0.5)

for(i in 1:2){
  
  drawCircle(cx[i],cy[i],r[i], ccol=NA, clwd=3)
  
}

arrows(x0=1,y0=1.5,x1=2,y1=1.5,
       length=0.1,angle=45,
       code=2, 
       lwd=3)

text(x=cx, y=cy,
     labels = c("A", "B"), 
     cex = 3, 
     font = 2)

text(x=cx, y=cy+0.75,
     labels = c("Origin/Residence", "Destination/Work"), 
     cex = 1, 
     font = 2)

axis(1, at = cx, labels = F, tcl = 0.3, pos = 0.75, lwd = 1, padj = 0.5)
axis(1, at = mean(cx), labels = bquote(distance[list(A,B)]), tick = F, pos=0.75,padj = 0.5, font=3)



axis(3, at = mean(cx), labels = bquote(T[AB]~"="~frac(P[A]^{beta[1]}%*%P[B]^{beta[2]},d[AB]^{beta[3]})), tick=F, pos=2.25, padj=0.5, hadj=0.5)

dev.off()










### distance decay
png("./03-Output/02-Figures/concept_gravity3_distancedecay.png", width = 5, height = 5, units = "in", res = 300, pointsize = 10)


x <- seq(1,10,by=0.5)
y <- x^(-1)


plot(x,y, type = "l", 
     axes = F, 
     xlab = "Distance", 
     ylab = "Probability of Displacement/Movement/Travel", 
     xlim = c(1,10), 
     ylim = c(0,1))

# axis(1, at = par('usr')[1:2], labels = F, tcl = 0, pos = 0, padj = 0.5, hadj = 0.5)
arrows(#x0=mean(par('usr')[1:2])-mean(par('usr')[1:2])*0.5,x1=mean(par('usr')[1:2])+mean(par('usr')[1:2])*0.5, 
       x0=1, x1=par('usr')[2], 
       y0=-0.05, y1=-0.05,
       code = 2, 
       angle = 45, 
       lwd = 1, 
       xpd = T, 
       length = 0.1)


axis(2, at = c(0,0.5,1), labels = c(0,0.5,1), tcl = 0.1, pos = 0.5, las = 2, hadj = 1, cex.axis = 0.7)




rect(xleft = 1, xright = mean(c(1,10)), 
     ybottom = 0, ytop = 1, 
     col = adjustcolor("orange", alpha.f = 0.5), 
     border = NA)
rect(xleft = mean(c(1,10)), xright = 10, 
     ybottom = 0, ytop = 1, 
     col = adjustcolor("blue", alpha.f = 0.5), 
     border = NA)


segments(x0=mean(c(1,10)), y0=0, y1=1.1, 
         lty = 3, 
         lwd = 4, 
         col = "black", 
         xpd = T)

text(x = mean(c(1,10)), y = 1.1, 
     labels = c("Distance\nThreshold"), 
     adj = c(0,1), 
     cex = 1, 
     font = 2, 
     xpd = T, 
     srt = 45)



text(x = c(mean(c(1,mean(c(1,10)))), mean(c(mean(c(1,10)),10))), 
     y = par('usr')[4], 
     labels = c("Short-distance", "Long-distance"), 
     adj = c(0.5, 3), 
     cex = 1, 
     font = 3)


dev.off()








## assortative property based on population sizes
png("./03-Output/02-Figures/concept_gravity4_largepopassortative.png", width = 5, height = 5, units = "in", res = 300, pointsize = 10)


xlefts <- rep(1:3,3)
xrights <- xlefts+1
ybottoms <- rep(1:3,each=3)
ytops <- ybottoms+1

sqcols <- adjustcolor(c(rep("yellow",8), "purple"), alpha.f = 0.5)


plot.new()
plot.window(xlim=c(0.5,4),
            ylim=c(0.5,4))

rect(xleft=xlefts,
     xright=xrights,
     ybottom=ybottoms,
     ytop=ytops,
     col = sqcols, 
     border = "gainsboro")


axis(1,at=mean(par('usr')[1:2]),labels = c("Population Size\nof Destination/Work Location "), tick=F,hadj=0.5,padj=0.5, pos = 0.5)
axis(1,at=1:3+0.5,labels = c("Small", "Medium", "Large"),tick=F,hadj=0.5,padj=0.5, pos = c(1))
axis(2,at=mean(par('usr')[3:4]),labels = c("Population Size\nof Origin/Resident Location "), tick=F,line=1,hadj=0.5,padj=0.5, pos = 0.5)
axis(2,at=1:3+0.5,labels = c("Small", "Medium", "Large"),tick=F,hadj=0.5,padj=0.5, pos = c(1))

dev.off()






## introductory conceptual figure


png("./03-Output/02-Figures/concept_gravity5_figexample.png", width = 5, height = 5, units = "in", res = 300, pointsize = 10)

plot.new()
# par(mar=c(0,4,0,0))
plot.window(xlim = c(0,19), ylim = c(-5,20))


# abline(h = seq(0,19, by = 0.5), col = "gainsboro");
# abline(v = seq(6,15, by = 0.5), col = "gainsboro")


bind_rows(expand.grid(x0 = 6, 
                      x1 = 15, 
                      y0 = c(seq(0,4,by = 0.5),
                             seq(0,4,by = 0.5)+5,
                             seq(0,4,by = 0.5)+10,
                             seq(0,4,by = 0.5)+15)) %>% 
            mutate(y1 = y0), 
          expand.grid(x0 = seq(6,15,by=0.5), 
                      y0 = c(0,5,10,15)) %>% 
            mutate(x1 = x0, 
                   y1=y0+4)) %>% 
  apply(., 
        MARGIN = 1, 
        (\(z){
          segments(x0=z["x0"],
                   x1=z["x1"],
                   y0=z["y0"],
                   y1=z["y1"], 
                   col = "gainsboro", 
                   xpd = T)
        }))



### origins
cxorigin <- c(2,2,2,2)+2
cyorigin <- c(17,12,7,2)
rorigin <- c(2,2,2,2)


### destinations
cxdest <- c(10,11,10,11)+6
# cydest <- c(17,12,7,2)
cydest <- c(16,12,6,2)
rdest <- c(1,2,1,2)


for(ii in 1:4){
  
  
  flow <- seq(cxorigin[ii]+rorigin[ii], cxdest[ii]-rdest[ii], length.out = 50)
  
  
  decay <- flow^c(-2, -3, -0.5, -0.25)[ii]
  # decay <- decay-min(decay)
  decay <- decay/max(decay)
  
  y0 <- cyorigin[ii]-rorigin[ii]
  yt <- y0+rorigin[ii]*2*decay
  # yb <- y0-rorigin[ii]*decay
  yb <- rep(y0, length(decay))
  
  
  # polygon(x = c(cxorigin[ii],flow, flow[length(flow):1],cxorigin[ii]), 
  #         y = c(cyorigin[ii]+rorigin[ii], yt, yb[length(yb):1], cyorigin[ii]-rorigin[ii]), 
  #         col = b3col[ii])
  polygon(x = c(flow, flow[length(flow):1]), 
          y = c(yt, yb[length(yb):1]), 
          col = c("gray30", "black", "gray70", "gray90")[ii], 
          border = NA)
  
  segments(x0 = cxorigin[ii]+rorigin[ii], 
           x1 = cxdest[ii]-rdest[ii], 
           y0 = cyorigin[ii]+rorigin[ii], 
           # y1 = cydest[ii]-rdest[ii], 
           y1 = cyorigin[ii]-rorigin[ii]+rorigin[ii]*2*((flow^-1)/max(flow^-1))[length(flow)],
           lty = 3, 
           col = "red")
  
  drawCircle(cxorigin[ii], cyorigin[ii], rorigin[ii], c("yellow", "purple", "yellow", "purple")[ii])
  text(x=cxorigin[ii], y=cyorigin[ii], 
       labels = expression(beta[1]), 
       adj = c(0.5,0.5), 
       cex = 0.8)
  
  drawCircle(cxdest[ii], cydest[ii], rdest[ii], c("yellow", "purple", "yellow", "purple")[ii])
  text(x=cxdest[ii], y=cydest[ii], 
       labels = expression(beta[2]), 
       adj = c(0.5,0.5), 
       cex = 0.8)
}

axis(2, at = c(0,9), labels = F, tcl = 0.2, pos = 1.5)
axis(2, at = mean(c(0,9)), labels = "Long-distance", tcl = -0.1, pos=1.5, padj = 1.5, cex.axis = 0.7)
axis(2, at = c(0,9)+10, labels = F, tcl = 0.2, pos=1.5)
axis(2, at = mean(c(0,9)+10), labels = "Short-distance", tcl = -0.1, pos=1.5, padj = 1.5, cex.axis = 0.7)


rect(xleft = 0, xright = 1.5, 
     ybottom = 0, ytop = 9, 
     col = adjustcolor("blue", alpha.f = 0.5), 
     border = NA)
rect(xleft = 0, xright = 1.5, 
     ybottom = 10, ytop = 19, 
     col = adjustcolor("orange", alpha.f = 0.5), 
     border = NA)



text(x=c(4, mean(c(4,16.5)), 16.5), y=c(20,20,20), 
     adj = c(0.5, 0), 
     labels = c(expression(beta[1]), expression(beta[3]), expression(beta[2])), 
     font = 2, 
     xpd = T)

dev.off()





## save


## clean environment
rm(list=ls())
gc()



