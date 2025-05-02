# Script to create figure showing gravity models for each region


## load data
# load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")
load("./01-Data/02-Analytic-Data/estimates_gravity_model_simple.rdata")


## packages
library(dplyr)
library(RColorBrewer)



## helper functions


drawCircle <- function(cx,cy,r,ccol="black",nv=100){
  # cx <- 2
  # cy <- 2
  # 
  # r <- 1
  # 
  x <- seq(cx-r, cx+r, length.out = 50)
  
  y <- (sqrt((r^2)-(x-cx)^2)+cy)
  y <- c(y, cy+(cy-y))
  
  x <- c(x, x[length(x):1])
  
  polygon(x,y,border="black",col=ccol,xpd=T)
}









## manage data

gpowers <- gravity.powers.simple %>%
  filter(!(region%in%c("All US")) & period%in%c("2011-2020"))%>%
  arrange() %>% 
  mutate(b3scaled = ifelse(base%in%c("Distance (km)"), abs(Beta), NA), 
         # b3scaled = b3scaled-min(b3scaled, na.rm=T), 
         b3scaled = b3scaled/max(b3scaled, na.rm=T), 
         b3scaled = b3scaled^(1/2))







## colors n breaks n stuff

reg <- c("MW", "NE", "S", "W")
regcol <- brewer.pal(8, "Dark2")[c(1,3,4,7)]


png(filename = "./03-Output/02-Figures/figure_gravity_summary_concept.png", width = 10, height = 10, units = "in", res = 300, bg = "beige")

par(mfrow=c(2,2))


for(i in 1:4){
  
  
  b1 <- gpowers %>% filter(base%in%c("Population Size, Origin") & substr(region,1,1)==substr(reg[i],1,1))
  b2 <- gpowers %>% filter(base%in%c("Population Size, Destination") & substr(region,1,1)==substr(reg[i],1,1))
  b3 <- gpowers %>% filter(base%in%c("Distance (km)") & substr(region,1,1)==substr(reg[i],1,1))
  
  
  
  
  # betapops.breaks <- seq(min(c(b1$Beta, b2$Beta)), max(c(b1$Beta, b2$Beta)), length.out = 11)
  
  
  b1col <- colorRamp(c("white",regcol[i]))(b1$Beta) %>% 
    apply(., MARGIN = 1, (\(x){do.call("rgb", as.list(x/255))}))
  
  
  b2col <- colorRamp(c("white",regcol[i]))(b2$Beta) %>% 
    apply(., MARGIN = 1, (\(x){do.call("rgb", as.list(x/255))}))
  
  
  
  
  b3col <- colorRamp(c("white","black"))(b3$b3scaled) %>% 
    apply(., MARGIN = 1, (\(x){do.call("rgb", as.list(x/255))}))
  
  
  
  
  plot.new()
  par(mar=c(0,4,0,0))
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
    
    
    decay <- flow^b3$Beta[ii]
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
            col = b3col[ii])
    
    segments(x0 = cxorigin[ii]+rorigin[ii], 
             x1 = cxdest[ii]-rdest[ii], 
             y0 = cyorigin[ii]+rorigin[ii], 
             # y1 = cydest[ii]-rdest[ii], 
             y1 = cyorigin[ii]-rorigin[ii]+rorigin[ii]*2*((flow^-1)/max(flow^-1))[length(flow)],
             lty = 3, 
             col = "red")
    
    drawCircle(cxorigin[ii], cyorigin[ii], rorigin[ii], b1col[ii])
    text(x=cxorigin[ii], y=cyorigin[ii], 
         labels = round(b1$Beta[ii],3), 
         adj = c(0.5,0.5), 
         cex = 0.8)
    
    drawCircle(cxdest[ii], cydest[ii], rdest[ii], b2col[ii])
    text(x=cxdest[ii], y=cydest[ii], 
         labels = round(b2$Beta[ii],3), 
         adj = c(0.5,0.5), 
         cex = 0.8)
  }
  
  axis(2, at = c(0,9), labels = F, tcl = 0.2, pos = 1.5)
  axis(2, at = mean(c(0,9)), labels = "Long-distance", tcl = -0.1, pos=1.5, padj = 1.5, cex.axis = 0.7)
  axis(2, at = c(0,9)+10, labels = F, tcl = 0.2, pos=1.5)
  axis(2, at = mean(c(0,9)+10), labels = "Short-distance", tcl = -0.1, pos=1.5, padj = 1.5, cex.axis = 0.7)
  
  rect(par('usr')[1],0,par('usr')[1]+par('cxy')[2]*1.25,19, 
       col = regcol[i],
       border = regcol[i])
  text(x=mean(c(par('usr')[1],par('usr')[1]+par('cxy')[2]*1.25)), 
       y=mean(c(0,19)),
       labels = reg[i], 
       col = "white", 
       srt = 90)
  
  
  
  drawCircle(8.5,-1.5,0.25,ccol = NA)
  drawCircle(8.5,-3,0.5,ccol = NA)
  text(x=c(7.875,7.875),y=c(-1.5,-3),labels=c("Smaller", "Large"),adj=c(1,0.5), xpd = T, cex=0.5)
  text(x=8.1,y=-4,labels="Population\nSizes",xpd=T,font=2,cex=0.5,adj=c(0.5,1))
  
  
  xlefts <- rep(10,10)
  xrights <- xlefts+1
  ybottoms <- seq(-3.5,-1.25,length.out=11)
  ytops <- ybottoms[-1]
  ybottoms <- ybottoms[-length(ybottoms)]
  
  
  colscale <- colorRamp(c("white",regcol[i]))(seq(0,1,length.out=10)) %>% 
    apply(., MARGIN = 1, (\(x){do.call("rgb", as.list(x/255))}))
  
  
  rect(xleft = xlefts, 
       xright = xrights, 
       ybottom = ybottoms, 
       ytop = ytops, 
       col = colscale,
       border = colscale,
       xpd = T)
  rect(10,-3.5,11,-1.25,col=NA,border="black",xpd=T)
  text(x=11.1,y=seq(-3.5,-1.25,length.out=4),labels=round(seq(0,1,length.out=4),2),adj=c(0,0.5), xpd = T, cex = 0.5)
  text(x=10.875,y=-4,labels="Population\nPower",xpd=T,font=2,cex=0.5,adj=c(0.5,1))
  
  
  
  flow2 <- seq(12.5,13.75,by=0.01)
  decay2 <- flow2^-10
  decay2 <- decay2/max(decay2)
  top <- -3.5+2.25*decay2
  
  rect(12.5,-3.5,13.75,-1.25,col=NA,border="black",xpd=T)
  
  polygon(x=c(flow2,flow2[length(flow2):1]), 
          y=c(top,rep(-3.5,length(flow2))), 
          col="grey40")
  
  segments(x0=12.5,y0=-1.25,x1=13.75,y1=-2.25,lty=3,col="red",xpd=T)
  text(x=13.25,y=-4,labels="Distance\nDecay",xpd=T,font=2,cex=0.5,adj=c(0.5,1))
  text(x=13.75,y=-2.25,labels="1/d",xpd=T,font=3,cex=0.5,adj=c(-0.1,0.5),col="red")
  
  
  # axis(3,at=c(2,6),labels =F, tcl = 0.2, pos=19.5)
  axis(3,at=mean(c(2,6)),labels = "Origin /\nResidence", tick = F, tcl = -0.1, pos=19.25,padj=0.75,cex.axis=0.7)
  # axis(3,at=c(15,19),labels =F, tcl = 0.2, pos=19.5)
  axis(3,at=mean(c(15,19)),labels = "Destination /\nWork", tick =F, tcl = -0.1, pos=19.25,padj=0.75,cex.axis=0.7)
  
  axis(3,at=c(6,15),labels =F, tcl = 0.2, pos=19.5)
  axis(3,at=mean(c(6,15)),labels = "Commutes vs Distance", tcl = -0.1, pos=19.5,padj=1.75,cex.axis=0.7)
  
  
  
}


dev.off()






## save


## clean environment
rm(list=ls())
gc()



