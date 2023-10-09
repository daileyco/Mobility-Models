# Script to create figure showing total commuters vs population at origin

## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")

## packages
library(dplyr)
library(RColorBrewer)


## split data to loop

dt <- distance.thresholds %>%
  split(., 
        f=~model.extent) %>%
  lapply(., 
         function(l){
           split(l, f=~objective.scale)# %>%
             # c(.,
             #      list(both=l))
         })

# > names(dt)
# [1] "base*distance_threshold"                      
# [2] "base*distance_threshold*large_populations"    
# [3] "base*distance_threshold*population_categories"
names(dt) <- c("Gravity w/ Distance Threshold", 
               "Large Population Pairing", 
               "Population Tertiles Pairing")



for(i in 1:length(dt)){
  
  me <- names(dt)[i]
  
  
  for(j in 1:length(dt[[i]])){
    
    os <- names(dt[[i]])[j] %>%
      toupper()%>%(\(x) paste0(substr(x,1,1), tolower(substr(x,2,nchar(x)))))
    
    
    tdt <- dt[[i]][[j]] %>%
      mutate(Period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE))
    
    png(filename = paste0("./03-Output/02-Figures/figure_parameters_gravity_distance_thresholds_", c("g0","g1","g2")[i], "_", tolower(os), ".png"), units = "in", res=300, width = 8, height = 4.5, pointsize=10)
    par(mar = c(5.1, 4.1, 2.1, 5.1))
    
    barplot(distance_threshold~region+Period, 
            data=tdt, 
            beside=TRUE, 
            col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
            ylim = c(0,10), 
            xpd = FALSE, 
            axes = FALSE,
            ylab = "",
            xlab = "",
            axisnames = FALSE
            # , legend.text = TRUE,
            # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
            # , plot = FALSE
    )
    abline(h = log(c(1,10,100,1000,10000)), 
           lty = 5, 
           col = "grey95")
    abline(h = log(c(2,5,20,50,200,500,2000,5000)), 
           lty = 3, 
           col = "grey90")
    
    
    barplot(distance_threshold~region+Period, 
            data=tdt, 
            beside=TRUE, 
            col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
            ylim = c(0,10), 
            xpd = FALSE, 
            axes = FALSE,
            ylab = "",
            xlab = "",
            axisnames = FALSE, 
            add = TRUE
            # , legend.text = TRUE,
            # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
            # , plot = FALSE
            ) %>%
      apply(., 
            MARGIN = 2, 
            (\(x) {
              axis(1, at = x, labels = c("US", "MW", "NE", "S", "W"), line = -1, tick = F, cex.axis = 0.8, gap.axis = 0.1)
              axis(1, at = range(x), labels = F, line = 1.25, tcl = 0.2);
              axis(1, at = mean(x), labels = F, line = 1.25, tcl = -0.2); 
              return(x)
            })) %>%
      (\(x) {
        axis(1, at = colMeans(x), labels = levels(tdt$Period), line = 0.5, tick = FALSE, cex.axis = 0.8);
        
        points(x=rep(unlist(x), 5), 
               y = od %>% 
                 filter(distance.km!=0) %>% 
                 bind_rows(mutate(., census.region.origin="All US")) %>% 
                 bind_rows(mutate(., period = "2011-2020")) %>% 
                 mutate(period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
                 arrange(period,census.region.origin)%>%
                 (\(y) {
                   
                   tapply(y, ~period+census.region.origin, (\(z) {
                     Hmisc::wtd.quantile(z$log.distance.km, weights = z$`Workers in Commuting Flow`, probs = c(0:4/4)) %>% t() %>% as.data.frame() %>%
                       bind_cols(data.frame(period=unique(z$period), region=unique(z$census.region.origin))) %>%
                       return()
                   }))
                   
                   
                   }) %>%
                     bind_rows() %>%
                 arrange(period) %>%
                 select(-period,-region) %>%
                 unlist()
               
               ## alternative
               # od %>% 
               #   filter(distance.km!=0) %>% 
               #   bind_rows(mutate(., census.region.origin="All US")) %>% 
               #   bind_rows(mutate(., period = "2011-2020")) %>% 
               #   mutate(period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
               #   arrange(period,census.region.origin) %>% group_by(period, census.region.origin) %>% summarise(across(c(log.distance.km), ~Hmisc::wtd.quantile(.x, .data[["Workers in Commuting Flow"]]) %>% t() %>% as.data.frame(), .unpack = TRUE)) %>% ungroup() %>% select(-period,-census.region.origin) %>% unlist()
               , 
               pch = rep(c(".", "-", "_", "-", "."), each = length(x)),
               cex = 2.5,
               col = "grey70");
        # return(x)
      })
      
    
    
    axis(2, at = log(c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000)), labels = c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000), las = 2, cex.axis = 0.8)
    axis(2, at = mean(log(c(25,50,100,200,400))), labels = "Distance (km)", las = 3, line = 1.75, tick = FALSE)
    
    # legend(x = par('usr')[2]/par('plt')[2], 
    #        y = par('usr')[4]/par('plt')[4],
    #        # xjust = 1, 
    #        # yjust = 1,
    #        # adj = c(0,0.5),
    #        xjust = 1, 
    #        yjust = 1,
    #        adj = c(1,0.5),
    #        x.intersp = 3,
    #   # "topright", 
    #        # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
    #        legend=c(as.expression(bquote('Q'[0])), 
    #                 as.expression(bquote('Q'[1])), 
    #                 as.expression(bquote('Q'[2])), 
    #                 as.expression(bquote('Q'[3])), 
    #                 as.expression(bquote('Q'[4]))), 
    #        pch = c(".", "-", "_", "-", "."), 
    #        col = "grey70", 
    #        cex = 0.5, 
    #        pt.cex = 2, 
    #        bty = "n", 
    #        # inset=-0.05, 
    #        xpd = TRUE, 
    #   title = "Observed\nDistances",
    #   title.cex = 0.7, 
    #   title.adj = 0) %>% 
    #   # .$rect %>% .$h %>%
    #   # legend(x = par('usr')[2]/par('plt')[2], 
    #   #        y = par('usr')[4]/par('plt')[4]-1*.,
    #   #        xjust = 0.625, 
    #   #        yjust = 1,
    #   #        adj = c(1,0.5),
    #   #        x.intersp = 5.5,
    #   #        # "topright", 
    #   #        # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
    #   #        legend=c("All US", "Midwest", "Northeast", "South", "West"), 
    #   #        # pch = rep(15,5), 
    #   #        fill = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
    #   #        cex = 0.6, 
    #   #        pt.cex = rep(1,5),
    #   #        ncol = 1,
    #   #        bty = "n", 
    #   #        # inset=-0.05, 
    #   #        xpd = TRUE, 
    #   #        title = "\nRegion",
    #   #        title.cex = 0.7, 
    #   #        title.adj = 0.5)
    #   .$rect %>% .$w %>%
    # legend(x = par('usr')[2]/par('plt')[2]-1.2*.,
    #        y = par('usr')[4]/par('plt')[4],
    #        xjust = 1,
    #        yjust = 1,
    #        adj = c(0,0.5),
    #        # "topright",
    #        # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
    #        legend=c("All US", "Midwest", "Northeast", "South", "West"),
    #        # pch = rep(15,5),
    #        fill = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]),
    #        cex = 0.5,
    #        pt.cex = rep(1,5),
    #        ncol = 1,
    #        bty = "n",
    #        # inset=-0.05,
    #        xpd = TRUE,
    #        title = "\nRegion",
    #        title.cex = 0.7,
    #        title.adj = 0)
    
    legend(x = par('usr')[2], 
           y = par('usr')[4]/par('plt')[4],
           xjust = 0,
           # x = par('usr')[2]/par('plt')[2],
           # y = par('usr')[4]/par('plt')[4]-1*.,
           # xjust = 0.625,
           yjust = 1,
           adj = c(0,0.5),
           # x.intersp = 5.5,
           # "topright",
           # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
           legend=c("All US", "Midwest", "Northeast", "South", "West"),
           # pch = rep(15,5),
           fill = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]),
           cex = 0.8,
           pt.cex = rep(1,5),
           ncol = 1,
           bty = "n",
           # inset=-0.05,
           xpd = TRUE,
           title = "Optimized\nDistance\nThreshold",
           title.cex = 0.9,
           title.adj = 0
           ) %>% 
      .$rect %>% .$h %>%
      legend(x = par('usr')[2],
             y = par('usr')[4]/par('plt')[4]-1*.,xjust = 0,
             # x = par('usr')[2]/par('plt')[2], 
             # y = par('usr')[4]/par('plt')[4],
             # xjust = 1, 
             # yjust = 1,
             # adj = c(0,0.5),
             # xjust = 1, 
             yjust = 1,
             adj = c(1,0.5),
             x.intersp = 3,
             # "topright", 
             # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
             legend=c(as.expression(bquote('Q'[0])), 
                      as.expression(bquote('Q'[1])), 
                      as.expression(bquote('Q'[2])), 
                      as.expression(bquote('Q'[3])), 
                      as.expression(bquote('Q'[4]))), 
             pch = c(".", "-", "_", "-", "."), 
             col = "grey70", 
             fill = NA,
             lty = NA,
             border = NA,
             cex = 0.8, 
             pt.cex = 2, 
             bty = "n", 
             # inset=-0.05, 
             xpd = TRUE, 
             title = "Observed\nFlow\nDistances",
             title.cex = 0.9, 
             title.adj = 0)
    
    
    dev.off()
    
    
    
  }
  
}









png(filename = "./03-Output/02-Figures/figure_parameters_gravity_distance_thresholds_bymodelandobjscale.png", units = "in", res=300, width = 8.5, height = 13.5, pointsize=10)
par(mfrow = c(3,2), mar = c(5.1, 4.1, 2.1, 5.1))
# par(mfrow = c(3,2))

for(i in 1:length(dt)){
  
  me <- names(dt)[i]
  # par(mar = c(5.1, 6.1, 2.1, 4.1))
  
  for(j in 1:length(dt[[i]])){
    
    # if(j==2){
    #   par(mar = c(5.1, 4.1, 2.1, 5.1))
    # }
    
    os <- names(dt[[i]])[j] %>%
      toupper()%>%(\(x) paste0(substr(x,1,1), tolower(substr(x,2,nchar(x)))))
    
    
    tdt <- dt[[i]][[j]] %>%
      mutate(Period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE))
    barplot(distance_threshold~region+Period, 
            data=tdt, 
            beside=TRUE, 
            col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
            width = 2,
            ylim = c(0,10), 
            xpd = FALSE, 
            axes = FALSE,
            ylab = "",
            xlab = "",
            axisnames = FALSE
            # , legend.text = TRUE,
            # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
            # , plot = FALSE
    )
    abline(h = log(c(1,10,100,1000,10000)), 
           lty = 5, 
           col = "grey95")
    abline(h = log(c(2,5,20,50,200,500,2000,5000)), 
           lty = 3, 
           col = "grey90")
    title(#main = me, 
          main = paste0(me, "\n", os, " Objective Scale"), cex.main = 1)
    
    # if(j==1){
    #   mtext(me, side = 3, line = 3, at = par('usr')[2]/par('plt')[2], adj = 0.5, padj = 0.5)
    # }
    
    barplot(distance_threshold~region+Period, 
            data=tdt, 
            beside=TRUE, 
            col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
            width = 2,
            ylim = c(0,10), 
            xpd = FALSE, 
            axes = FALSE,
            ylab = "",
            xlab = "",
            axisnames = FALSE, 
            add = TRUE
            # , legend.text = TRUE,
            # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
            # , plot = FALSE
    ) %>%
      apply(., 
            MARGIN = 2, 
            (\(x) {
              axis(1, at = x, labels = c("US", "MW", "NE", "S", "W"), line = -1, tick = F, cex.axis = 0.8, gap.axis = 0.1)
              axis(1, at = range(x), labels = F, line = 1.25, tcl = 0.2);
              axis(1, at = mean(x), labels = F, line = 1.25, tcl = -0.2); 
              return(x)
            })) %>%
      (\(x) {
        axis(1, at = colMeans(x), labels = levels(tdt$Period), line = 0.5, tick = FALSE, cex.axis = 0.8);
        
        points(x=rep(unlist(x), 5), 
               y = od %>% 
                 filter(distance.km!=0) %>% 
                 bind_rows(mutate(., census.region.origin="All US")) %>% 
                 bind_rows(mutate(., period = "2011-2020")) %>% 
                 mutate(period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
                 arrange(period,census.region.origin)%>%
                 (\(y) {
                   
                   tapply(y, ~period+census.region.origin, (\(z) {
                     Hmisc::wtd.quantile(z$log.distance.km, weights = z$`Workers in Commuting Flow`, probs = c(0:4/4)) %>% t() %>% as.data.frame() %>%
                       bind_cols(data.frame(period=unique(z$period), region=unique(z$census.region.origin))) %>%
                       return()
                   }))
                   
                   
                 }) %>%
                 bind_rows() %>%
                 arrange(period) %>%
                 select(-period,-region) %>%
                 unlist(), 
               pch = rep(c(".", "-", "_", "-", "."), each = length(x)),
               cex = 2.5,
               col = "grey70");
        # return(x)
      })
    
    
    
    axis(2, at = log(c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000)), labels = c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000), las = 2, cex.axis = 0.8)
    axis(2, at = mean(log(c(25,50,100,200,400))), labels = "Distance (km)", las = 3, line = 1.75, tick = FALSE)
    
    if(j==2){
      
      legend(x = par('usr')[2], 
             y = par('usr')[4]/par('plt')[4],
             xjust = 0,
             # x = par('usr')[2]/par('plt')[2],
             # y = par('usr')[4]/par('plt')[4]-1*.,
             # xjust = 0.625,
             yjust = 1,
             adj = c(0,0.5),
             # x.intersp = 5.5,
             # "topright",
             # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
             legend=c("All US", "Midwest", "Northeast", "South", "West"),
             # pch = rep(15,5),
             fill = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]),
             cex = 0.8,
             pt.cex = rep(1,5),
             ncol = 1,
             bty = "n",
             # inset=-0.05,
             xpd = TRUE,
             title = "Optimized\nDistance\nThreshold",
             title.cex = 0.9,
             title.adj = 0
      ) %>% 
        .$rect %>% .$h %>%
        legend(x = par('usr')[2],
               y = par('usr')[4]/par('plt')[4]-1*.,xjust = 0,
               # x = par('usr')[2]/par('plt')[2], 
               # y = par('usr')[4]/par('plt')[4],
               # xjust = 1, 
               # yjust = 1,
               # adj = c(0,0.5),
               # xjust = 1, 
               yjust = 1,
               adj = c(1,0.5),
               x.intersp = 3,
               # "topright", 
               # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
               legend=c(as.expression(bquote('Q'[0])), 
                        as.expression(bquote('Q'[1])), 
                        as.expression(bquote('Q'[2])), 
                        as.expression(bquote('Q'[3])), 
                        as.expression(bquote('Q'[4]))), 
               pch = c(".", "-", "_", "-", "."), 
               col = "grey70", 
               fill = NA,
               lty = NA,
               border = NA,
               cex = 0.8, 
               pt.cex = 2, 
               bty = "n", 
               # inset=-0.05, 
               xpd = TRUE, 
               title = "Observed\nFlow\nDistances",
               title.cex = 0.9, 
               title.adj = 0)
      # .$rect %>% .$w %>%
      #   legend(x = par('usr')[2]/par('plt')[2]-1.2*.,
      #          y = par('usr')[4]/par('plt')[4],
      #          xjust = 1,
      #          yjust = 1,
      #          adj = c(0,0.5),
      #          # "topright",
      #          # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
      #          legend=c("All US", "Midwest", "Northeast", "South", "West"),
      #          # pch = rep(15,5),
      #          fill = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]),
      #          cex = 0.5,
      #          pt.cex = rep(1,5),
      #          ncol = 1,
      #          bty = "n",
      #          # inset=-0.05,
      #          xpd = TRUE,
      #          title = "\nRegion",
      #          title.cex = 0.7,
      #          title.adj = 0)
      
      
    }
    
    ### add letter to top left
    text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
         par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
         labels = LETTERS[1+1*(j-1)+2*(i-1)], adj = c(0,1), xpd = T, cex = 1, font = 2)
    
    
  }
  
}



##close out
dev.off()




## clean environment
rm(list = ls())
gc()









# #old
# dt <- dt$log
# 
# 
# dt <- dt %>% 
#   arrange(model.extent, period, region) %>%
#   mutate(px = case_when(period=="2011-2020" ~ 0, 
#                         period=="2011-2015" ~ 6, 
#                         period=="2016-2020" ~ 12, 
#                         TRUE ~ NA), 
#          mx = case_when(model.extent=="base*distance_threshold" ~ 0, 
#                         model.extent=="base*distance_threshold*large_populations" ~ 19, 
#                         model.extent=="base*distance_threshold*population_categories" ~ 38)) %>%
#   group_by(model.extent, period) %>%
#   mutate(rx = row_number()) %>%
#   ungroup() %>%
#   mutate(x = rx + px + mx) %>% 
#   bind_rows(., 
#             data.frame(distance_threshold=0, x=which(!(1:max(.$x)%in%.$x))))
# 
# barplot(matrix(0, length(1:max(dt$x))), ylim = c(3,7), xlim = c(1,max(dt$x)))
# barplot(distance_threshold~x, 
#         data=dt, 
#         width = 1,
#         space = 0,
#         ylim = c(3,7),
#         xpd = FALSE,
#         col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)])
#         # , add = TRUE
#         )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## create flow type variable
# od <- od %>% 
#   mutate(flow.type = case_when(distance.km==0 ~ "Intracounty", 
#                                `State FIPS Residence`==`State FIPS Work` ~ "Intrastate", 
#                                census.region.origin==census.region.destination ~ "Intraregion", 
#                                TRUE ~ "Interregion"))
# 
# ## summarise by flow type and origin location
# od.ft <- od %>%
#   group_by(period, `County FIPS Residence`, `County Residence`, `State FIPS Residence`, `State Residence`, POPESTIMATE.origin, census.region.origin, flow.type) %>%
#   summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`)) %>%
#   ungroup() %>%
#   group_by(period, `County FIPS Residence`, `County Residence`, `State FIPS Residence`, `State Residence`) %>%
#   mutate(Total.Commuters = sum(`Workers in Commuting Flow`)) %>%
#   ungroup() %>%
#   arrange(census.region.origin)
# 
# ### summarise all the way by origin location
# od.commuters <- od %>%
#   group_by(period, `County FIPS Residence`, `County Residence`, `State FIPS Residence`, `State Residence`, POPESTIMATE.origin, census.region.origin, Total.Workers.Residence, Total.Workers.Residence.Domestic) %>%
#   summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`)) %>%
#   ungroup() %>%
#   arrange(census.region.origin)
# 
# 
# 
# ## plot
# ### save figure
# png("./03-Output/02-Figures/commuters_vs_population_by_flowtype.png", units = "in", res = 300, width = 8.5, height = 15, pointsize = 10)
# 
# par(mfrow = c(5,2), mar = c(5.1,5.1,1.1,1.1))
# 
# 
# ### total commuters vs population origin
# plot(x=log(od.commuters$POPESTIMATE.origin), 
#      y=log(od.commuters$`Workers in Commuting Flow`), 
#      pch = 16, 
#      col = rgb(0.1,0.1,0.1,alpha = 0.05), 
#      axes = F, 
#      xlab = "", 
#      ylab = "", 
#      xlim = log(c(50,11000000)), 
#      ylim = c(4,log(max(od.commuters$`Workers in Commuting Flow`))))
# 
# 
# axis(1, 
#      at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
#      # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
#      labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
#      las = 1,
#      line = 0, 
#      tcl = -0.2)
# 
# axis(2, 
#      at = log(c(50,100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000)), 
#      # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
#      labels = c(50,100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m"),
#      las = 1,
#      line = 0, 
#      tcl = -0.2)
# 
# title(xlab = "Resident Population")
# title(ylab = "Total Commuters", line = 3.5)
# 
# box()
# 
# 
# 
# ### add loess lines
# loess.fit <- loess(log(od.commuters$`Workers in Commuting Flow`)~log(od.commuters$POPESTIMATE.origin))
# 
# lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#       col = "black", 
#       lty = 1, 
#       lwd = 2)
# 
# #### for each census region
# for(i in 1:4){
#   loess.fit <- loess(log(od.commuters$`Workers in Commuting Flow`)[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])]~log(od.commuters$POPESTIMATE.origin)[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])])
#   
#   lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#         col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
#         lty = c(2:5)[i], 
#         lwd = 3)
# }
# 
# ### legend
# legend("topleft", 
#        legend = c("All US", unique(od.commuters$census.region.origin)), 
#        col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
#        lty = c(1, c(2:5)), 
#        lwd = 3, 
#        seg.len = 4)
# ### add letter to top left
# text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#      par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#      labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)
# 
# 
# 
# ### population origin proportion
# plot(x=log(od.commuters$POPESTIMATE.origin), 
#      y=od.commuters$`Workers in Commuting Flow`/od.commuters$POPESTIMATE.origin, 
#      pch = 16, 
#      col = rgb(0.1,0.1,0.1,alpha = 0.1), 
#      axes = F, 
#      xlab = "", 
#      ylab = "", 
#      xlim = log(c(50,11000000)), 
#      ylim = c(0,1))
# 
# 
# axis(1, 
#      at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
#      # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
#      labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
#      las = 1,
#      line = 0, 
#      tcl = -0.2)
# 
# axis(2, 
#      at = 0:10/10, 
#      las = 1,
#      line = 0, 
#      tcl = -0.2)
# 
# title(xlab = "Resident Population")
# title(ylab = "Proportion of Resident Population who Commute", line = 3.5)
# 
# box()
# 
# 
# 
# ### add loess lines
# loess.fit <- loess(od.commuters$`Workers in Commuting Flow`/od.commuters$POPESTIMATE.origin~log(od.commuters$POPESTIMATE.origin))
# 
# lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#       col = "black", 
#       lty = 1, 
#       lwd = 2)
# 
# #### for each census region
# for(i in 1:4){
#   loess.fit <- loess(od.commuters$`Workers in Commuting Flow`[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])]/od.commuters$POPESTIMATE.origin[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])]~log(od.commuters$POPESTIMATE.origin)[which(od.commuters$census.region.origin==unique(od.commuters$census.region.origin)[i])])
#   
#   lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#         col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
#         lty = c(2:5)[i], 
#         lwd = 3)
# }
# 
# # ### legend
# # legend("topright", 
# #        legend = c("All US", unique(od.commuters$census.region.origin)), 
# #        col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
# #        lty = c(1, c(2:5)), 
# #        lwd = 3, 
# #        seg.len = 4)
# ### add letter to top left
# text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#      par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#      labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# fts <- c("Intracounty", "Intrastate", "Intraregion", "Interregion")
# fts.who <- c("within Their Resident County", "within Their Resident State", "within Their Resident Census Region", "outside Their Resident Census Region")
# 
# for(ii in 1:4){
#   
#   od.sub <- od.ft %>%
#     filter(flow.type==fts[ii])
#   
#   ### total commuters vs population origin
#   plot(x=log(od.sub$POPESTIMATE.origin), 
#        y=log(od.sub$`Workers in Commuting Flow`), 
#        pch = 16, 
#        col = rgb(0.1,0.1,0.1,alpha = 0.05), 
#        axes = F,
#        xlab = "", 
#        ylab = "", 
#        xlim = log(c(50,11000000)),
#        ylim = log(range(od.sub$`Workers in Commuting Flow`))
#        )
#   
#   
#   axis(1, 
#        at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
#        # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
#        labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
#        las = 1,
#        line = 0, 
#        tcl = -0.2)
#   
#   axis(2, 
#        at = log(c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000)), 
#        # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
#        labels = c(1,2,5,10,20,50,100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m"),
#        las = 1,
#        line = 0, 
#        tcl = -0.2)
#   
#   title(xlab = "Resident Population")
#   title(ylab = paste0("Total ", fts[ii], " Commuters"), line = 3.5)
#   
#   box()
#   
#   
#   
#   ### add loess lines
#   loess.fit <- loess(log(od.sub$`Workers in Commuting Flow`)~log(od.sub$POPESTIMATE.origin))
#   
#   lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#         col = "black", 
#         lty = 1, 
#         lwd = 2)
#   
#   #### for each census region
#   for(i in 1:4){
#     loess.fit <- loess(log(od.sub$`Workers in Commuting Flow`)[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])]~log(od.sub$POPESTIMATE.origin)[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])])
#     
#     lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#           col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
#           lty = c(2:5)[i], 
#           lwd = 3)
#   }
#   
#   # ### legend
#   # legend("topleft", 
#   #        legend = c("All US", unique(od.sub$census.region.origin)), 
#   #        col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
#   #        lty = c(1, c(2:5)), 
#   #        lwd = 3, 
#   #        seg.len = 4)
#   ### add letter to top left
#   text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#        par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#        labels = LETTERS[c(-1,-2)][1+(ii-1)*2], adj = c(0,1), xpd = T, cex = 1, font = 2)
#   
#   
#   
#   ### population origin proportion
#   plot(x=log(od.sub$POPESTIMATE.origin), 
#        y=od.sub$`Workers in Commuting Flow`/od.sub$POPESTIMATE.origin, 
#        pch = 16, 
#        col = rgb(0.1,0.1,0.1,alpha = 0.1), 
#        axes = F, 
#        xlab = "", 
#        ylab = "", 
#        xlim = log(c(50,11000000)), 
#        ylim = c(0,max((od.sub$`Workers in Commuting Flow`/od.sub$POPESTIMATE.origin)[which(od.sub$`Workers in Commuting Flow`<od.sub$POPESTIMATE.origin)])))
#   
#   
#   axis(1, 
#        at = log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000,5000000,10000000)), 
#        # labels = format(c(1,10,100,1000,10000,100000,500000), scientific = FALSE),
#        labels = c(100,200,500,1000,2000, "5k", "10k", "20k", "50k", "100k", "200k", "500k", "1m", "2m", "5m", "10m"),
#        las = 1,
#        line = 0, 
#        tcl = -0.2)
#   
#   axis(2, 
#        at = 0:20/20, 
#        las = 1,
#        line = 0, 
#        tcl = -0.2)
#   
#   title(xlab = "Resident Population")
#   title(ylab = paste0("Proportion Commuting ", fts.who[ii]), line = 3.5)
#   
#   box()
#   
#   
#   
#   ### add loess lines
#   loess.fit <- loess(od.sub$`Workers in Commuting Flow`/od.sub$POPESTIMATE.origin~log(od.sub$POPESTIMATE.origin))
#   
#   lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#         col = "black", 
#         lty = 1, 
#         lwd = 2)
#   
#   #### for each census region
#   for(i in 1:4){
#     loess.fit <- loess(od.sub$`Workers in Commuting Flow`[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])]/od.sub$POPESTIMATE.origin[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])]~log(od.sub$POPESTIMATE.origin)[which(od.sub$census.region.origin==unique(od.sub$census.region.origin)[i])])
#     
#     lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], 
#           col = brewer.pal(8, "Dark2")[c(1,3,4,7)][i], 
#           lty = c(2:5)[i], 
#           lwd = 3)
#   }
#   
#   # ### legend
#   # legend("topright", 
#   #        legend = c("All US", unique(od.sub$census.region.origin)), 
#   #        col = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
#   #        lty = c(1, c(2:5)), 
#   #        lwd = 3, 
#   #        seg.len = 4)
#   ### add letter to top left
#   text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#        par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#        labels = LETTERS[c(-1,-2)][2+(ii-1)*2], adj = c(0,1), xpd = T, cex = 1, font = 2)
#   
#   
# }










