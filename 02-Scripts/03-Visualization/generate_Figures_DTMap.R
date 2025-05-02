# Script to create figure showing total commuters vs population at origin

## load data
load("./01-Data/02-Analytic-Data/od.rds")
load("./01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")

load("./01-Data/01-Processed-Data/county_centers.rds")


## packages
library(dplyr)
library(RColorBrewer)
library(sf)





## load shapefile
county <- st_read("./01-Data/00-Raw-Data/Spatial/cb_2018_us_county_5m.shp")


# ak hi pr vi 
ak <- "02"
hi <- "15"
pr <- "72"
others <- c("78", "60", "66" , "69")

notmain <- c(ak, hi, pr, others)


mainland <- county %>% 
  filter(!STATEFP%in%notmain) %>%
  left_join(., 
            centers %>%
              st_drop_geometry() %>% 
              filter(YEAR%in%c(2010)), 
            by = c("STATEFP", "COUNTYFP")) %>%
  #oglala lakota sd vs shannon sd
  mutate(Region = ifelse(STATEFP=="46"&COUNTYFP=="102", 
                         "Midwest",
                         Region), 
         STNAME = ifelse(STATEFP=="46"&COUNTYFP=="102", 
                         "South Dakota",
                         STNAME))%>% 
  arrange(STATEFP, COUNTYFP)



cenreg <- mainland %>% 
  group_by(Region) %>%
  summarise(do_union = TRUE) %>%
  ungroup()





tdt <- distance.thresholds %>% 
  filter(model.extent%in%c("base*distance_threshold*large_populations") & objective.scale%in%c("log")) %>% 
  mutate(Period = factor(period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE))





# png(filename = "./03-Output/02-Figures/main_figure_dtandcountymapex.png", units = "in", res=300, width = 9, height = 6, pointsize=10)

svg(filename = "./03-Output/02-Figures/main_figure_dtandcountymapex.svg", width = 9, height = 4.6, pointsize=10)



{

layout(mat = matrix(data = c(1,1,2,3,4,2,3,5,2), byrow = TRUE, ncol = 3), heights = c(5,1.5,1.5), widths = c(2,1,5))
# layout.show(n=5)




mycols <- brewer.pal(8, "Dark2")[c(1,3,4,7)]

mycolsmuted <- sapply(mycols, \(x){colorRampPalette(c(x,"white"))(10)[7]})
# mycolsmuted <- adjustcolor(brewer.pal(8, "Dark2")[c(1,3,4,7)], alpha.f = 0.25)







{
  
  
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
          # , horiz = T, xlim = c(0,10)
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
          # lwd = 10,
          # border = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 
          # col = c("grey90", mycolsmuted),
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
         title = " Optimized\n Distance\n Threshold",
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
           title = " Observed\n Flow\n Distances",
           title.cex = 0.9, 
           title.adj = 0)
  
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "A", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
  box()
  box("fig")
}









{
  par(mar = c(0,0,0,0))
  plot(mainland$geometry, border = "gainsboro", col = mycolsmuted[match(mainland$Region,c("Midwest", "Northeast", "South", "West"))]
       # , 
       # setParUsrBB = TRUE, 
       # xlim = c(-126,-66), ylim = c(23,50)
  )
  
  # #to find examples
  # View(distance.thresholds%>%
  #   filter(model.extent%in%c("base*distance_threshold*large_populations") & 
  #            objective.scale%in%c("log") &
  #            period%in%c("2011-2020"))%>%
  #   select(region, distance_threshold)%>%
  #   mutate(dtkm=exp(distance_threshold)))
  # 
  # View(od %>%
  #        filter(census.region.origin=="Midwest" &
  #                 distance.km < 121 &
  #                 distance.km > 119))
  
  
  #Indiana
  # mainland[which(mainland$STATEFP%in%c("18")&mainland$COUNTYFP%in%c("095", "003")),]
  
  a <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("18") & mainland$COUNTYFP%in%c("095"))]))
  b <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("18") & mainland$COUNTYFP%in%c("003"))]))
  
  segments(x0=a[1],y0=a[2],x1=b[1],y1=b[2], col =  mycols[1])
  
  plot(mainland$geometry[which(mainland$STATEFP%in%c("18") & mainland$COUNTYFP%in%c("095", "003"))], 
       add = TRUE, 
       col =  mycols[1], 
       border = mycols[1])
  
  
  
  
  #Oneida & Albany New York
  # mainland[which(mainland$STATEFP%in%c("36")&mainland$COUNTYFP%in%c("065", "001")),]
  
  a <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("36") & mainland$COUNTYFP%in%c("065"))]))
  b <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("36") & mainland$COUNTYFP%in%c("001"))]))
  
  segments(x0=a[1],y0=a[2],x1=b[1],y1=b[2], col =  mycols[2])
  
  plot(mainland$geometry[which(mainland$STATEFP%in%c("36") & mainland$COUNTYFP%in%c("065", "001"))], 
       add = TRUE, 
       col =  mycols[2], 
       border = mycols[2])
  
  
  
  #Clarke & Richmond Georgia
  # mainland[which(mainland$STATEFP%in%c("13")&mainland$COUNTYFP%in%c("059", "245")),]
  a <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("13") & mainland$COUNTYFP%in%c("059"))]))
  b <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("13") & mainland$COUNTYFP%in%c("245"))]))
  
  segments(x0=a[1],y0=a[2],x1=b[1],y1=b[2], 
           col =  mycols[3])
  
  plot(mainland$geometry[which(mainland$STATEFP%in%c("13") & mainland$COUNTYFP%in%c("059", "245"))], 
       add = TRUE, 
       col =  mycols[3], 
       border = mycols[3])
  
  #Fresno & San Joaquin California
  # mainland[which(mainland$STATEFP%in%c("06")&mainland$COUNTYFP%in%c("019", "077")),]
  a <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("06") & mainland$COUNTYFP%in%c("019"))]))
  b <- st_coordinates(st_centroid(mainland$geometry[which(mainland$STATEFP%in%c("06") & mainland$COUNTYFP%in%c("077"))]))
  
  segments(x0=a[1],y0=a[2],x1=b[1],y1=b[2], 
           col =  mycols[4])
  
  plot(mainland$geometry[which(mainland$STATEFP%in%c("06") & mainland$COUNTYFP%in%c("019", "077"))], 
       add = TRUE, 
       col =  mycols[4], 
       border = mycols[4])
  

  
  
  plot(cenreg$geometry, border = mycols, add = TRUE, lwd = 1.5)
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = "B", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
  
  box()
  
}

# View(od %>%
#        filter(((`State FIPS Residence`%in%c("18")&`County FIPS Residence`%in%c("095")&
#                     `State FIPS Work`%in%c("18")&     `County FIPS Work`%in%c("003")) | 
#                 
#                 (`State FIPS Residence`%in%c("36")&`County FIPS Residence`%in%c("065")&
#                       `State FIPS Work`%in%c("36")&     `County FIPS Work`%in%c("001")) |
#                 
#                 (`State FIPS Residence`%in%c("13")&`County FIPS Residence`%in%c("059")&
#                       `State FIPS Work`%in%c("13")&     `County FIPS Work`%in%c("245")) |
#                 
#                 (`State FIPS Residence`%in%c("06")&`County FIPS Residence`%in%c("019")&
#                       `State FIPS Work`%in%c("06")&     `County FIPS Work`%in%c("077")) | 
#                 
#                 (`State FIPS Residence`%in%c("02")&`County FIPS Residence`%in%c("170")&
#                       `State FIPS Work`%in%c("02")&     `County FIPS Work`%in%c("090")) |
#                 
#                 (`State FIPS Residence`%in%c("15")&`County FIPS Residence`%in%c("003")&
#                       `State FIPS Work`%in%c("15")&     `County FIPS Work`%in%c("007")) |
#                 
#                 (`State FIPS Residence`%in%c("72")&`County FIPS Residence`%in%c("153")&
#                       `State FIPS Work`%in%c("72")&     `County FIPS Work`%in%c("147"))) & period%in%c("2011-2015")))




flow.legend <- od %>%
  filter(((`State FIPS Residence`%in%c("18")&`County FIPS Residence`%in%c("095")&
             `State FIPS Work`%in%c("18")&     `County FIPS Work`%in%c("003")) | 
            
            (`State FIPS Residence`%in%c("36")&`County FIPS Residence`%in%c("065")&
               `State FIPS Work`%in%c("36")&     `County FIPS Work`%in%c("001")) |
            
            (`State FIPS Residence`%in%c("13")&`County FIPS Residence`%in%c("059")&
               `State FIPS Work`%in%c("13")&     `County FIPS Work`%in%c("245")) |
            
            (`State FIPS Residence`%in%c("06")&`County FIPS Residence`%in%c("019")&
               `State FIPS Work`%in%c("06")&     `County FIPS Work`%in%c("077")) | 
            
            (`State FIPS Residence`%in%c("02")&`County FIPS Residence`%in%c("170")&
               `State FIPS Work`%in%c("02")&     `County FIPS Work`%in%c("090")) |
            
            (`State FIPS Residence`%in%c("15")&`County FIPS Residence`%in%c("003")&
               `State FIPS Work`%in%c("15")&     `County FIPS Work`%in%c("007")) |
            
            (`State FIPS Residence`%in%c("72")&`County FIPS Residence`%in%c("153")&
               `State FIPS Work`%in%c("72")&     `County FIPS Work`%in%c("147"))) & 
           
           period%in%c("2011-2015")) %>% 
  mutate(flow = paste0(`County Residence`, " & ", `County Work`, ", ", `State Residence`, ", ", format(distance.km, digits = 1), "km")) %>% 
  select(flow) %>% 
  unlist()

legend("bottomleft", 
       legend = flow.legend, 
       col = mycols[c(4,4,3,4,1,2,2)], 
       lty = 1, 
       lwd = 2, 
       bty = 'n', 
       cex = 0.8)





# ak hi pr



nmainland <- county %>% 
  filter(STATEFP%in%c(ak,hi,pr)) %>%
  left_join(., 
            centers %>%
              st_drop_geometry() %>% 
              filter(YEAR%in%c(2010)), 
            by = c("STATEFP", "COUNTYFP")) %>%
  #kusilvak ak
  mutate(Region = ifelse(STATEFP=="02"&COUNTYFP=="158",
                         "West",
                         Region),
         STNAME = ifelse(STATEFP=="02"&COUNTYFP=="158",
                         "Alaska",
                         STNAME))%>%
  arrange(STATEFP, COUNTYFP)


# View(distance.thresholds%>%
#   filter(model.extent%in%c("base*distance_threshold*large_populations") & 
#            objective.scale%in%c("log") &
#            period%in%c("2011-2020"))%>%
#   select(region, distance_threshold)%>%
#   mutate(dtkm=exp(distance_threshold)))
# 
# View(od %>%
#        filter(`State FIPS Residence`%in%c(ak) & `State FIPS Work`%in%c(ak)
#               # ,
#               #   distance.km < 200 &
#               #   distance.km > 150
#               ))
# ak 02 198 220
# hi 15 003 007
# pr 72 153 147


par(mar = c(0,0,0,0))
plot(nmainland$geometry[which(nmainland$STATEFP%in%c("02")&!nmainland$COUNAME%in%c("Aleutians West"))], border = "gainsboro", col = mycolsmuted[match(nmainland$Region,c("Midwest", "Northeast", "South", "West"))]
     
)
plot(nmainland$geometry[which(nmainland$STATEFP%in%c("02"))], 
     border = "gainsboro", 
     col = mycolsmuted[match(nmainland$Region,c("Midwest", "Northeast", "South", "West"))], 
     add = TRUE
     
)

a <- st_coordinates(st_centroid(nmainland$geometry[which(nmainland$STATEFP%in%c("02") & nmainland$COUNTYFP%in%c("170"))]))
b <- st_coordinates(st_centroid(nmainland$geometry[which(nmainland$STATEFP%in%c("02") & nmainland$COUNTYFP%in%c("090"))]))

segments(x0=a[1],y0=a[2],x1=b[1],y1=b[2], 
         col =  mycols[4])

plot(nmainland$geometry[which(nmainland$STATEFP%in%c("02")&nmainland$COUNTYFP%in%c("170", "090"))], 
     add = TRUE, 
     col =  mycols[4], 
     # border = mycols[4]
     border = "gainsboro"
     )

### add letter to top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "C", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)

box()

par(mar = c(0,0,0,0))
plot(nmainland$geometry[which(nmainland$STATEFP%in%c("15"))], border = "gainsboro", col = mycolsmuted[match(nmainland$Region,c("Midwest", "Northeast", "South", "West"))]
     # ,
     # setParUsrBB = TRUE,
     # xlim = c(-180,-127)#, ylim = c(23,50)
)

a <- st_coordinates(st_centroid(nmainland$geometry[which(nmainland$STATEFP%in%c("15") & nmainland$COUNTYFP%in%c("003"))]))
b <- st_coordinates(st_centroid(nmainland$geometry[which(nmainland$STATEFP%in%c("15") & nmainland$COUNTYFP%in%c("007"))]))

segments(x0=a[1],y0=a[2],x1=b[1],y1=b[2], 
         col =  mycols[4])

plot(nmainland$geometry[which(nmainland$STATEFP%in%c("15")&nmainland$COUNTYFP%in%c("003", "007"))], 
     add = TRUE, 
     col =  mycols[4], 
     # border = mycols[4]
     border = "gainsboro"
)

### add letter to top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "D", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)

box()


par(mar = c(0,0,0,0))
plot(nmainland$geometry[which(nmainland$STATEFP%in%c("72"))], border = "gainsboro", col = mycolsmuted[match(nmainland$Region[which(nmainland$STATEFP%in%c("72"))],c("Midwest", "Northeast", "South", "West"))]
     # ,
     # setParUsrBB = TRUE,
     # xlim = c(-180,-127)#, ylim = c(23,50)
)

a <- st_coordinates(st_centroid(nmainland$geometry[which(nmainland$STATEFP%in%c("72") & nmainland$COUNTYFP%in%c("153"))]))
b <- st_coordinates(st_centroid(nmainland$geometry[which(nmainland$STATEFP%in%c("72") & nmainland$COUNTYFP%in%c("147"))]))

segments(x0=a[1],y0=a[2],x1=b[1],y1=b[2], 
         col =  mycols[2])

plot(nmainland$geometry[which(nmainland$STATEFP%in%c("72")&nmainland$COUNTYFP%in%c("153", "147"))], 
     add = TRUE, 
     col =  mycols[2], 
     # border = mycols[4]
     border = "gainsboro"
)

### add letter to top left
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "E", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)

box()















}


dev.off()





## clean environment
rm(list = ls())
gc()

