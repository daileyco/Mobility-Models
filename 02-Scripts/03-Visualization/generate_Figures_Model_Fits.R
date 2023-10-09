# Script to create figure showing total commuters vs population at origin

## load data
load("./03-Output/01-Tables/tables_fits_gravity.rdata")
load("./03-Output/01-Tables/table_fits_uo.rds")

## packages
library(dplyr)
library(tidyr)
library(RColorBrewer)


## cleaning

table.gravity.fits <- table.gravity.fits %>%
  mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  mutate(Model = "G2") %>%
  select(Period, Model, everything()) %>%
  arrange(Period, Model) %>%
  mutate(across(!Period, ~as.character(.x)))



table.gravity.fits.simple <- table.gravity.fits.simple %>%
  mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  mutate(Model = "G1") %>%
  select(Period, Model, everything()) %>%
  arrange(Period, Model) %>%
  mutate(across(!Period, ~as.character(.x)))



table.uo.fits <- table.uo.fits %>%
  mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
  select(Period, `Surrounding Population, s_{ij}`,`Total Commuters, T_{i}`, Model, everything()) %>%
  mutate(Model = factor(Model, levels = c("UO", "OO", "OPS", "Radiation"), labels = c("UO", "OO", "OPS", "Rad"), ordered = TRUE)) %>%
  arrange(Period, `Surrounding Population, s_{ij}`,`Total Commuters, T_{i}`, Model) %>%
  mutate(across(!Period, ~as.character(.x))) %>%
  filter(`Surrounding Population, s_{ij}`=="Comprehensive, Census Data" &
           `Total Commuters, T_{i}`=="Observed" &
           `Objective Scale for Tuning`%in%c("", "log"))%>%
  select(-`Surrounding Population, s_{ij}`,
         -`Total Commuters, T_{i}`,
         -`Objective Scale for Tuning`)


# table.uo.fits1 <- table.uo.fits %>%
#   mutate(Period = factor(Period, levels = c("2011-2020", "2011-2015", "2016-2020"), ordered = TRUE)) %>%
#   select(Period, `Surrounding Population, s_{ij}`,`Total Commuters, T_{i}`, Model, everything()) %>%
#   mutate(Model = factor(Model, levels = c("UO", "OO", "OPS", "Radiation"), labels = c("UO", "OO", "OPS", "Rad"), ordered = TRUE)) %>%
#   bind_rows(., 
#             filter(., `Objective Scale for Tuning`=="")%>%
#               mutate(`Objective Scale for Tuning`="log"), 
#             filter(., `Objective Scale for Tuning`=="")%>%
#               mutate(`Objective Scale for Tuning`="identity")) %>%
#   filter(`Objective Scale for Tuning`%in%c("log", "identity")) %>%
#   arrange(Period, `Surrounding Population, s_{ij}`,`Total Commuters, T_{i}`, Model) %>%
#   mutate(across(!Period, ~as.character(.x))) %>%
#   split(., f = ~`Surrounding Population, s_{ij}`
#         +`Total Commuters, T_{i}`
#         +`Objective Scale for Tuning`)






## combine into list

fits <- bind_rows(table.gravity.fits,
                  table.gravity.fits.simple,
                  table.uo.fits)%>%
  arrange(Period) %>%
  filter(Parameter %in% c("RMSE_identity", "RMSE_log")) %>%
  pivot_longer(c(`All US`, Midwest, Northeast, South, West), names_to = "Region", values_to = "RMSE") %>%
  mutate(Scale = sub("RMSE_", "", Parameter),
         RMSE = as.numeric(RMSE)) %>%
  select(-Parameter) %>%
  mutate(across(c(Period, Model, Region), ~factor(.x))) %>%
  arrange(Period, Model, Region) %>%
  # pivot_wider(names_from = Model, values_from = RMSE) %>%
  split(., f = ~Period+Scale)




for(i in 1:length(fits)){
  
  png(filename = paste0("./03-Output/02-Figures/figure_fits_by_model_", sub("[.]", "_", names(fits)[i]), ".png"), units = "in", res=300, width = 13.5, height = 4.5, pointsize=10)
  
  par(mar = c(5.1, 4.1, 7.1, 7.1))
  
  
  tuo <- fits[[i]]
  barplot(RMSE~Model+Region, 
          data=tuo, 
          beside=TRUE, 
          width=1,
          space=c(0.125,1),
          
          col = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), each = 6),
          # col = NA, 
          # border = NA,
          ylim = if(grepl("identity", names(fits)[i])){c(0,40000)}else{c(0,4)}, 
          xpd = FALSE, 
          axes = FALSE,
          ylab = "",
          xlab = "",
          axisnames = FALSE
          # , legend.text = TRUE,
          # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
          # , plot = FALSE
  )
  if(grepl("identity", names(fits)[i])){
    
    abline(h = seq(0,40000,by=5000)[which(!seq(0,40000,by=5000)%in%seq(0,40000,by=10000))], 
           lty = 3, 
           col = "grey95", 
           lwd = 0.75)
    abline(h = seq(0,40000,by=10000), 
           lty = 5, 
           col = "grey90", 
           lwd = 0.75)
    
    axis(2, at = seq(0,40000,by=5000), labels = paste0(seq(0,40,by=5), "k"), las = 2, cex.axis = 0.8)
    axis(2, at = mean(par('usr')[3:4]), labels = "Root Mean Square Error of Commuters Predicted", las = 3, line = 1.75, tick = FALSE, cex.axis = 0.8)
    
    
  }
  if(grepl("log", names(fits)[i])){
    
    abline(h = seq(0,4,by=0.5)[which(!seq(0,4,by=0.5)%in%seq(0,4,by=1))], 
           lty = 3, 
           col = "grey95", 
           lwd = 0.75)
    abline(h = seq(0,4,by=1), 
           lty = 5, 
           col = "grey90", 
           lwd = 0.75)
    
    axis(2, at = seq(0,4,by=0.5), las = 2, cex.axis = 0.8)
    axis(2, at = mean(par('usr')[3:4]), labels = "Root Mean Square Error of log(Commuters) Predicted", las = 3, line = 1.75, tick = FALSE, cex.axis = 0.8)
    
    
  }
  
  barplot(RMSE~Model+Region, 
          data=tuo, 
          beside=TRUE, 
          width=1,
          space=c(0.125,1),
          add=TRUE,
          col = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), each = 6),
          # density = rep(c(25,75,25,50,75,100), 6),
          # angle = rep(c(0,0,45,135,135,135), 6),
          # col = NA,
          # border = NA,
          # ylim = c(0,1), 
          xpd = FALSE, 
          axes = FALSE,
          ylab = "",
          xlab = "",
          axisnames = FALSE
          # , legend.text = TRUE,
          # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
          # , plot = FALSE
  )
  barplot(RMSE~Model+Region, 
          data=tuo, 
          beside=TRUE, 
          width=1,
          space=c(0.125,1),
          add=TRUE,
          # col = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), each = 6),
          density = rep(c(0,25,10,20,30,40), 6),
          angle = rep(c(0,0,45,135,135,135), 6),
          col = "white",
          # lwd = 0.1,
          # border = NA,
          # ylim = c(0,1), 
          xpd = FALSE, 
          axes = FALSE,
          ylab = "",
          xlab = "",
          axisnames = FALSE
          # , legend.text = TRUE,
          # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
          # , plot = FALSE
  ) %>%
    apply(., 
          MARGIN = 2, 
          (\(x) {
            axis(1, at = x, labels = c(as.expression(bquote("G"[1])), as.expression(bquote("G"[2])), "UO", "OO", "OPS", "Rad"), line = -1, tick = F, cex.axis = 0.8, gap.axis = 0, padj = 1)
            axis(1, at = range(x), labels = F, line = 2.25, tcl = 0.2);
            axis(1, at = mean(x), labels = F, line = 2.25, tcl = -0.2, xpd=TRUE); 
            return(x)
          })) %>%
    (\(x) {
      axis(1, at = colMeans(x), labels = c("US", "MW", "NE", "S", "W"), line = 1.5, tick = FALSE, cex.axis = 0.8);
      
      # return(x)
    })
  
  
  
  legend(x = par('usr')[2],
         y = par('usr')[4]/par('plt')[4],
         xjust = 0.5,
         yjust = 1,
         adj = c(0.5,0.5),
         x.intersp = -0.75, 
         y.intersp = 0.75,
         # "topright",
         # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
         legend=c(rep(NA,30)),
         # legend=c(rep(NA,5),c("All US", "Midwest", "Northeast", "South", "West")),
         # pch = rep(15,5),
         fill = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 6),
         density = rep(c(-1,25,10,20,30,40), each = 5),
         angle = rep(c(0,0,45,135,135,135), each = 5),
         col = "white",
         ncol = 6,
         cex = 1.25,
         bty = "n",
         # inset=-0.05,
         xpd = TRUE,
         title = "",
         title.cex = 0.9,
         title.adj = )%>%
    (\(x) {
      text(x=x$text$x[c(1,6,11,16,21,26)], 
           y=x$text$y[1]+(x$text$y[1]-x$text$y[2])/5*3, 
           adj = c(0,1),
           labels = c(as.expression(bquote("G"[1])), 
                      as.expression(bquote("G"[2])), 
                      "OO", "OPS", "Rad", "UO"),
           xpd = TRUE, 
           cex = 0.8, 
           srt = 90);
      text(x=x$text$x[26]+(x$text$x[26]-x$text$x[21]), 
           y=x$text$y[26:30], 
           adj = c(0,0.5),
           labels = c("All US", "Midwest", "Northeast", "South", "West"),
           xpd = TRUE, 
           cex = 0.8);
      # return(x)
    })
  
  
  title(main = paste0(sub("^([0-9]{4}[-][0-9]{4})[.].+$", 
                          "\\1", 
                          names(fits)[i]), 
                      "\n", 
                      sub("[0-9]{4}[-][0-9]{4}[.]", 
                         "", 
                         names(fits)[i])%>%
                       (\(x) {
                         paste0(toupper(substr(x,1,1)), substr(x,2, nchar(x))) %>% 
                           return()
                       }), 
                     " Objective Scale"
                     ), 
        font.main = 1, 
        adj = 0, 
        line = 2.5, 
        cex.main = 0.8)
  
  
  dev.off()
  
}











png(filename = paste0("./03-Output/02-Figures/figure_fits_by_model_all.png"), units = "in", res=300, width = 13.5, height = 9, pointsize=10)

par(mfcol = c(3,2), mar = c(5.1, 4.1, 7.1, 7.1))


for(i in 1:length(fits)){
  
  
  
  tuo <- fits[[i]]
  barplot(RMSE~Model+Region, 
          data=tuo, 
          beside=TRUE, 
          width=1,
          space=c(0.125,1),
          
          col = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), each = 6),
          # col = NA, 
          # border = NA,
          ylim = if(grepl("identity", names(fits)[i])){c(0,40000)}else{c(0,4)}, 
          xpd = FALSE, 
          axes = FALSE,
          ylab = "",
          xlab = "",
          axisnames = FALSE
          # , legend.text = TRUE,
          # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
          # , plot = FALSE
  )
  if(grepl("identity", names(fits)[i])){
    
    abline(h = seq(0,40000,by=5000)[which(!seq(0,40000,by=5000)%in%seq(0,40000,by=10000))], 
           lty = 3, 
           col = "grey95", 
           lwd = 0.75)
    abline(h = seq(0,40000,by=10000), 
           lty = 5, 
           col = "grey90", 
           lwd = 0.75)
    
    axis(2, at = seq(0,40000,by=5000), labels = paste0(seq(0,40,by=5), "k"), las = 2, cex.axis = 0.8)
    axis(2, at = mean(par('usr')[3:4]), labels = "Root Mean Square Error of Commuters Predicted", las = 3, line = 1.75, tick = FALSE, cex.axis = 0.8)
    
    
  }
  if(grepl("log", names(fits)[i])){
    
    abline(h = seq(0,4,by=0.5)[which(!seq(0,4,by=0.5)%in%seq(0,4,by=1))], 
           lty = 3, 
           col = "grey95", 
           lwd = 0.75)
    abline(h = seq(0,4,by=1), 
           lty = 5, 
           col = "grey90", 
           lwd = 0.75)
    
    axis(2, at = seq(0,4,by=0.5), las = 2, cex.axis = 0.8)
    axis(2, at = mean(par('usr')[3:4]), labels = "Root Mean Square Error of log(Commuters) Predicted", las = 3, line = 1.75, tick = FALSE, cex.axis = 0.8)
    
    
  }
  
  barplot(RMSE~Model+Region, 
          data=tuo, 
          beside=TRUE, 
          width=1,
          space=c(0.125,1),
          add=TRUE,
          col = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), each = 6),
          # density = rep(c(25,75,25,50,75,100), 6),
          # angle = rep(c(0,0,45,135,135,135), 6),
          # col = NA,
          # border = NA,
          # ylim = c(0,1), 
          xpd = FALSE, 
          axes = FALSE,
          ylab = "",
          xlab = "",
          axisnames = FALSE
          # , legend.text = TRUE,
          # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
          # , plot = FALSE
  )
  barplot(RMSE~Model+Region, 
          data=tuo, 
          beside=TRUE, 
          width=1,
          space=c(0.125,1),
          add=TRUE,
          # col = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), each = 6),
          density = rep(c(0,25,10,20,30,40), 6),
          angle = rep(c(0,0,45,135,135,135), 6),
          col = "white",
          # lwd = 0.1,
          # border = NA,
          # ylim = c(0,1), 
          xpd = FALSE, 
          axes = FALSE,
          ylab = "",
          xlab = "",
          axisnames = FALSE
          # , legend.text = TRUE,
          # args.legend = list(x="topleft", horiz=F, cex=0.8, inset = c(-0.1), xpd = T)
          # , plot = FALSE
  ) %>%
    apply(., 
          MARGIN = 2, 
          (\(x) {
            axis(1, at = x, labels = c(as.expression(bquote("G"[1])), as.expression(bquote("G"[2])), "UO", "OO", "OPS", "Rad"), line = -1, tick = F, cex.axis = 0.8, gap.axis = 0, padj = 1)
            axis(1, at = range(x), labels = F, line = 2.25, tcl = 0.2);
            axis(1, at = mean(x), labels = F, line = 2.25, tcl = -0.2, xpd=TRUE); 
            return(x)
          })) %>%
    (\(x) {
      axis(1, at = colMeans(x), labels = c("US", "MW", "NE", "S", "W"), line = 1.5, tick = FALSE, cex.axis = 0.8);
      
      # return(x)
    })
  
  
  
  if(i%in%c(4,5,6)){
    legend(x = par('usr')[2],
           y = par('usr')[4]/par('plt')[4],
           xjust = 0.5,
           yjust = 1,
           adj = c(0.5,0.5),
           x.intersp = -0.75, 
           y.intersp = 0.75,
           # "topright",
           # parse(text=paste0("as.expression(bquote('Q'[",0:4,"]))"))
           legend=c(rep(NA,30)),
           # legend=c(rep(NA,5),c("All US", "Midwest", "Northeast", "South", "West")),
           # pch = rep(15,5),
           fill = rep(c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)]), 6),
           density = rep(c(-1,25,10,20,30,40), each = 5),
           angle = rep(c(0,0,45,135,135,135), each = 5),
           col = "white",
           ncol = 6,
           cex = 1.25,
           bty = "n",
           # inset=-0.05,
           xpd = TRUE,
           title = "",
           title.cex = 0.9,
           title.adj = )%>%
      (\(x) {
        text(x=x$text$x[c(1,6,11,16,21,26)], 
             y=x$text$y[1]+(x$text$y[1]-x$text$y[2])/5*3, 
             adj = c(0,1),
             labels = c(as.expression(bquote("G"[1])), 
                        as.expression(bquote("G"[2])), 
                        "OO", "OPS", "Rad", "UO"),
             xpd = TRUE, 
             cex = 0.8, 
             srt = 90);
        text(x=x$text$x[26]+(x$text$x[26]-x$text$x[21]), 
             y=x$text$y[26:30], 
             adj = c(0,0.5),
             labels = c("All US", "Midwest", "Northeast", "South", "West"),
             xpd = TRUE, 
             cex = 0.8);
        # return(x)
      })
  }
  
  
  title(main = paste0(sub("^([0-9]{4}[-][0-9]{4})[.].+$", 
                          "\\1", 
                          names(fits)[i]), 
                      "\n", 
                      sub("[0-9]{4}[-][0-9]{4}[.]", 
                          "", 
                          names(fits)[i])%>%
                        (\(x) {
                          paste0(toupper(substr(x,1,1)), substr(x,2, nchar(x))) %>% 
                            return()
                        }), 
                      " Objective Scale"
  ), 
  font.main = 1, 
  adj = 0, 
  line = 2.5, 
  cex.main = 0.8)
  
  ### add letter to top left
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = LETTERS[c(1,3,5,2,4,6)][i], adj = c(0,1), xpd = T, cex = 1, font = 2)
  
  
}


dev.off()




## clean environment
rm(list = ls())
gc()



