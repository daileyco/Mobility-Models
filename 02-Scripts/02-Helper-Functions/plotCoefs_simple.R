# function to plot point and interval estimates


plotCoefs <- function(coef.df, xlims){
  
  require(dplyr)
  # require(viridis)
  require(RColorBrewer)
  
  colpal <- apply(rgb2hsv(col2rgb(brewer.pal(8, "Dark2")[c(1,3,4,7)])), MARGIN = 2, function(x){do.call(what = "hsv", args = list(h=x[["h"]], s=x[["s"]]-0*x[["s"]], v=x[["v"]]))})
  # c("#1B9E77", "#7570B3", "#E7298A", "#A6761D")
  desatcolpal <- apply(rgb2hsv(col2rgb(brewer.pal(8, "Dark2")[c(1,3,4,7)])), MARGIN = 2, function(x){do.call(what = "hsv", args = list(h=x[["h"]], s=x[["s"]]-0.4*x[["s"]], v=x[["v"]]))})
  # c("#1EAE83", "#817BC5", "#FE2D98", "#B78220")
  # c("#167E5F", "#5E5A8F", "#B9216E", "#855E17")
  # 
  # 
  # c("#849E96", "#A7A6B3", "#E7C1D4", "#A69C8B")
  # c("#6A9E8E", "#9A98B3", "#E79BC2", "#A6936F")
  # c("#4F9E87", "#8E8BB3", "#E775AF", "#A68954")
  # c("#359E7F", "#817DB3", "#E74F9D", "#A68038")
  coef.df <- coef.df %>%
    arrange(long.distance, large.populations, region) %>%
    mutate(ys = 21.5-(row_number()+ifelse(long.distance=="Long", 0.5, 0) + ifelse(large.populations=="Large-to-Large", 0.25,0)), 
           ltys = ifelse(long.distance=="Long", 1, 3), 
           # cols = c("black", viridis(4))[region], 
           # cols = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)])[region], 
           cols = c("black", colpal)[region], 
           dscs = c("#4D4D4D", desatcolpal)[region],
           # pchs = case_when(large.populations=="Other" & long.distance=="Short" ~ 1, 
           #                  large.populations=="Other" & long.distance=="Long" ~ 21, 
           #                  large.populations=="Large-to-Large" & long.distance=="Short" ~ 5, 
           #                  large.populations=="Large-to-Large" & long.distance=="Long" ~ 23, 
           #                  TRUE ~ NA), 
           pchs = case_when(large.populations=="Other" & long.distance=="Short" ~ 21, 
                            large.populations=="Other" & long.distance=="Long" ~ 21, 
                            large.populations=="Large-to-Large" & long.distance=="Short" ~ 23, 
                            large.populations=="Large-to-Large" & long.distance=="Long" ~ 23, 
                            TRUE ~ NA),
           bgcs = case_when(large.populations=="Other" & long.distance=="Short" ~ "white", 
                            large.populations=="Other" & long.distance=="Long" ~ dscs, 
                            large.populations=="Large-to-Large" & long.distance=="Short" ~ "white", 
                            large.populations=="Large-to-Large" & long.distance=="Long" ~ dscs, 
                            TRUE ~ NA), 
           pi = paste0(round(Beta,2), " (", round(LL,2), ",", round(UL,2), ")"), 
           pi = ifelse(is.na(Beta), "--", pi))
    
  
  # points <- coef.df$Beta
  # lls <- coef.df$LL
  # uls <- coef.df$UL
  
  ylims <- c(0.5, 20.75)
  
  par(mar = c(3.1, 2.1, 2.1, 9.1))
  plot(NA, type = "n", xlim = xlims, ylim = ylims, xlab = "", ylab = "", axes = F)
  
  abline(v = seq(-5,1, by=0.05)[-which(seq(-5,1, by=0.05)%in%seq(-5,1, by=0.1))], lty = "aa", col = "gray92")
  abline(v = seq(-5,1, by=0.1)[-which(seq(-5,1, by=0.1)%in%c(0))], lty = 5, col = "gray88")
  abline(v = 0, lty = 5, col = "black")
  axis(1, at = seq(-5,1, by=0.1), labels = FALSE, line = 0, tcl = -0.2)
  axis(1, at = seq(-5,1, by=0.1), tick = FALSE, line = -0.75, cex.axis = 8/10)
  
  
  # abline(v = seq(-5,1, by=0.25)[-which(seq(-5,1, by=0.25)%in%seq(-5,1, by=0.5))], lty = "aa", col = "gray92")
  # abline(v = seq(-5,1, by=0.5)[-which(seq(-5,1, by=0.5)%in%c(0))], lty = 5, col = "gray88")
  # abline(v = 0, lty = 5, col = "black")
  # 
  arrows(x0 = coef.df$LL, y0 = coef.df$ys, x1 = coef.df$UL, y1 = coef.df$ys, 
         angle = 90, length = 0.075, code = 3, 
         col = coef.df$cols, lty = coef.df$ltys, lwd = 1.75)
  
  points(x = coef.df$Beta, y = coef.df$ys, pch = coef.df$pchs, col = coef.df$cols, cex = 1.5, bg = coef.df$bgcs)
  
  # axis(1, at = seq(-5,1, by=0.25), labels = FALSE, line = 0, tcl = -0.2)
  # axis(1, at = seq(-5,1, by=0.25), tick = FALSE, line = -0.75, cex.axis = 8/10)
  # 
  
  mtext(text = coef.df$pi, side = 4, line = 0.25, at = coef.df$ys, las = 1, adj = 0, padj = 0.5, cex = 8/10)
  # mtext(text = "Mean (95%CI)", side = 4, line = 0.1, at = 12, las = 1, adj = 0, padj = 0.5, cex = 8/10, font = 3)
  

  box()
  
 
}

