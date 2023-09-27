# function to plot point and interval estimates


plotCoefs <- function(coef.df, xlims, left=FALSE, top=FALSE, leftmiddle=NULL, topmiddle=NULL){
  
  require(dplyr)
  # require(viridis)
  require(RColorBrewer)
  
  coef.df <- coef.df %>%
    arrange(long.distance, region) %>%
    mutate(ys = 11.5-(row_number()+ifelse(long.distance=="Long", 0.5, 0)), 
           ltys = ifelse(long.distance=="Long", 1, 3), 
           # cols = c("black", viridis(4))[region], 
           cols = c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)])[region], 
           pchs = ifelse(region=="All US", 1, 16), 
           pi = paste0(round(Beta,2), " (", round(LL,2), ",", round(UL,2), ")"), 
           pi = ifelse(is.na(Beta), "--", pi))
    
  
  # points <- coef.df$Beta
  # lls <- coef.df$LL
  # uls <- coef.df$UL
  
  ylims <- c(0.75, 10.75)
  
  
  if(unique(coef.df$pop.cat.destination)=="Large"){
    lmar = 2.1
    rmar = 10.1
  }else{
    if(unique(coef.df$pop.cat.destination)=="Medium"){
      lmar = 2.6
      rmar = 9.6
    }else{
      lmar = 3.6
      rmar = 8.6
    }
  }
  
  if(unique(coef.df$pop.cat.origin)=="Large"){
    bmar = 3.6
    tmar = 1.6
  }else{
    if(unique(coef.df$pop.cat.origin)=="Medium"){
      bmar = 2.6
      tmar = 2.6
    }else{
      bmar = 1.6
      tmar = 3.6
    }
  }
  
  par(mar = c(bmar, lmar, tmar, rmar))
  
  plot(NA, type = "n", xlim = xlims, ylim = ylims, xlab = "", ylab = "", axes = F)
  
  
  abline(v = seq(-5,1, by=0.25)[-which(seq(-5,1, by=0.25)%in%seq(-5,1, by=0.5))], lty = "aa", col = "gray92")
  abline(v = seq(-5,1, by=0.5)[-which(seq(-5,1, by=0.5)%in%c(0))], lty = 5, col = "gray88")
  abline(v = 0, lty = 5, col = "black")
  
  points(x = coef.df$Beta, y = coef.df$ys, pch = coef.df$pchs, col = coef.df$cols, cex = 2.5)
  arrows(x0 = coef.df$LL, y0 = coef.df$ys, x1 = coef.df$UL, y1 = coef.df$ys, 
         angle = 90, length = 0.075, code = 3, 
         col = coef.df$cols, lty = coef.df$ltys, lwd = 2.5)
  
  
  axis(1, at = seq(-5,1, by=0.25), labels = FALSE, line = 0, tcl = -0.2)
  axis(1, at = seq(-5,1, by=0.25), tick = FALSE, line = -0.75, cex.axis = 8/10)
  
  if(left){
    axis(2, at = 6, labels = toupper(unique(coef.df$pop.cat.origin)), tick = FALSE, line = -0.5, font = 2, cex.axis = 9/10)
    if(!is.na(leftmiddle)){
      axis(2, at = 6, labels = leftmiddle, tick = FALSE, line = 1, font = 2, cex.axis = 1)
    }
  }
  if(top){
    axis(3, at = mean(xlims), labels = toupper(unique(coef.df$pop.cat.destination)), tick = FALSE, line = -0.5, font = 2, cex.axis = 9/10)
    if(!is.na(topmiddle)){
      axis(3, at = mean(xlims), labels = topmiddle, tick = FALSE, line = 1, font = 2, cex.axis = 1)
    }
  }
  
  mtext(text = coef.df$pi, side = 4, line = 0.25, at = coef.df$ys, las = 1, adj = 0, padj = 0.5, cex = 8/10)
  # mtext(text = "Mean (95%CI)", side = 4, line = 0.1, at = 12, las = 1, adj = 0, padj = 0.5, cex = 8/10, font = 3)
  

  box()
  
  if(top & left){
    legend("topleft", 
           cex = 8/10, 
           # inset = -0.05,
           xpd = TRUE,
           # ncol = 2,
           lty = c(1, 3, NA, rep(NA,5)), 
           col = c("black", "black", NA, c("black", brewer.pal(8, "Dark2")[c(1,3,4,7)])), 
           pch = c(NA, NA, NA, 1, rep(16,4)), 
           legend = c("Long Distance", "Short Distance", "", "All US", "Midwest", "Northeast", "South", "West"))
    
  }
  
 
}

