# calculate root mean square error



RMSE <- function(obs = NULL, 
                 preds = NULL){
  
  sqrt(mean((obs-preds)^2, na.rm = TRUE))
  
}












