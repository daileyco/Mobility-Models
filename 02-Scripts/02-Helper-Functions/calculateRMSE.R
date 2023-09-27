# calculate root mean square error

calculateRMSE <- function(obs = NULL, 
                          preds = NULL){
  
  sqrt(mean((obs-preds)^2, na.rm = TRUE))
  
}
