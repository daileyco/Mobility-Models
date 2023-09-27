# function to dummy code continuous variables for interactions with categorical

createDummies <- function(data, dt){
  
  require(dplyr)
  
  catvars <- c("indicator.long.distance", "pop.cats")
  
  data <- data %>%
    
    mutate(indicator.long.distance = ifelse(log.distance.km>dt, 1, 0))
  
  
  combos <- sapply(catvars, 
                   function(thisvar){
                     unique(data[[thisvar]])
                   }) %>% 
    expand.grid() %>%
    mutate(across(where(is.factor), ~as.character(.x)))
  
  for(i in 1:nrow(combos)){
    
    data <- data %>%
      mutate(across(c(log.POPESTIMATE.origin, log.POPESTIMATE.destination, log.distance.km), 
                    ~ifelse(eval(parse(text=paste0(".data[['", names(combos), "']]=='", combos[i,], "'", collapse = " & "))), 
                            .x, 
                            0), 
                    .names = paste0("{.col}_", 
                                    paste0(names(combos), combos[i,], collapse = "_"))))
    
  }
  
  return(data)
  
}