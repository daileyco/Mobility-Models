# function to dummy code continuous variables for interactions with categorical

createDummies <- function(data){
  
  require(dplyr)
  
  catvars <- c("census.region.origin")
  
  combos <- sapply(catvars, 
                   function(thisvar){
                     unique(data[[thisvar]])
                   }) %>% 
    expand.grid() %>%
    mutate(across(where(is.factor), ~as.character(.x))) %>%
    setNames(., nm = catvars)
  
  for(i in 1:nrow(combos)){
    
    data <- data %>%
      mutate(log.pop.origin.times.total.workers.residence.domestic = log.POPESTIMATE.origin*log.Total.Workers.Residence.Domestic) %>%
      mutate(across(c(log.POPESTIMATE.origin, log.Total.Workers.Residence.Domestic, log.pop.origin.times.total.workers.residence.domestic), 
                    ~ifelse(eval(parse(text=paste0(".data[['", names(combos), "']]=='", combos[i,], "'", collapse = " & "))), 
                            .x, 
                            0), 
                    .names = paste0("{.col}_", 
                                    paste0(names(combos), combos[i,], collapse = "_"))))
    
  }
  
  return(data)
  
}
