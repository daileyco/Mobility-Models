# Fit regression model



fitModel <- function(model.function = "lm",
                     model.arguments = NULL){
  
  require(dplyr)
  require(tibble)
  require(tidyr)
  
  fit <- do.call(model.function, model.arguments)
  
  fit.stats <- cbind(coef(fit), confint(fit)) %>% 
    as.data.frame() %>%
    setNames(., nm = c("Beta", "LL", "UL")) %>% 
    
    rownames_to_column(., var = "Parameter") %>%
    mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
    select(Parameter, Estimate) %>%
    add_row(., 
            .after = nrow(.), 
            Parameter = "AIC", Estimate = as.character(AIC(fit))) %>%
    add_row(., 
            .after = nrow(.), 
            Parameter = "BIC", Estimate = as.character(BIC(fit))) %>%
    pivot_wider(names_from = "Parameter", values_from = "Estimate") %>%
    mutate(Model = model.function) %>%
    select(Model, everything())
  
}











