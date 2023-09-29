# function to estimate commuter flux using a generalization of the radiation model

## Universal opportunity model as described by Liu and Yan (https://doi.org/10.1038/s41598-020-61613-y)
### generalization of radiation model of Simini, Gonzalez, Maritan, and Barabasi (https://doi.org/10.1038/nature10856)

### two parameters (descriptions from Liu and Yan)
#### alpha, exploratory tendency
##### destination whose benefit is higher than origin's and intervening opportunities
##### larger alpha = greater probability distant destinations selected
#### beta, cautious tendency
##### destination whose benefit is higher than origin's, and origin's is higher than intervening opportunities
##### larger beta = greater probability near destinations selected

### constraints
#### alpha and beta are both non-negative
#### alpha + beta <= 1


### special cases
#### alpha = 0 and beta = 0
##### opportunity only model
#### alpha = 1 and beta = 0
##### opportunity priority selection (OPS) model
#### alpha = 0 and beta = 1
##### radiation model


## arguments
### params = c(alpha, beta)
### observed.data = origin destination dataframe with necessary variables
#### hardcoded for this project
### Tvar = total number of commuters originating from focal origin location
#### used to estimate flux by multiplying with probability estimated from universal opportunity model




calculateUniversalOpportunity <- function(params, observed.data, Svar = "sij", Tvar = "Total.Workers.Residence.Domestic"){
  
  require(dplyr)
  
  ## set parameter values
  alpha <- params[1]
  beta <- params[2]
  
  ## broken constraints
  if(alpha<0 | beta<0){
    return(NA)
  }
  if(alpha+beta>1){
    return(NA)
  }
  
  ## calculate
  observed.data %>%
    
    ### calculate numerator, (is this sufficient alone?)
    mutate(uo.prob.num = ((POPESTIMATE.origin+alpha*.data[[Svar]])*POPESTIMATE.destination)/((POPESTIMATE.origin+(alpha+beta)*.data[[Svar]])*(POPESTIMATE.origin+POPESTIMATE.destination+(alpha+beta)*.data[[Svar]]))) %>%
    
    ### calculate denominator, sum of numerator value over all destinations
    group_by(period,`State FIPS Residence`,`County FIPS Residence`) %>%
    mutate(uo.prob.den = sum(uo.prob.num)) %>%
    ungroup() %>%
    
    ### calculate probability, numerator divided by denominator
    mutate(uo.prob = uo.prob.num/uo.prob.den) %>%
    
    ### multiply probability by Total Commuters originating in Origin Location to estimate flux
    mutate(uo = .data[[Tvar]]*uo.prob, 
           uo.log = .data[[paste0("log.", Tvar)]]+log(uo.prob)) %>%
    
    ### return calculated values
    select(matches("uo")) %>%
    return()
  
}

