# Function to estimate human mobility using a variation of a gravity model

# model

## base model
### b0 + b1 log(Pi) + b2 log(Pj) + b3 log(dij)

## this base model is estimated for 18 groups specified with 2 categorical variables
### short versus long distances as determined by a distance threshold
### population size categories of origin-destination pairs
#### population size for all locations split according to tertiles: s[mall], m[edium], l[arge]
#### nine combinations indicating pairing of origin population size category and destination population size category
##### ss, sm, sm, ms, mm, ml, ls, lm, ll


# function arguments
## params is a vector of parameter values
### single parameter implementation
### params[1] = dt

## observed.data is a dataframe of observed movements between origins and destinations
### contains independent variables 
#### (hardcoded for this project)
#### log transformed


# function

calculateGravity <- function(params, observed.data){
  
  require(dplyr)
  
  ## adjust indicator based on provided distance threshold
  observed.data <- observed.data %>%
    
    mutate(indicator.long.distance = ifelse(log.distance.km>params[1], 1, 0))
          
  
  ## fit linear model
  fit <- lm(log.Workers.in.Commuting.Flow ~ 
              pop.cats*indicator.long.distance*log.POPESTIMATE.origin
            + pop.cats*indicator.long.distance*log.POPESTIMATE.destination
            + pop.cats*indicator.long.distance*log.distance.km,
            data = observed.data)
  
  ## generate and return predictions
  observed.data %>%
    
    mutate(gravity.flow.log = predict(fit, data.frame(log.POPESTIMATE.origin=log.POPESTIMATE.origin,
                                                      log.POPESTIMATE.destination=log.POPESTIMATE.destination,
                                                      log.distance.km=log.distance.km,
                                                      indicator.long.distance=indicator.long.distance, 
                                                      pop.cats=pop.cats))) %>%
    
    mutate(gravity.flow = exp(gravity.flow.log)) %>%
    
    select(pop.cats, 
           indicator.long.distance, 
           gravity.flow.log,
           gravity.flow) %>%
    
    return()
  
}

