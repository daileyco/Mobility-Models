# Helper function for Metropolis random walk, MCMC
## script adapted from version authored by Kevin Dobbin 
## from Markov Models and Bayesian Computation course
### errors are mine alone


## arguments
### chain.length is integer for length of Markov chain
### observed.data is a dataframe containing at least observed values for dependent and independent variables used in objective.function
### objective.function is a function to generate predictions given parameter values and then compare between observed and predicted values
### posterior.function is a function to calculate posterior, wrapper for objective.function to generate likelihood (?)
#### both objective.function and posterior.function take arguments "params" and "observed.data"
### params.proposal.tbl is a ***tibble*** dataframe with the following columns
#### param.names is a character vector giving names for all parameters
#### proposal.function is a character vector giving names for all functions used to generate proposed values for each parameter
#### proposal.function.args is a *list* vector (?) where each entry is a named list containing arguments needed for proposal.function
#### proposal.function.args.mc.depend is a character vector giving an argument for proposal function which depends on previous iteration in Markov chain
##### this argument should not be in proposal.function.args list; if NA, proposal.function.args assumed complete
###### e.g., "mean" for rnorm()
#### initial.values is a numeric vector of initial values for parameters


## function
runMetropolisRandomWalk <- function(chain.length = 100, 
                                    observed.data = NULL, 
                                    objective.function = NULL, 
                                    posterior.function = NULL, 
                                    params.proposal.tbl = NULL){
  
  ## packages
  require(dplyr)
  require(tibble)
  
  ## set up df for accepted values in chains
  chains.df <- matrix(c(NA), 
                      nrow = chain.length, 
                      ncol = nrow(params.proposal.tbl)) %>% 
    as.data.frame() %>% 
    setNames(., 
             nm = unlist(params.proposal.tbl$param.names))
  
  ### input initial values
  chains.df[1,] <- unlist(params.proposal.tbl$initial.values)
  
  
  ## set up df for monitoring mcmc acceptance
  monitoring.chains.df <- matrix(c(NA), 
                                 nrow = chain.length, 
                                 ncol = 3) %>% 
    as.data.frame() %>% 
    setNames(., 
             nm = c("accept", "alpha", "objective"))
  
  ### input initial value for objective function
  monitoring.chains.df$objective[1] <- do.call(objective.function, 
                                               args = list(observed.data=observed.data, 
                                                           params=unlist(chains.df[1,])))
  
  
  
  ## MCMC
  
  ### track run time and progress
  start.time <- Sys.time()
  mcmc.progress <- txtProgressBar(min = 0, 
                                  max = chain.length, 
                                  initial = 1, 
                                  style = 3)
  
  
  for(mc in 2:chain.length){
    
    setTxtProgressBar(mcmc.progress, 
                      value = mc)
    
    ## extract previous mc parameter values
    params.current = unlist(chains.df[mc-1,])
    
    ## generate proposal values
    params.proposed = sapply(1:nrow(params.proposal.tbl), 
                             function(index){
                               do.call(what = params.proposal.tbl$proposal.function[index], 
                                       args = if(is.na(params.proposal.tbl$proposal.function.args.mc.depend[index])){
                                                params.proposal.tbl$proposal.function.args[[index]]
                                              }else{
                                                c(params.proposal.tbl$proposal.function.args[[index]],
                                                  chains.df[mc-1,index] %>%
                                                    setNames(., nm = params.proposal.tbl$proposal.function.args.mc.depend[index]))
                                              }
                                       )
                             })
    
    
    ## calculate posterior / fit for current and proposed values
    alpha.numerator = do.call(what = posterior.function, 
                              args = list(observed.data=observed.data, 
                                          params=params.proposed))
    
    alpha.denominator = do.call(what = posterior.function, 
                                args = list(observed.data=observed.data, 
                                            params=params.current))
    
    
    
    ## calculate alpha
    ### alpha > 1 corresponds to better fit for proposal, proposal always accepted
    ### alpha < 1 corresponds to worse fit for proposal, proposal *sometimes* accepted
    alpha = min(1, alpha.numerator/alpha.denominator)
    
    monitoring.chains.df$alpha[mc] = alpha
    
    
    ## generate uniform random number
    myunif = runif(1)
    
    
    ## determine acceptance and store values in chains
    if(myunif <= alpha){
      monitoring.chains.df$accept[mc] = 1
      monitoring.chains.df$objective[mc] = do.call(what = objective.function, 
                                                   args = list(observed.data=observed.data, 
                                                               params=params.proposed))
      
      chains.df[mc,] = params.proposed
    }
    if(myunif > alpha){
      monitoring.chains.df$accept[mc] = 0
      monitoring.chains.df$objective[mc] = monitoring.chains.df$objective[mc-1]
      
      chains.df[mc,] = chains.df[mc-1,]
    }
  }
  
  ### stop tracking run time and progress
  close(mcmc.progress)
  end.time <- Sys.time()
  
  #### print run time
  cat("Total run time: ", end.time-start.time, " ", units(end.time-start.time))
  #### print acceptance rate
  cat("Acceptance: ", round(mean(monitoring.chains.df$accept, na.rm=T)*100,2), "%")
  
  ## return
  
  bind_cols(chains.df, 
            monitoring.chains.df) %>%
    return()
  
}

