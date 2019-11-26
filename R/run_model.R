#' Run the MCMC chains for the EcoDiet model
#'
#' @param data the list containing the different loaded and preprocessed data objects
#' @param inits the list containing the initial values of the variables
#' @param model_string the string containing the definition of the EcoDiet model
#' 
#' @return an mcmc.list containing the variables to store, i.e., the pi and lambda variables
#' 
#' @examples
#' mcmc_output <- run_model(preprocessed_data, model_string)
#'
#' @export


run_model <- function(data, inits, model_file, n.iter = 1e+03){
  
  n.chains <- 3
  n.burnin <- floor(n.iter/2)
  n.thin <- max(1, floor((n.iter - n.burnin)/1000))

  inits <- rep(list(inits), n.chains)

  mcmc_output <- R2jags::jags(
    data = data, 
    inits = inits,
    parameters.to.save = c("LAMBDA", "PI"), 
    model.file = model_file,
    n.chains = n.chains, n.iter = n.iter, 
    n.burnin = n.burnin, n.thin = n.thin)
  
  mcmc_output <- coda::as.mcmc(mcmc_output)
  
  return(mcmc_output)
}