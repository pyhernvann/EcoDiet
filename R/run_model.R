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

run_model <- function(data, inits, model_file, n_iter = 1e+03){
  
  start_time <- Sys.time()
  
  n_chains <- 3
  n_burnin <- floor(n_iter/2)
  n_thin <- max(1, floor((n_iter - n_burnin)/1000))

  inits <- rep(list(inits), n_chains)

  mcmc_output <- R2jags::jags(
    data = data, 
    inits = inits,
    parameters.to.save = c("LAMBDA", "PI"), 
    model.file = model_file,
    n.chains = n_chains, n.iter = n_iter, 
    n.burnin = n_burnin, n.thin = n_thin)
  
  mcmc_output <- coda::as.mcmc(mcmc_output)
  
  end_time <- Sys.time()
  cat(end_time - start_time)
  
  return(mcmc_output)
}