#' Run the MCMC chains for the EcoDiet model with the rjags package
#'
#' @param data the list containing the different loaded and preprocessed data objects
#' @param inits the list containing the initial values of the variables
#' @param model_file the file containing the definition of the EcoDiet model
#' @param n_iter the number of iterations to be run
#' @param n_chains the number of Markov chains to be run
#' 
#' @return an mcmc.list containing the variables to store, i.e., the pi and lambda variables
#' 
#' @examples
#' mcmc_output <- run_model(preprocessed_data, inits, model_string)
#'
#' @export

run_model <- function(data, inits, model_file, n_iter = 1e+03, n_chains = 3){
  
  start_time <- Sys.time()
  
  n_adapt <- 1e+03
  n_burnin <- floor(n_iter/2)
  n_thin <- max(1, floor((n_iter - n_burnin)/1000))
  
  jags_model <- rjags::jags.model(
    data = data, 
    inits = inits,
    file = model_file,
    n.chains = n_chains,
    n.adapt = n_adapt)
  
  cat("\nBurning in the MCMC chain...\n\n")
  update(jags_model, n.iter = n_burnin)

  cat("\nSampling finally the MCMC chains...\n\n")
  mcmc_output <- rjags::coda.samples(
    model = jags_model,
    variable.name = c("LAMBDA", "PI"),
    n.iter = (n_iter - n_burnin),
    thin = n_thin)
  
  end_time <- Sys.time()
  cat(end_time - start_time)
  
  return(mcmc_output)
}