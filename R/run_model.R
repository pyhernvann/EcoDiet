#' Run the MCMC chains for the EcoDiet model with the rjags package
#'
#' @param model_file the file containing the definition of the EcoDiet model
#' @param data the list containing the different loaded and preprocessed data objects
#' @param inits the list containing the initial values of the variables.
#' By default the initialisation values are null, i.e., the first parameters at the beginning
#' of the chains are drawns randomly from their prior distribution.
#' @param n_iter the number of iterations to be run
#' @param n_chains the number of Markov chains to be run
#' 
#' @return an mcmc.list containing the variables to store, i.e., the pi and lambda variables
#' 
#' @examples
#' mcmc_output <- run_model(preprocessed_data, inits, model_string)
#'
#' @export

run_model <- function(model_file, data, inits = NULL, n_iter = 1e+03, n_chains = 3){
  
  start_time <- Sys.time()
  
  n_adapt <- 1e+03
  n_burnin <- floor(n_iter/2)
  n_thin <- max(1, floor((n_iter - n_burnin)/1000))
  
  jags_model <- rjags::jags.model(
    file = model_file,
    data = data, 
    inits = inits,
    n.chains = n_chains,
    n.adapt = n_adapt)
  
  cat("\nBurning in the MCMC chains...\n\n")
  update(jags_model, n.iter = n_burnin)

  cat("\nSampling finally the MCMC chains...\n\n")
  mcmc_output <- rjags::coda.samples(
    model = jags_model,
    variable.name = c("LAMBDA", "PI"),
    n.iter = (n_iter - n_burnin),
    thin = n_thin)
  
  end_time <- Sys.time()
  cat("The time that the model took to run is:") 
  print(end_time - start_time)
  
  gelman <- coda::gelman.diag(mcmc_output, multivariate = FALSE)$psrf[, 1]
  variable_number <- length(gelman)
  gelman <- gelman[which(!is.nan(gelman))]
  variable_number_over_1.1 <- length(gelman[which(gelman > 1.1)])
  variable_number_over_1.05 <- length(gelman[which(gelman > 1.05)])
  
  if (variable_number_over_1.1 > 0){
    message("\n          /!\\   /!\\   BIG CONVERGENCE PROBLEM   /!\\   /!\\ \n")
    message("Out of the ", variable_number,  " variables, ",
            variable_number_over_1.1, " variables have a Gelman-Rubin diagnostic > 1.1.")
  } else if (variable_number_over_1.05 > 0){
    message("\n             /!\\   /!\\   CONVERGENCE PROBLEM   /!\\   /!\\ \n")
    message("Out of the ", variable_number,  " variables, ",
            variable_number_over_1.05, " variables have a Gelman-Rubin diagnostic > 1.05.")
  }
  message("\nYou should increase the number of iterations of your model with the `n_iter` argument:")
  message("> mcmc_output <- run_model(textConnection(model_string), data, n_iter = 1e+06)\n")
  
  return(mcmc_output)
}