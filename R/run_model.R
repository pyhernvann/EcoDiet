#' Print a message if there is a convergence problem with the MCMC chains
#' This is an internal function called only in the run_model function below.
#' 
#' @param mcmc_output the mcmc.list containing the variables to store, i.e., the pi and lambda variables
#' 
#' @keywords internal
#' @noRd

print_convergence_diagnostic <- function(mcmc_output){
  
  gelman <- coda::gelman.diag(mcmc_output, multivariate = FALSE)$psrf
  save(gelman, file = "convergence_diagnostic.Rdata")
  
  # We keep only the point estimates of the psrf
  gelman <- gelman[, 1] 
  variable_number <- length(gelman)
  
  # The NAs are for the variables that were always equal to 0 or 1,
  # and we consider that these variables have converged by default.
  gelman <- gelman[which(!is.nan(gelman))] 
  variable_number_over_1.1 <- length(gelman[which(gelman > 1.1)])
  
  if (variable_number_over_1.1 > 0){
    message("Warning message:")
    message("  Convergence problem")
    message("Out of the ", variable_number,  " variables, ",
            variable_number_over_1.1, " variables have a Gelman-Rubin statistic > 1.1.")
    message("You should increase the number of iterations of your model with the `nb_iter` argument.\n")
  }
  
}


#' Run the MCMC chains for the EcoDiet model with the rjags package
#'
#' @param model_file the file containing the definition of the EcoDiet model
#' @param data the list containing the different loaded and preprocessed data objects
#' @param inits the list containing the initial values of the variables.
#' By default the initialisation values are null, i.e., the first parameters at the beginning
#' of the chains are drawns randomly from their prior distribution.
#' @param nb_iter the number of iterations to be run
#' 
#' @return an mcmc.list containing the variables to store, i.e., the pi and lambda variables
#' 
#' @examples
#' mcmc_output <- run_model(preprocessed_data, inits, model_string)
#'
#' @export

run_model <- function(model_file, data, inits = NULL, 
                      nb_iter = 1e+03, nb_adapt = 1e+03, nb_burnin = floor(nb_iter/2)){
  
  if (nb_burnin >= nb_iter){
    stop("The number of burnin (\"nb_burnin\") needs to be inferior ",
         "to the number of iterations (\"nb_iter\").\n", "Please decrease it.")
  }
  
  start_run <- Sys.time()
  
  nb_thin <- max(1, floor((nb_iter - nb_burnin)/1000))
  
  jags_model <- rjags::jags.model(
    file = model_file,
    data = data, 
    inits = inits,
    n.chains = 3,
    n.adapt = nb_adapt)
  
  cat("\nBurning in the MCMC chains...\n\n")
  update(jags_model, n.iter = nb_burnin)

  cat("\nSampling finally the MCMC chains...\n\n")
  mcmc_output <- rjags::coda.samples(
    model = jags_model,
    variable.name = c("eta", "PI"),
    n.iter = (nb_iter - nb_burnin),
    thin = nb_thin)
  
  duration_run <- Sys.time() - start_run
  message("The model took ", round(unclass(duration_run), 1), " ", attr(duration_run, "units"), " to run.\n")
  
  print_convergence_diagnostic(mcmc_output)
  
  mcmc_output <- as.matrix(mcmc_output)
  mcmc_output[] <- signif(mcmc_output, digits = 2)
  save(mcmc_output, file ="mcmc_output.Rdata")
  
  return(mcmc_output)
}