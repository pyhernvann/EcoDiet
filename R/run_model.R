#' Extract the all Rhat (Gelman-Rubin diagnostic) values stored in the summary
#' table of the jagsUI object returned by the \code{\link{run_model}} function
#' and print a convergence problem message if at least one is > 1.1
#' 
#' @param jags_output the MCMC output by the jagsUI::jags function called by the
#' 
#' @keywords internal
#' @noRd

print_convergence_diagnostic <- function(jags_output){
  
  rhat <- jags_output$summary[, "Rhat"]

  variable_number <- length(rhat)
  over_1.1 <- rhat[which(rhat > 1.1)]
  variable_number_over_1.1 <- length(over_1.1)
  worst_var <- sort(over_1.1, decreasing=T)[seq(1, min(length(over_1.1), 10))]
  
  if (variable_number_over_1.1 > 0){
    message("\n  /!\\ Convergence warning:")
    message("Out of the ", variable_number,  " variables, ",
            variable_number_over_1.1, " variables have a Gelman-Rubin statistic > 1.1.")
    message("You may consider modifying the model run settings.\n")
    message("The variables with the poorest convergence are: ", paste(names(worst_var), collapse=", "),  ".\n")
  }else{
    message("\n  Convergence is met for all variables (Rhat < 1.1)")
  }
  
}


#' Run the EcoDiet model
#' 
#' @description
#' 
#' This function runs the EcoDiet model using a Markov chain Monte Carlo approximation 
#' through the 'jagsUI' package to provide an approximated distribution for the variables of interest.
#' 
#' Depending on the \code{nb_iter} entered, this function may take hours, or even days 
#' to run. We advise you to first test whether your model is compiling properly with the by-default parameters,
#' as this should take 1-2 min to run depending on your data size.
#' 
#' To save time, this function can solicit several cores (if available) to parallelize chains. Note that progress bars
#' won't be displayed if chains are parallelized. 
#' 
#' A warning message is printed if the model has not converged in the end (if the Gelman-Rubin diagnostic
#' of at least one variable is > 1.1). For each run, the default 'jagsUI' package messages summarize
#' the '.txt' file used for the definition of the BUGS model, the configuration of the model (iteration, adaptation, 
#' burnin, thin rate), the time required to run the model, and main statistics for the variables.
#' 
#' You need to have run the \code{preprocess_data} and the \code{write_model} functions 
#' before using this function, as their outputs are used as the inputs for \code{run_model}.
#' 
#'
#' @param model_file The file containing the BUGS definition of the EcoDiet model 
#'   output by the \code{write_model} function
#' @param data The preprocessed data list output by the preprocess_data() function
#' @param inits A list containing the initial values of the variables.
#'   By default the initialisation values are NULL, which means that the chain initial values
#'   are drawn from the prior distributions.
#' @param run_param A object that can be a list of the parameters to configure the JAGS model or a string acting as
#'   a shortcut characterizing the overall length of the run requested (e.g. "short" or "long"). If run_param is 
#'   provided as a list, the user should provide at least nb_iter, i.e. the number of iterations to run (the more
#'   iterations, the better are the chances that the model will converge; very small by default to test if the model
#'   compiles properly), and nb_burnin, i.e. the number of burn-in steps to run (so that the variable approximations
#'   are not too influenced by the first initial random values). nb_thin, the thinning rate, is by default defined by
#'   the function. The number of adaptation steps nb_adapt can be specified but is not required (see jasgUI documentation 
#'   for more details). If set manually, it should be at least set at 1000.
#' @param variables_to_save A vector of variable names defining the variables to output. 
#'   The number has a big number of variables but by default we only save the variables of interest
#'   that are the trophic link probabilities \code{eta} and the diet proportions \code{PI}.
#'   Only these saved variables are used to compute the Gelman-Rubin statistics that indicate whether
#'   the model has converged or not.
#' @param parallelize Indicates whether chains should be parallelized using several cores. Recommended in case of complex models. 
#' @param DIC.out Indicates whether the DIC (Deviance Information Criterion) should be reported.
#'
#' @return A MCMC output formatted as a jagsUI object.
#' 
#' @examples
#' 
#' \donttest{
#' realistic_biotracer_data <- read.csv(system.file("extdata", "realistic_biotracer_data.csv",
#'                                                package = "EcoDiet"))
#' realistic_stomach_data <- read.csv(system.file("extdata", "realistic_stomach_data.csv",
#'                                              package = "EcoDiet"))
#'
#' data <- preprocess_data(biotracer_data = realistic_biotracer_data,
#'                         trophic_discrimination_factor = c(0.8, 3.4),
#'                         literature_configuration = FALSE,
#'                         stomach_data = realistic_stomach_data)
#'                         
#' write_model(literature_configuration = FALSE)
#' 
#' mcmc_output <- run_model("EcoDiet_model.txt", data, run_param="short", parallelize = TRUE)
#' }
#' 
#' @seealso \code{\link{preprocess_data}} to preprocess the data, and 
#' \code{\link{write_model}} to define the model. 
#'
#' @export

run_model <- function(model_file, data, inits = NULL, run_param,
                      variables_to_save = c("eta", "PI"), parallelize = FALSE, DIC.out = TRUE){
  
  if(is.list(run_param)){
    
    if(((!"nb_iter" %in% names(run_param)) | (!"nb_burnin" %in% names(run_param)))){
      stop("Uncorrect run parameters provided. Please ensure that you are at least providing nb_iter or nb_burnin. Or provide a string (e.g. 'short')")
    }
    
    if (run_param$nb_burnin >= run_param$nb_iter){
      stop("The number of burnin (\"nb_burnin\") needs to be inferior ",
           "to the number of iterations (\"nb_iter\").\n", "Please decrease it.")
    }
    
    mcmc_param <- run_param
    
    if(!("nb_iter" %in% names(run_param))){
      mcmc_param$nb_thin <- max(1, floor((run_param$nb_iter - run_param$nb_burnin)/1000))
    }
    
    if(!("nb_adapt" %in% names(run_param))){
      mcmc_param$nb_adapt <- NULL
    }
    
    if(!("nb_chains" %in% names(run_param))){
      mcmc_param$nb_chains <- 3
    }
    
    
    } else { # if the user has entered custom mcmc parameters, use them
    if(run_param=="test") mcmc_param <- list(nb_chains=3, nb_iter=1000, nb_burnin=500, nb_thin=1, nb_adapt=nb_iter/2)
    if(run_param=="very short") mcmc_param <- list(nb_chains=3, nb_iter=10000, nb_burnin=5000, nb_thin=5, nb_adapt=nb_iter/2)
    if(run_param=="short") mcmc_param <- list(nb_chains=3, nb_iter=50000, nb_burnin=25000, nb_thin=25, nb_adapt=nb_iter/2)
    if(run_param=="normal") mcmc_param <- list(nb_chains=3, nb_iter=100000, nb_burnin=50000, nb_thin=50, nb_adapt=nb_iter/2)
    if(run_param=="long") mcmc_param <- list(nb_chains=3, nb_iter=300000, nb_burnin=200000, nb_thin=100, nb_adapt=nb_iter/3)
    if(run_param=="very long") mcmc_param <- list(nb_chains=3, nb_iter=1000000, nb_burnin=500000, nb_thin=500, nb_adapt=nb_iter/3)
    }
  

  jags_model <- jagsUI::jags(data, inits = NULL, variables_to_save, 
                             model = model_file, DIC = DIC.out,
                             n.chains = mcmc_param$nb_chains, n.adapt = mcmc_param$nb_adapt, n.iter = mcmc_param$nb_iter, n.burnin = mcmc_param$nb_burnin, n.thin = mcmc_param$nb_thin,
                             parallel = parallelize)
  

  print_convergence_diagnostic(jags_model)
  
  print(jags_model)
  return(jags_model)
  
}
