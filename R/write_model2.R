#' Write a string containing the EcoDiet model in the BUGS syntax
#'
#' @param literature_prior a boolean (TRUE or FALSE) indicating whether the model will have 
#' prior distributions defined by a study of the literature
#' @return a string containing the model definition written in BUGS syntax
#' 
#' @examples
#' model_string <- write_model2()
#'
#' @export

write_model2 <- function(literature_prior = FALSE){
  
  model_string1 <- 
  "model{

for (i in 1:nb_group){

  for (j in 1:nb_elem){
  
    mu[j, i]  ~ dnorm(0, 1.0E-6)
  
  }

  for (l in 1:nb_y[i]){

    y[i, l, ] ~ dmnorm(mu[, i], SIGMA_inv[, , i])
  
  }
  
  SIGMA_inv[1:nb_elem, 1:nb_elem, i] ~ dwish(ID, nb_elem)

}

for (j in 1:nb_elem){

  for (i in list_pred){
  
    for (k in list_prey[i, 1:nb_prey[i]]){
    
      mix_numerator[j, k, i] <- PI[k, i] * q[j, k] * (mu[j, k] + delta[j, i])
      mix_denominator[j, k, i] <- PI[k, i] * q[j, k]
    
    }
    
    ZEROS[j, i] ~ dpois(ss[j, i])
    ss[j, i] <- pow(mu[j, i] - 
    sum(mix_numerator[j, list_prey[i, 1:nb_prey[i]], i]) / sum(mix_denominator[j, list_prey[i, 1:nb_prey[i]], i]), 
    2) + 0.0001
    
    delta[j, i] <- DELTA[j] + xi[j] * theta[j, i]
    theta[j, i] ~ dnorm(0, tau_theta[j])
  
  }

  xi[j] ~ dnorm(0, 0.0016)
  tau_theta[j] ~ dgamma(0.5, 0.5)

}

for (i in list_pred){
    
  for (k in list_prey[i, 1:nb_prey[i]]){"

  if (literature_prior){
    model_string2 <- 
"      eta[k, i] ~ dbeta(o[k, i] + eta_hyperparam_1[k, i], nb_o[i] - o[k, i] + eta_hyperparam_2[k, i])
      alpha[k, i] <- (nb_PI_prior * ped[i] * alpha_lit[k, i] + 
        nb_eta_to_PI * eta[k, i] / (sum(eta[list_prey[i, 1:nb_prey[i]], i])) + 1)"
  } else {
    model_string2 <- 
"      eta[k, i] ~ dbeta(o[k, i] + 1, nb_o[i] - o[k, i] + 1)
      alpha[k, i] <- nb_eta_to_PI * eta[k, i] / (sum(eta[list_prey[i, 1:nb_prey[i]], i])) + 1"
  }
  
  model_string3 <-
    "rho[k, i] ~ dgamma(alpha[k, i], 1)
    PI[k, i] <- rho[k, i] / sum(rho[list_prey[i, 1:nb_prey[i]], i])
    
    }
  }
}"   
  
  model_string <- paste(model_string1, model_string2, model_string3, sep = "\n\n")

  return(model_string)
}