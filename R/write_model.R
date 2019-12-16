#' Write a string containing the EcoDiet model in the BUGS syntax
#'
#' @param file_name the name of the bugs files on which the model will be written
#' @return a string containing the model definition written in bugs syntax
#' 
#' @examples
#' model_string <- write_model()
#'
#' @export

write_model <- function(literature_prior = FALSE){

  model_string1 <- 
"model{
  for (i in list_pred){
    
    for (k in list_prey[i, 1:nb_prey[i]]){
    
      o[k, i] ~ dbin(eta[k, i], nb_o[i])
      LAMBDA[k, i] ~ dbern((eta[k, i]))
    
    }
    
    s[i] <- sum(LAMBDA[list_prey[i, 1:nb_prey[i]], i])
    is_link_identified[i] <- ifelse(s[i] == 0, 0, 1)
    
  }"
  
  if (literature_prior){
    model_string2 <- 
"  for (i in list_pred){
    
    dzeta[i] <- nb_lit * ped[i]      
    
    for (k in list_prey[i, 1:nb_prey[i]]){
    
      eta_hyperparam_1[k, i] <- ifelse(alpha_lit[k,i] == 0, 1, (dzeta[i]))
      eta_hyperparam_2[k, i] <- dzeta[i] + 1 - eta_hyperparam_1[k, i]	
      
      eta[k, i] ~ dbeta(eta_hyperparam_1[k, i], eta_hyperparam_2[k, i])
    
      }
    }"
  } else {
    model_string2 <- 
"  for (i in list_pred){
    
    for (k in list_prey[i, 1:nb_prey[i]]){
    
      eta[k, i] ~ dbeta(1, 1)

    }
  }"
  }
  
  model_string3 <-  
"  for (i in 1:nb_group){
  
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

  }"
  
  if (literature_prior){
    model_string4 <-
"  for (i in list_pred){
  
      are_several_links[i] <- step(s[i] - 2)
      iota[i] <- (((s[i] - 1) / (CVs[i]^2)) - 1) * are_several_links[i] + (1 - are_several_links[i])
      
      psi[i] <- is_link_identified[i] * sum(psi_sub[list_prey[i, 1:nb_prey[i]], i]) + (1 - is_link_identified[i])
      
      for (k in list_prey[i, 1:nb_prey[i]]){
      
        psi_sub[k, i] <- (alpha_lit[k, i] + add[k, i]) * LAMBDA[k, i]
        
        add[k, i] <- ifelse(alpha_lit[k, i] == 0, 
        1 / (s[i] * (is_link_identified[i]) + nb_prey[i] * (1 - is_link_identified[i])), 
        0)
        
        alpha[k, i] <- (alpha_lit[k, i] + add[k, i]) * iota[i] / psi[i]) + 0.1
        
        rho[k, i] ~ dgamma(alpha[k, i], 1)
        PI[k, i] <- rho[k, i] / sum(rho[list_prey[i, 1:nb_prey[i]], i])
      }
    }
  }"
  } else {
    model_string4 <-
"  for (i in list_pred){
    
    for (k in list_prey[i, 1:nb_prey[i]]){
      
      alpha[k, i] <- ((1 - is_link_identified[i]) + is_link_identified[i] * LAMBDA[k,i]) + 0.1
      rho[k, i] ~ dgamma(alpha[k, i], 1)
      PI[k, i] <- rho[k, i] / sum(rho[list_prey[i, 1:nb_prey[i]], i])

    }
  }
}"   
  }
  
  model_string <- paste(model_string1, model_string2, model_string3, model_string4, sep = "\n\n")

  return(model_string)
}