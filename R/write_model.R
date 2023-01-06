#' Write the EcoDiet model in BUGS
#' 
#' @description 
#' 
#' This function writes the EcoDiet model in the BUGS syntax as a several line long string.
#' 
#' The model definition depends on whether or not literature data will be used to inform the priors,
#' hence the parameter \code{literature_configuration}.
#' 
#' To know more about what is inside the model, please read the reference article.
#'
#'
#' @inheritParams preprocess_data
#' @param file.name The name and location under which the '.txt' BUGS definition
#' of the model will be saved. If not provided, the file will be saved in the
#' current repository under the "EcoDiet_model.txt" name.
#' @param print.model Indicates whether the user wants to print the written model in the console.
#' @return A string containing the model definition in BUGS
#'
#' @examples
#' \donttest{
#' write_model(file.name="my_model_with_priors.txt", literature_configuration = TRUE)
#' 
#' write_model(literature_configuration = FALSE, print.model = TRUE)
#' 
#' unlink('my_model_with_priors.txt')
#' unlink('EcoDiet_model.txt')
#' }
#' 
#' @seealso \code{\link{run_model}} to run the model after it has been defined
#'
#' @export

write_model <- function(file.name="EcoDiet_model.txt",  literature_configuration = FALSE, print.model = FALSE){

  model_string1 <-
"model{
  for (i in list_pred){

    for (k in list_prey[i, 1:nb_prey[i]]){

      LAMBDA[k, i] ~ dbern(eta[k, i])"
  if (literature_configuration){
    model_string2 <-
"      eta[k, i] ~ dbeta(o[k, i] + eta_hyperparam_1[k, i], nb_o[i] - o[k, i] + eta_hyperparam_2[k, i])"
  } else {
    model_string2 <-
"      eta[k, i] ~ dbeta(o[k, i] + 1, nb_o[i] - o[k, i] + 1)"
  }

  model_string3 <-
"   }

    s[i] <- sum(LAMBDA[list_prey[i, 1:nb_prey[i]], i])
    is_link_identified[i] <- ifelse(s[i] == 0, 0, 1)

  }

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

  for (i in list_pred){"

  if (literature_configuration){
    model_string4 <-
"      are_several_links[i] <- step(s[i] - 2)
      iota[i] <- (((s[i] - 1) / (CVs[i]^2)) - 1) * are_several_links[i] + (1 - are_several_links[i])

      psi[i] <- is_link_identified[i] * sum(psi_sub[list_prey[i, 1:nb_prey[i]], i]) + (1 - is_link_identified[i])

      for (k in list_prey[i, 1:nb_prey[i]]){

        psi_sub[k, i] <- (alpha_lit[k, i] + add[k, i]) * LAMBDA[k, i]

        add[k, i] <- ifelse(alpha_lit[k, i] == 0,
        1 / (s[i] * (is_link_identified[i]) + nb_prey[i] * (1 - is_link_identified[i])),
        0)

        alpha[k, i] <- ((1 - is_link_identified[i]) + is_link_identified[i] * LAMBDA[k, i]) *
                       ((alpha_lit[k, i] + add[k, i]) * iota[i] / psi[i]) + 0.1"
  } else {
    model_string4 <-
"    for (k in list_prey[i, 1:nb_prey[i]]){

      alpha[k, i] <- (1 - is_link_identified[i]) + is_link_identified[i] * LAMBDA[k, i] + 0.1"
  }

  model_string5 <-
"      rho[k, i] ~ dgamma(alpha[k, i], 1)
      PI[k, i] <- rho[k, i] / sum(rho[list_prey[i, 1:nb_prey[i]], i])
    }
  }
}"

  model_string <- paste(model_string1, model_string2, model_string3, model_string4, model_string5,
                        sep = "\n\n")

  write(model_string, file = file.name)
  
  if(print.model==T){
    cat(model_string)
  }
  
}
