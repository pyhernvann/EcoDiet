#' Write the EcoDiet model in bugs langage on a file
#'
#' @param file_name the name of the bugs files on which the model will be written
#' @return a string containing the model definition written in bugs syntaw
#' 
#' @examples
#' model_def <- write_model()
#'
#' @export

write_model <- function(){

  model_def <- 
"for (i in 1:nc){
  
  for (k in 1:ns[ic[i]]){
  
  o[is[i, k], ic[i]] ~ dbin(eta[is[i, k], ic[i]], n_sca[1, ic[i]])        
  LAMBDA[is[i, k], ic[i]] <- dbern((eta[is[i, k], ic[i]]))      
  
  }
  
  s[i] <- sum(LAMBDA[is[i, 1:ns[ic[i]]], ic[i]])  
  
  is_link_identified[i] <- ifelse(s[i] == 0, 0, 1)
  
}


for (i in 1:nc){

dzeta[ic[i]] <- n_lit * g[ic[i]]      

for (k in 1:ns[ic[i]]){

eta_hyperparam_1[is[i,k], ic[i]] <- ifelse(alpha_lit[is[i,k],ic[i]] == 0, 1, (dzeta[ic[i]]))
eta_hyperparam_2[is[i,k], ic[i]] <- dzeta[ic[i]] + 1 - eta_hyperparam_1[is[i,k],ic[i]]	

eta[is[i,k], ic[i]] ~ dbeta(eta_hyperparam_1[is[i, k], ic[i]] * switch_conf_3 + (1 - switch_conf_3), 
eta_hyperparam_2[is[i, k], ic[i]] * switch_conf_3 + (1 - switch_conf_3))

}
}


for(i in 1:(nb + nc)){

for(j in 1:ne){

mu[j, i]  ~ dnorm(0, 1.0E-6)

}

for(l in 1:n_sia[1, i]){

y[, i, l] ~ dmnorm(mu[, i], SIGMA_inv[, , i])

}

SIGMA_inv[1:ne, 1:ne, i] ~ dwish(ID[1:ne, 1:ne], ne)
SIGMA[1:ne, 1:ne, i] <- inverse(SIGMA_inv[, ,i])

}

for(j in 1:ne){

for(i in 1:nc){

for(k in 1:ns[ic[i]]){

mix_numerator[j, is[i, k], ic[i]] <- PI[is[i, k], ic[i]] * q[j, is[i, k]] * (mu[j, is[i, k]] + delta[j, ic[i]])
mix_denominator[j, is[i, k], ic[i]] <- PI[is[i, k], ic[i]] * q[j, is[i, k]]

}

ZEROS[j, i] ~ dpois(ss[j, i])
ss[j, i] <- pow(mu[j, ic[i]] - 
sum(mix_numerator[j, is[i, 1:ns[ic[i]]], ic[i]]) / sum(mix_denominator[j, is[i, 1:ns[ic[i]]], ic[i]]), 
2) + 0.0001

delta[j, ic[i]] <- DELTA[j, 1] + group_effect[j, ic[i]]
group_effect[j,ic[i]] <-  xi[j,1] * theta[j,ic[i]]
theta[j,ic[i]] ~ dnorm(0, tau_theta[j, 1])

}

xi[j,1] ~ dnorm(0, 0.0016)
tau_theta[j,1] ~ dgamma(0.5, 0.5)
sigma[j] <- abs(xi[j, 1]) / sqrt(tau_theta[j, 1])

}

for(i in 1:nc){

are_several_links[i] <- step(s[i] - 2)
iota[ic[i]] <- (((s[i] - 1) / (CVs[ic[i]]^2)) - 1) * are_several_links[i] + (1 - are_several_links[i])

psi[ic[i]] <- is_link_identified[i] * sum(psi_sub[is[i, 1:ns[ic[i]]], ic[i]]) + (1 - is_link_identified[i])

for(k in 1:ns[ic[i]]){

psi_sub[is[i, k], ic[i]] <- (alpha_lit[is[i, k], ic[i]] + add[is[i, k], ic[i]]) * LAMBDA[is[i, k], ic[i]]

add[is[i, k], ic[i]] <- ifelse(alpha_lit[is[i, k], ic[i]] == 0, 
1 / (s[i] * (is_link_identified[i]) + ns[ic[i]] * (1 - is_link_identified[i])), 
0)

alpha[is[i, k], ic[i]] <- ((1 - is_link_identified[i]) + is_link_identified[i] * LAMBDA[is[i,k],ic[i]]) * 
((1-switch_conf_3) + switch_conf_3 * (alpha_lit[is[i, k], ic[i]] + add[is[i,k],ic[i]]) * iota[ic[i]] / psi[ic[i]]) +
0.1

rho[is[i, k], ic[i]] ~ dgamma(alpha[is[i, k], ic[i]], 1)
PI[is[i,k],ic[i]] <- (rho[is[i,k],ic[i]])/sum(rho[is[i,1:ns[ic[i]]],ic[i]])
}
}"

  return(model_def)
}