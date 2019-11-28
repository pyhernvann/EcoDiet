#' Initialize the different variables of the EcoDiet model to start the MCMC chains
#'
#' @param data the list of the preprocessed data needed to compute the dimension of the variables
#' @param topo_run the topology matrix that is part of the raw data
#' @return a list containing the initial values for each variable that need to be initialized
#' 
#' @examples
#' inits <- initialize_model(load(ecodiet_example), ecodiet_example$topo_run)
#'
#' importFrom("stats", "median", "var")
#' @export

initialize_model <- function(data, topo_run){
  attach(data)
  nb_grp <- nb + nc
  
  topo_run <- topo_run[, order(colnames(topo_run))]
  topo_run <- topo_run[order(rownames(topo_run)), ]
  topo_run <- as.matrix(topo_run)
  
  id_with_sca <- which(n_sca[1, ic] != 0)
  id_without_sca <- seq(1, nc)[-id_with_sca]  
  nb_with_sca <- length(id_with_sca)
  
  init_LAMBDA <- matrix(NA, nb_grp, nb_grp)
  init_LAMBDA[, ic[id_without_sca]] <- topo_run[, ic[id_without_sca]]
  init_LAMBDA[init_LAMBDA == 0] <- NA
  
  for (i in 1:nb_with_sca){
    for (k in 1:ns[ic[id_with_sca[i]]]){
      if (o[is[id_with_sca[i], k], ic[id_with_sca[i]]] >= 0.5){
        init_LAMBDA[is[id_with_sca[i], k], ic[id_with_sca[i]]] <- 1
      }
      if (o[is[id_with_sca[i], k], ic[id_with_sca[i]]] < 0.5){
        init_LAMBDA[is[id_with_sca[i], k], ic[id_with_sca[i]]] <- 0
      }			
      
    }
  }
  
  init_mu   <- apply(y, c(1,2), mean, na.rm=TRUE)
  init_SIGMA_inv <- array(0, dim = c(ne, ne, nb_grp))
  for (sp in 1:nb_grp){
    for (j in 1:ne){
      init_SIGMA_inv[j,j,sp] <- 1 / var(t(y[1:ne, sp, ])[, j], na.rm = "TRUE")
    }
  }
  
  for(j in 1:ne){
    init_mu[j, is.nan(init_mu[j,])] <- median(init_mu[j, !is.nan(init_mu[j,])])
  }
  for (i in 1:ne){
    for (j in 1:ne){
      init_SIGMA_inv[i, j, is.na(init_SIGMA_inv[i, j, ])] <- median(init_SIGMA_inv[i, j, !is.na(init_SIGMA_inv[i, j, ])])
    }
  }
  
  init_rho <- matrix(NA, nrow = nb_grp, ncol = nb_grp) 
  for(i in 1:nc){init_rho[is[i, 1:ns[ic[i]]], ic[i]] <- 1.0}
  
  init_xi        <- matrix(0, nrow = ne, ncol = 1) 
  init_theta     <- matrix(0, nrow = ne, ncol = nb_grp) 
  init_tau_theta <- matrix(1, nrow = ne, ncol = 1) 
  
  ib <- which(colSums(topo_run) == 0)
  init_theta[, ib] <- NA
  
  init_list <- list(  
    mu      = init_mu,
    SIGMA_inv    = init_SIGMA_inv,
    rho   = init_rho,
    LAMBDA    = init_LAMBDA,
    xi      = init_xi,
    theta     = init_theta,
    tau_theta = init_tau_theta 
  )
}
