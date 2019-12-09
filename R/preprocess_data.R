#' Load and preprocess the raw data to feed the EcoDiet model
#'
#' @param raw_data_list the list containing the different raw data objects
#' 
#' @return a list of processed data, ready to be run by the EcoDiet model
#' 
#' @examples
#' data <- preprocess_data(ecodiet_example)
#'
#' @export

preprocess_data <- function(raw_data_list){
  
  # Constructs the index from the binary web matrix
  
  topo_run <- raw_data_list$topo_run
  topo_run <- topo_run[, order(colnames(topo_run))]
  topo_run <- topo_run[order(rownames(topo_run)), ]
  topo_run <- as.matrix(topo_run)
  
  nb_group <- ncol(topo_run)
  nb_prey  <- colSums(topo_run)
  
  list_pred <- as.vector(which(colSums(topo_run) != 0))
  nb_pred <- length(list_pred)
  
  list_prey <- matrix(data = NA, nrow = nb_group, ncol = nb_group)
  for (i in 1:nb_group){
    if (sum(topo_run[, i]) > 0) {
      list_prey[i, 1:nb_prey[i]] <- as.vector(which(topo_run[, i] != 0))
    }
  }
  
  
  # stomachal data
  
  nb_o <- raw_data_list$nb_stom_SCA_data
  nb_o <- as.matrix(nb_o[, order(colnames(nb_o))])
  
  
  SCA_data <- raw_data_list$SCA_data
  SCA_data <- SCA_data[, order(colnames(SCA_data))]
  SCA_data <- SCA_data[order(rownames(SCA_data)), ]
  SCA_input <- as.matrix(SCA_data)
  
  # parameters for the isotopic analysis
  
  el_conc <- as.matrix(raw_data_list$element_conc_data)
  
  tdf <- as.matrix(raw_data_list$mean_tdf)
  
  # SIA data preprocessing
  
  SIA_data <- raw_data_list$isotope_data
  SIA_data <- SIA_data[order(SIA_data$Group), ] 
  
  nb_elem <- ncol(SIA_data) - 1
  nb_max_spl <- max(table(SIA_data$Group))
  
  SIA_input <- array(NA, dim=c(nb_elem, nb_group, nb_max_spl))
  names_grp <- rownames(topo_run)
  colnames(SIA_input) <- names_grp
  
  for (el in 1:nb_elem){
    for (grp in 1:nb_group){
      SIA_input[el, grp, ] <- SIA_data[SIA_data$Group==names_grp[grp], el + 1]
    }
  }
  
  nb_y <- apply(ifelse(!is.na(SIA_input), 1, 0), seq(1, nb_elem), sum)
  
  # parameters for the priors
  
  literature_data <- raw_data_list$literature_data
  
  Pedigree_literature_data <- raw_data_list$pedigree_literature_data
  Ped <- as.matrix(Pedigree_literature_data)
  Ped <- Ped[ ,order(colnames(Ped))]
  
  g_slope_param <- as.vector(raw_data_list$g_slope_param)
  CV_calc <- 1 - Ped * g_slope_param[1]
  
  n_lit <- as.matrix(raw_data_list$n_lit_param)
  
  literature_data <- raw_data_list$literature_data
  priors_lit <- as.matrix(literature_data)
  priors_lit <- priors_lit[,order(colnames(priors_lit))]
  priors_lit <- priors_lit[order(rownames(priors_lit)),]
  
  switch_prior <- 1
  
  data_list <- list(
    y          = SIA_input,
    nb_y       = nb_y,
    o          = SCA_input,
    nb_o       = nb_o,
    nb_elem    = nb_elem,
    nb_group   = nb_group,
    nb_prey    = nb_prey,
    list_pred  = list_pred,
    list_prey  = list_prey,
    DELTA = tdf ,
    q     = el_conc,
    ZEROS = matrix(0, nrow = nb_elem, ncol = nb_group),
    ID    = diag(nb_elem),
    g     = Ped,
    CVs   = CV_calc,
    alpha_lit = priors_lit,
    n_lit = n_lit,
    switch_conf_3 = switch_prior
  )
  
  return(data_list)
}