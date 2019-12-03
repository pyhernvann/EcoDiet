#' Load the raw data and process it to feed the EcoDiet model
#'
#' @param raw_data_list the list containing the different raw data objects
#' 
#' @return a list of processed data, ready to be run by the EcoDiet model
#' 
#' @examples
#' data <- load_data(ecodiet_example)
#'
#' @export

load_data <- function(raw_data_list){
  
  topo_run <- raw_data_list$topo_run
  topo_run <- topo_run[, order(colnames(topo_run))]
  topo_run <- topo_run[order(rownames(topo_run)), ]
  topo_run <- as.matrix(topo_run)
  
  # Constructs the index from the binary web matrix
  
  names_grp <- rownames(topo_run)
  nb_grp <- nrow(topo_run)
  nb_sources  <- colSums(topo_run)
  
  id_base <- which(colSums(topo_run) == 0)
  nb_base <- length(id_base)
  id_cons <- which(colSums(topo_run) != 0)
  nb_cons <- length(id_cons)
  
  id_source <- t(sapply(1:nb_cons,function(i){ 
    output <- rep(NA,max(nb_sources)) 
    output[1:nb_sources[id_cons][i]] <- which(topo_run[,id_cons[i]]==1) 
    output
  }))
  
  # stomachal data
  
  nb_stom_SCA_data <- raw_data_list$nb_stom_SCA_data
  nb_stom_SCA_data <- as.matrix(nb_stom_SCA_data[, order(colnames(nb_stom_SCA_data))])
  
  
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
  
  nb_elements <- ncol(SIA_data) - 1
  nb_max_spl <- max(table(SIA_data$Group))
  
  SIA_input <- array(NA, dim=c(nb_elements, nb_grp, nb_max_spl))
  colnames(SIA_input) <- names_grp
  
  for (el in 1:nb_elements){
    for (grp in 1:nb_grp){
      SIA_input[el, grp, ] <- SIA_data[SIA_data$Group==names_grp[grp], el + 1]
    }
  }
  
  nb_samples <- apply(ifelse(!is.na(SIA_input), 1, 0), seq(1, nb_elements), sum)
  
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
    y     = SIA_input	,
    o     = SCA_input,
    nb    = nb_base,
    nc   	= nb_cons,
    ns    = nb_sources,
    n_sia = nb_samples,
    ne    = nb_elements,
    DELTA = tdf ,
    q     = el_conc,
    ZEROS = matrix(0, nrow=nb_elements, ncol=nb_cons),
    ID    = diag(nb_elements),
    ic    = id_cons,
    is    = id_source,
    n_sca = nb_stom_SCA_data,
    g     = Ped,
    CVs   = CV_calc,
    alpha_lit = priors_lit,
    n_lit = n_lit,
    switch_conf_3 = switch_prior
  )
  
  return(data_list)
}