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

preprocess_data <- function(stomach_data_path, isotope_data_path, 
                            trophic_enrichment_factor, literature_prior){
  
  # Rearrange the stomachal data
  
  stomach_data <- read.csv(stomach_data_path)
  
  if (ncol(stomach_data) == nrow(stomach_data) && colnames(stomach_data)[1] == "X"){
    row.names(stomach_data) <- stomach_data[, 1]
    stomach_data[, 1] <- NULL
  }
  
  stomach_data <- stomach_data[, order(colnames(stomach_data))]
  
  nb_o <- stomach_data[nrow(stomach_data), ]
  nb_o <- as.matrix(nb_o)
  
  stomach_data <- stomach_data[-nrow(stomach_data), ]
  stomach_data <- stomach_data[order(rownames(stomach_data)), ]
  stomach_data <- as.matrix(stomach_data)
  
  if (ncol(stomach_data) != nrow(stomach_data)){
    stop("You should have the same number of preys and predators in your stomachal data.")
  }
  if (!all(colnames(stomach_data) == rownames(stomach_data))){
    stop("The trophic groups in the rows and colums should have the same names in your stomachal data.")
  }
  
  # Rearrange the isotopic data
  
  isotope_data <- read.csv(isotope_data_path)
  isotope_data <- isotope_data[order(isotope_data$Group), ] 
  
  nb_elem <- ncol(isotope_data) - 1
  
  SIA_input <- array(NA, dim = c(nb_elem, nb_group, max(table(isotope_data$Group))))
  names_grp <- rownames(topo_run)
  colnames(SIA_input) <- names_grp
  
  for (el in 1:nb_elem){
    for (grp in 1:nb_group){
      SIA_input[el, grp, ] <- isotope_data[isotope_data$Group==names_grp[grp], el + 1]
    }
  }
  
  nb_y <- apply(ifelse(!is.na(SIA_input), 1, 0), seq(1, nb_elem), sum)
  
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
  
  # parameters for the isotopic analysis
  
  el_conc <- as.matrix(raw_data_list$element_concentration)
  
  tdf <- as.vector(raw_data_list$mean_tdf)
  

  
  # Create the data list to feed the JAGS function
  
  data_list <- list(
    y          = SIA_input,
    nb_y       = nb_y,
    o          = stomach_data,
    nb_o       = nb_o,
    nb_elem    = nb_elem,
    nb_group   = nb_group,
    nb_prey    = nb_prey,
    list_pred  = list_pred,
    list_prey  = list_prey,
    DELTA = tdf ,
    q     = el_conc,
    ZEROS = matrix(0, nrow = nb_elem, ncol = nb_group),
    ID    = diag(nb_elem)
  )
  
  if (literature_prior == TRUE){
    
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
    
    
    data_list <- c(data_list, list(
      g     = Ped,
      CVs   = CV_calc,
      alpha_lit = priors_lit,
      n_lit = n_lit
    ))
  }
  
  return(data_list)
}