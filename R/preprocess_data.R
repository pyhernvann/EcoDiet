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
                            trophic_enrichment_factor, literature_prior,
                            element_concentration = 1, binary_web = NULL){
  
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
  o <- as.matrix(stomach_data)
  
  nb_group <- ncol(stomach_data)
  
  # Check the stomachal data
  
  if (ncol(stomach_data) != nrow(stomach_data)){
    stop("You should have the same number of preys and predators in your stomachal data.")
  }
  if (!all(colnames(stomach_data) == rownames(stomach_data))){
    stop("The trophic groups in the rows and colums should have the same names in your stomachal data.")
  }
  
  # Rearrange the isotopic data
  
  isotope_data <- read.csv(isotope_data_path)
  isotope_data <- isotope_data[order(isotope_data$group), ]
  
  nb_elem <- ncol(isotope_data) - 2
  nb_y <- as.vector(table(isotope_data$group))
  
  y <- array(NA, dim = c(nb_group, max(nb_y) + 1, nb_elem))
  for (el in 1:nb_elem){
    y[, , el] <- as.matrix(reshape(isotope_data[, c(1, 2, el + 2)], direction = "wide", 
                                   timevar = "sample", idvar = "group"))
  }
  rownames(y) <- y[, 1, 1]
  y <- y[, -1, ]
  
  if (length(element_concentration) == 1){
    element_concentration <- matrix(element_concentration, nrow = nb_elem, ncol = nb_group)
  }
  
  # Check the isotopic data
  
  if (colnames(isotope_data)[1] != "group"){
    stop("The first column of the isotopic data should be named \"group\".")
  }
  if (colnames(isotope_data)[2] != "sample"){
    stop("The second column of the isotopic data should be named \"sample\".")
  }
  
  if (length(unique(isotope_data$group)) != ncol(stomach_data)){
    stop("You should have the same number of trophic groups in your stomachal and isotopic data.")
  }
  if (!all(as.vector(unique(isotope_data$group)) == colnames(stomach_data))){
    stop("The trophic groups in the isotopic data should have the same names as in your stomachal data.")
  }
  
  for (elem in 1:nb_elem){
    if (!all(rowSums(!is.na(y[, , elem])) == nb_y)){
      stop("You should have the same number of samples for each element within each trophic group.")
    }
  }
  
  # Constructs the binary web matrix from the stomachal data
  
  if (is.null(binary_web)){
    binary_web <- 1 * (stomach_data > 0)
  } 
  
  # Constructs the model indices from the binary web matrix
  
  nb_prey  <- colSums(binary_web)
  
  list_pred <- as.vector(which(colSums(binary_web) != 0))
  nb_pred <- length(list_pred)
  
  list_prey <- matrix(data = NA, nrow = nb_group, ncol = nb_group)
  for (i in 1:nb_group){
    if (sum(binary_web[, i]) > 0) {
      list_prey[i, 1:nb_prey[i]] <- as.vector(which(binary_web[, i] != 0))
    }
  }
  
  # Create the data list to feed the JAGS function
  
  data_list <- list(
    y          = y,
    nb_y       = nb_y,
    o          = o,
    nb_o       = nb_o,
    nb_elem    = nb_elem,
    nb_group   = nb_group,
    list_pred  = list_pred,
    list_prey  = list_prey,
    nb_prey    = nb_prey,
    DELTA      = trophic_enrichment_factor,
    q          = element_concentration,
    ZEROS      = matrix(0, nrow = nb_elem, ncol = nb_group),
    ID         = diag(nb_elem)
  )
  
  if (literature_prior == TRUE){
    
    literature_data <- raw_data_list$literature_data
    
    Pedigree_literature_data <- raw_data_list$pedigree_literature_data
    Ped <- as.matrix(Pedigree_literature_data)
    Ped <- Ped[ ,order(colnames(Ped))]
    
    g_slope_param <- as.vector(raw_data_list$g_slope_param)
    CV_calc <- 1 - Ped * g_slope_param[1]
    
    nb_lit <- as.matrix(raw_data_list$n_lit_param)
    
    literature_data <- raw_data_list$literature_data
    priors_lit <- as.matrix(literature_data)
    priors_lit <- priors_lit[,order(colnames(priors_lit))]
    priors_lit <- priors_lit[order(rownames(priors_lit)),]
    
    
    data_list <- c(data_list, list(
      ped     = Ped,
      CVs   = CV_calc,
      alpha_lit = priors_lit,
      nb_lit = nb_lit
    ))
  }
  
  return(data_list)
}