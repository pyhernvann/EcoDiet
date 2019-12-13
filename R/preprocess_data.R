#' Check the format of the stomachal and isotopic data and print an error message if something is not correct
#' 
#' @param stomach_data the preprocessed stomachal data
#' @param isotope_data the preprocessed isotopic data
#' 
#' @keywords internal

check_stomach_data <- function(stomach_data, nb_o){
  
  # Check the rows and columns of the stomachal data
  if (ncol(stomach_data) != nrow(stomach_data)){
    stop("You should have the same number of preys and predators in your stomachal data.
  But here you have ", nrow(stomach_data), " preys in the rows, and ", 
         ncol(stomach_data), " predators in the columns.")
  }
  if (!all(colnames(stomach_data) == rownames(stomach_data))){
    stop("The trophic groups should have the same names in the rows and the columns of the stomachal data.
  But here the trophic groups are named \"", paste(rownames(stomach_data), collapse = ", "),
         "\" in the rows and \"", paste(colnames(stomach_data), collapse = ", "), "\" in the colums.")
  }
  
  # Check the content of the stomachal data
  if (!is.integer(stomach_data)){
    stop("The stomachal data should contain only integer values, and not decimal or character.")
  }
  if (!all(stomach_data >= 0)){
    stop("The stomachal data should contain only positive values or zeros.")
  }
  if (!all(apply(stomach_data, 2, max) <= nb_o)){
    stop("The stomachal data cannot contain values higher than the number of full stomachs analyzed.")
  }
  
}


#' Check that the isotopic data is in a correct format and print an error message if not.
#' 
#' @param isotope_data the preprocessed isotopic data
#' 
#' @keywords internal

check_isotope_data <- function(isotope_data, stomach_data){
  
  # Check the column name of the isotopic data
  if (colnames(isotope_data)[1] != "group"){
    stop("The first column of the isotopic data should be named \"group\".
  But here it is named \"", colnames(isotope_data)[1],"\". Please rename it.")
  }

  # Check the coherence between the isotopic and stomachal data
  if (length(unique(isotope_data$group)) != ncol(stomach_data)){
    stop("You should have the same number of trophic groups in your stomachal and isotopic data.
  But here you have ", ncol(stomach_data), " trophic groups in your stomachal data (\"", 
         paste(colnames(stomach_data), collapse = ", "), "\"), and ", 
         length(unique(isotope_data$group)), " in your isotopic data(\"", 
         paste(unique(isotope_data$group), collapse = ", "), "\").")
  }
  if (!all(as.vector(unique(isotope_data$group)) == colnames(stomach_data))){
    stop("The trophic groups in the isotopic data and in the stomachal data should have the same names.
  But here your trophic groups are called: \"", paste(colnames(stomach_data), collapse = ", "), 
         "\" in your stomachal data, and: \"", paste(unique(isotope_data$group), collapse = ", "), 
         "\" in your isotopic data.")
  }
  
  # Check the content of the isotopic data
  if (sum(is.na(isotope_data[, 3:ncol(isotope_data)])) > 0){
    stop("The isotopic data should not contain NA values.
  But we have found NA here:
", paste("line", which(is.na(isotope_data[, 3:ncol(isotope_data)]), arr.ind = T)[, 1], 
         "column", which(is.na(isotope_data[, 3:ncol(isotope_data)]), arr.ind = T)[, 1] + 2, 
         collapse = ",\n"), ".
  Please enter a numerical value instead or remove the corresponding row(s).")
  }
}


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

preprocess_data <- function(stomach_data, isotope_data, 
                            trophic_enrichment_factor, literature_prior,
                            element_concentration = 1, binary_web = NULL){

  # Rearrange the stomachal data
  
  if (colnames(stomach_data)[1] == "X"){
    row.names(stomach_data) <- stomach_data[, 1]
    stomach_data[, 1] <- NULL
  }
  
  stomach_data <- stomach_data[, order(colnames(stomach_data))]
  
  nb_o <- stomach_data[nrow(stomach_data), ]
  nb_o <- as.matrix(nb_o)
  
  stomach_data <- stomach_data[-nrow(stomach_data), ]
  stomach_data <- stomach_data[order(rownames(stomach_data)), ]
  stomach_data <- as.matrix(stomach_data)
  
  check_stomach_data(stomach_data)
  
  nb_group <- ncol(stomach_data)
  
  # Rearrange the isotopic data
  
  check_isotope_data(isotope_data, stomach_data)
  
  nb_elem <- ncol(isotope_data) - 1
  nb_y <- as.vector(table(isotope_data$group))
  
  isotope_data <- isotope_data[order(isotope_data$group), ]
  
  sample <- vector()
  for (i in 1:nb_group){
    sample <- c(sample, 1:table(isotope_data$group)[i])
  }
  isotope_data$sample <- sample

  y <- array(NA, dim = c(nb_group, max(nb_y) + 1, nb_elem))
  for (el in 1:nb_elem){
    y[, , el] <- as.matrix(reshape(isotope_data[, c(1, el + 1, nb_elem + 2)], direction = "wide", 
                                   timevar = "sample", idvar = "group"))
  }
  rownames(y) <- y[, 1, 1]
  y <- y[, -1, ]
  
  if (length(element_concentration) == 1){
    element_concentration <- matrix(element_concentration, nrow = nb_elem, ncol = nb_group)
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
    o          = stomach_data,
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