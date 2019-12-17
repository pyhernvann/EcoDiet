#' Check the format of the stomachal and isotopic data and print an error message if something is not correct
#' 
#' @param stomach_data the almost raw stomachal data
#' 
#' @keywords internal

check_stomach_data <- function(stomach_data){
  
  # Check the rows and columns number of the stomachal data
  if (ncol(stomach_data) != nrow(stomach_data) - 1){
    stop("You should have the same number of preys and predators in your stomachal data.\n",
         "  But here you have ", nrow(stomach_data) - 1, " preys in the rows (\"", 
         paste(rownames(stomach_data)[-nrow(stomach_data)], collapse = ", "), "\"), and ", 
         ncol(stomach_data), " predators in the columns (\"", 
         paste(colnames(stomach_data), collapse = ", "), "\").")
  }
  
  # Check the rows and columns names of the stomachal data
  if (rownames(stomach_data)[nrow(stomach_data)] != "full"){
    stop("The last row of the stomachal data should be named \"full\".\n",
         "  But here it is named \"", rownames(stomach_data)[nrow(stomach_data)], "\".\n",
         "  Please rename it.")
  }
  if (!all(colnames(stomach_data) == rownames(stomach_data)[-nrow(stomach_data)])){
    stop("The trophic groups should have the same names in the rows and the columns of the stomachal data.\n",
         "  But here the trophic groups are named \"", 
         paste(rownames(stomach_data)[-nrow(stomach_data)], collapse = ", "), "\" in the rows and \"",
         paste(colnames(stomach_data), collapse = ", "), "\" in the colums.\n",
         "  Please rename them to be consistent.")
  }
  
  # Check the content of the stomachal data
  if (!is.integer(stomach_data)){
    stop("The stomachal data should only contain integer values, and not decimal or character.\n",
         "  Please remove the values that do not correspond to a number of stomachs.")
  }
  if (sum(is.na(stomach_data)) > 0){
    stop("The stomachal data should not contain NA or NaN.\n",
         "  But the number of stomachs from the predator \"",
         colnames(stomach_data)[which(is.na(stomach_data), arr.ind = T)[, 2][1]],
         "\" contain abnormal values.\n",
         "  Please enter a positive value or a zero instead.")
  }
  if (!all(stomach_data >= 0)){
    stop("The stomachal data should only contain positive values or zeros.\n",
         "  But we have found negative integers here:\n",
         paste("line", which(!stomach_data >= 0, arr.ind = T)[, 1], 
                  "column", which(!stomach_data >= 0, arr.ind = T)[, 2], collapse = ",\n"),
         ".\n  Please enter a positive value or a zero instead.")
  }
  if (!all(apply(stomach_data[-nrow(stomach_data), ], 2, max) <= stomach_data[nrow(stomach_data), ])){
    stop("The stomachal data should not contain values higher than the number of full stomachs analyzed.\n",
         "  But a prey is said to be found in more stomachs ", 
         "than the total number of stomachs for the predators named \"", 
         paste(names(which(!apply(stomach_data[-nrow(stomach_data), ], 2, max) <= stomach_data[nrow(stomach_data), ])), 
               collapse = "\", \""), 
         "\".\n  Please change this number to a normal value.")
  }
  
}


#' Check that the isotopic data is in a correct format and print an error message if not.
#' 
#' @param isotope_data the raw isotopic data
#' @param stomach_data the preprocessed stomachal data
#' 
#' @keywords internal

check_isotope_data <- function(isotope_data, stomach_data){
  
  # Check the column name of the isotopic data
  if (colnames(isotope_data)[1] != "group"){
    stop("The first column of the isotopic data should be named \"group\".\n",
  "  But here it is named \"", colnames(isotope_data)[1],"\".\n  Please rename it.")
  }

  # Check the coherence between the isotopic and stomachal data
  if (length(unique(isotope_data$group)) != ncol(stomach_data)){
    stop("You should have the same number of trophic groups in your stomachal and isotopic data.\n",
         "  But here you have ", ncol(stomach_data), " trophic groups in your stomachal data (\"", 
         paste(colnames(stomach_data), collapse = ", "), "\"), and ", 
         length(unique(isotope_data$group)), " in your isotopic data (\"", 
         paste(sort(unique(isotope_data$group)), collapse = ", "), "\").\n",
         "  Please put the same number of trophic groups in both datasets.")
  }
  if (!all(as.vector(sort(unique(isotope_data$group))) == colnames(stomach_data))){
    stop("The trophic groups in the isotopic data and in the stomachal data should have the same names.\n",
         "  But here your trophic groups are called: \"", paste(colnames(stomach_data), collapse = ", "), 
         "\" in your stomachal data, and: \"", paste(sort(unique(isotope_data$group)), collapse = ", "), 
         "\" in your isotopic data.\n",
         "  Please rename them to be consistent.")
  }
  
  # Check the content of the isotopic data
  if (!is.numeric(as.matrix(isotope_data[, -1]))){
    stop("The isotope data should only contain numbers, and not text.\n",
         "  Please remove the values that do not correspond to isotopic measures.")
  }
  if (sum(is.na(isotope_data[, -1])) > 0){
    stop("The isotopic data should not contain NA or NaN.\n",
         "  But we have found at least one NA in the \"",
         isotope_data[which(is.na(isotope_data), arr.ind = T)[, 1][1], 1],
         "\" trophic group for the \"",
         colnames(isotope_data)[which(is.na(isotope_data), arr.ind = T)[, 2][1]], 
         "\" measurement.\n",
         "  Please enter a number instead or remove the corresponding row.")
  }
  if (sum(is.infinite(as.matrix(isotope_data[, -1]))) > 0){
    stop("The isotopic data should not contain Infinite values (Inf).\n",
         "  Please enter a number instead or remove the corresponding row.")
  }
}


#' Check that the trophic enrichment factor is in a correct format and print an error message if not.
#' 
#' @param trophic_enrichment_factor the raw trophic enrichment factor data
#' @param isotope_data the raw isotopic data
#' 
#' @keywords internal

check_tef_data <- function(trophic_enrichment_factor, isotope_data){
  # Check the format
  if (!is.vector(trophic_enrichment_factor)){
    stop("The trophic enrichment factor should be a vector.\n",
         "  Please enter a vector as in the vignette example: \"trophic_enrichment_factor = c(0.8, 3.4)\".")
  }
  if (!is.numeric(trophic_enrichment_factor)){
    stop("The trophic enrichment factor should contain only numbers.\n",
         "  But here are the trophic enrichement factors you entered: ", 
         paste(trophic_enrichment_factor, collapse = ", "), "\n  Please use numbers instead.")
  }
  
  # Check whether the length is consistent with the isotopic data
  if (length(trophic_enrichment_factor) != ncol(isotope_data) - 1){
    stop("There should be as many trophic enrichment factors as there are chemical elements in the isotopic data.\n",
         "  But here there are actually ", length(trophic_enrichment_factor), 
         " trophic enrichement factors (\"", paste(trophic_enrichment_factor, collapse = ", "), 
         "\") and ", ncol(isotope_data) - 1, " chemical elements (\"", 
         paste(colnames(isotope_data)[-1], collapse = ", "), "\") in the isotopic data.")
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
  
  if (!is.logical(literature_prior)){
    stop("The literature_prior should be TRUE or FALSE, not anything else.")
  }

  # Rearrange the stomachal data
  
  if (colnames(stomach_data)[1] == "X"){
    row.names(stomach_data) <- stomach_data[, 1]
    stomach_data[, 1] <- NULL
  }
  
  stomach_data <- stomach_data[, order(colnames(stomach_data))]
  stomach_data <- rbind(stomach_data[order(rownames(stomach_data)[-nrow(stomach_data)]), ], 
                        stomach_data[nrow(stomach_data), ])
  stomach_data <- as.matrix(stomach_data)
  
  check_stomach_data(stomach_data)
  
  nb_o <- stomach_data[nrow(stomach_data), ]
  stomach_data <- stomach_data[-nrow(stomach_data), ]
  nb_group <- ncol(stomach_data)
  
  # Check the isotopic and trophic enrichement factor data
  
  check_isotope_data(isotope_data, stomach_data)
  check_tef_data(trophic_enrichment_factor, isotope_data)
  
  # Rearrange the isotopic data
  
  isotope_data <- isotope_data[order(isotope_data$group), ]
  
  nb_elem <- ncol(isotope_data) - 1
  nb_y <- as.vector(table(isotope_data$group))

  y <- array(NA, dim = c(nb_group, max(nb_y) + 1, nb_elem))
  rownames(y) <- colnames(stomach_data)
  for (el in 1:nb_elem){
    for (grp in 1:nb_group){
      y[grp, 1:nb_y[grp], el] <- isotope_data[isotope_data$group == colnames(stomach_data)[grp], el + 1]
    }
  }
  
  # Construct the element concentration matrix
  
  if (length(element_concentration) == 1){
    element_concentration <- matrix(element_concentration, nrow = nb_elem, ncol = nb_group)
  }
  
  # Construct the binary web matrix from the stomachal data
  
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