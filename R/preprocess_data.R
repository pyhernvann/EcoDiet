#' Check an input boolean parameter
#'
#' @param boolean_parameter the boolean parameter to check
#' @param parameter_name its name
#'
#' @keywords internal
#' @noRd

check_boolean_parameter <- function(boolean_parameter, parameter_name){
  
  # Check that this is a logical vector
  if (!is.logical(boolean_parameter)){
    stop("The ", parameter_name, " parameter should be TRUE or FALSE, not anything else.")
  }
  
  # Check that this is a vector of length one
  if (length(boolean_parameter) != 1){
    stop("The ", parameter_name, " parameter should have only one element and not many.\n",
         "  But here it is a vector of lenght ", length(boolean_parameter), ".\n",
         "  Please do not enter a ", parameter_name, " argument or use either:",
         " \"", parameter_name, " = TRUE\" or \"", parameter_name, " = FALSE\".")
  }
  
}


#' Check the stomach content data
#'
#' @param stomach_data the preprocessed stomach content data
#'
#' @keywords internal
#' @noRd

check_stomach_data <- function(stomach_data){

  # Check the rows and columns of the stomachal data
  if (rownames(stomach_data)[nrow(stomach_data)] != "full"){
    stop("The last row of the stomachal data should be named \"full\".\n",
         "  But here it is named \"", rownames(stomach_data)[nrow(stomach_data)], "\".\n",
         "  Please rename it.")
  }
  if (ncol(stomach_data) != nrow(stomach_data) - 1){
    stop("You should have the same number of preys and predators in your stomachal data.\n",
         "  But here you have ", nrow(stomach_data) - 1, " preys in the rows (\"",
         paste(rownames(stomach_data)[-nrow(stomach_data)], collapse = ", "), "\"), and ",
         ncol(stomach_data), " predators in the columns (\"",
         paste(colnames(stomach_data), collapse = ", "), "\").")
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
    stop("The stomachal data should only contain integer values, and not decimal values or text.\n",
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
         paste(names(which(!apply(stomach_data[-nrow(stomach_data), ], 2, max) <=
                             stomach_data[nrow(stomach_data), ])),
               collapse = "\", \""),
         "\".\n  Please change this number to a normal value.")
  }

}


#' Check the biotracer data
#'
#' @param biotracer_data the input biotracer data
#' @param stomach_data the preprocessed stomach content data 
#'   to verify the consistence between the biotracer and stomach
#'   data
#'
#' @keywords internal
#' @noRd

check_biotracer_data <- function(biotracer_data, stomach_data){

  # Check the column name of the biotracer data
  if (colnames(biotracer_data)[1] != "group"){
    stop("The first column of the biotracer data should be named \"group\".\n",
  "  But here it is named \"", colnames(biotracer_data)[1],"\".\n  Please rename it.")
  }

  # Check the coherence between the biotracer and stomachal data
  if (length(unique(biotracer_data$group)) != ncol(stomach_data)){
    stop("You should have the same number of trophic groups in your stomachal and biotracer data.\n",
         "  But here you have ", ncol(stomach_data), " trophic groups in your stomachal data (\"",
         paste(colnames(stomach_data), collapse = ", "), "\"), and ",
         length(unique(biotracer_data$group)), " in your biotracer data (\"",
         paste(sort(unique(biotracer_data$group)), collapse = ", "), "\").\n",
         "  Please put the same number of trophic groups in both datasets.")
  }
  if (!all(as.vector(sort(unique(biotracer_data$group))) == colnames(stomach_data))){
    stop("The trophic groups in the biotracer data and the stomachal data should have the same names.\n",
         "  But here your trophic groups are called: \"", paste(colnames(stomach_data), collapse = ", "),
         "\" in your stomachal data, and: \"", paste(sort(unique(biotracer_data$group)), collapse = ", "),
         "\" in your biotracer data.\n",
         "  Please rename them to be consistent.")
  }

  # Check the content of the biotracer data
  if (!is.double(as.matrix(biotracer_data[, -1]))){
    stop("The biotracer data should only contain numbers, and not text.\n",
         "  Please remove the values that do not correspond to an biotracer measurement.")
  }
  
  na_count <- sapply(biotracer_data[, -1], function(x) sum(is.na(x)))
  if (!all(na_count == na_count[1])){
    stop("The biotracer data should not contain individuals with both NA and regular measures.\n",
         "  But we have found a different number of NA for the biotracers: \n",
         paste(capture.output(na_count), collapse = "\n"),
         "\n  Please remove the individuals for which not all the biotracer measures were done.")
  }
}


#' Check the trophic discrimination facto
#'
#' @param trophic_discrimination_factor the input trophic discrimination factor data
#' @param biotracer_data the input biotracer data as a reference
#'
#' @keywords internal
#' @noRd

check_tef_data <- function(trophic_discrimination_factor, biotracer_data){

  # Check the format
  if (!is.null(dim(trophic_discrimination_factor))){
    stop("The trophic discrimination factor should be a vector, and not a matrix or an array.\n",
         "  Please enter a vector as in the vignette's example: \"trophic_discrimination_factor = c(0.8, 3.4)\".")
  }

  # Check the content
  if (!is.double(trophic_discrimination_factor)){
    stop("The trophic discrimination factor should contain only numbers, not text.\n",
         "  But here are the trophic enrichement factors you entered: ",
         paste(trophic_discrimination_factor, collapse = ", "), "\n  Please use numbers instead.")
  }
  if (sum(is.na(trophic_discrimination_factor)) > 0){
    stop("The trophic discrimination factor should not contain NA or NaN.\n",
         "  But we have found a NA for the discrimination factor corresponding to the \"",
         colnames(biotracer_data)[-1][which(is.na(trophic_discrimination_factor))[1]], "\" biotracer.\n",
         "  Please enter a number instead.")
  }

  # Check whether the length is consistent with the biotracer data
  if (length(trophic_discrimination_factor) != ncol(biotracer_data) - 1){
    stop("There should be as many trophic discrimination factors as",
         "there are chemical elements in the biotracer data.\n",
         "  But here there are actually ", length(trophic_discrimination_factor),
         " trophic enrichement factors (\"", paste(trophic_discrimination_factor, collapse = ", "),
         "\") and ", ncol(biotracer_data) - 1, " chemical elements (\"",
         paste(colnames(biotracer_data)[-1], collapse = ", "), "\") in the biotracer data.")
  }
}


#' Check the literature diet matrix
#'
#' @param literature_diets the preprocessed literature diet matrix
#' @param biotracer_data the preprocessed biotracer data as a reference
#'
#' @keywords internal
#' @noRd

check_literature_diets <- function(literature_diets, biotracer_data){

  # Check the rows and columns of the literature diets
  if (rownames(literature_diets)[nrow(literature_diets)] != "pedigree"){
    stop("The last row of the literature diets matrix should be named \"pedigree\".\n",
         "  But here it is named \"", rownames(literature_diets)[nrow(literature_diets)], "\".\n",
         "  Please rename it.")
  }
  if (ncol(literature_diets) != nrow(literature_diets) - 1){
    stop("You should have the same number of preys and predators in your literature diets.\n",
         "  But here you have ", nrow(literature_diets) - 1, " preys in the rows (\"",
         paste(rownames(literature_diets)[-nrow(literature_diets)], collapse = ", "), "\"), and ",
         ncol(literature_diets), " predators in the columns (\"",
         paste(colnames(literature_diets), collapse = ", "), "\").")
  }
  if (!all(colnames(literature_diets) == rownames(literature_diets)[-nrow(literature_diets)])){
    stop("The trophic groups should have the same names in the rows and the columns of the literature diets.\n",
         "  But here the trophic groups are named \"",
         paste(rownames(literature_diets)[-nrow(literature_diets)], collapse = ", "), "\" in the rows and \"",
         paste(colnames(literature_diets), collapse = ", "), "\" in the colums.\n",
         "  Please rename them to be consistent.")
  }

  # Check the coherence between the biotracer data and the literature diets
  if (length(unique(biotracer_data$group)) != ncol(literature_diets)){
    stop("You should have the same number of trophic groups in your literature diets and your biotracer data.\n",
         "  But here you have ", ncol(literature_diets), " trophic groups in your literature diets (\"",
         paste(colnames(literature_diets), collapse = ", "), "\"), and ",
         length(unique(biotracer_data$group)), " in your biotracer data (\"",
         paste(sort(unique(biotracer_data$group)), collapse = ", "), "\").\n",
         "  Please put the same number of trophic groups in both.")
  }
  if (!all(as.vector(sort(unique(biotracer_data$group))) == colnames(literature_diets))){
    stop("The trophic groups in the biotracer data and the literature diets should have the same names.\n",
         "  But here your trophic groups are called: \"", paste(colnames(literature_diets), collapse = ", "),
         "\" in your literature diets, and: \"", paste(sort(unique(biotracer_data$group)), collapse = ", "),
         "\" in your biotracer data.\n",
         "  Please rename them to be consistent.")
  }

  # Check the content of the literature diets
  if (!is.double(literature_diets)){
    stop("The literature diets should only contain numbers, and not text.\n",
         "  Please remove the values that do not correspond to a number.")
  }
  if (sum(is.na(literature_diets)) > 0){
    stop("The literature diets data should not contain NA or NaN.\n",
         "  But the literature diet estimator for the predator \"",
         colnames(literature_diets)[which(is.na(literature_diets), arr.ind = T)[, 2][1]],
         "\" contain abnormal values.\n",
         "  Please enter a value between 0 and 1 instead.")
  }
  if (!all((literature_diets >= 0) & (literature_diets <= 1))){
    stop("The literature diets should only contain values between 0 and 1.\n",
         "  Please remove the abnormal values.")
  }
  if (!all((colSums(literature_diets[-nrow(literature_diets), ]) == 0) |
           ((colSums(literature_diets[-nrow(literature_diets), ]) > 0.99) & (
             colSums(literature_diets[-nrow(literature_diets), ]) < 1.01)))){
    stop("Each column of the literature diets matrix should sum to one or be entirely filled with zeros.\n",
         "  But it is not the case with the \"",
         names(which(((colSums(literature_diets[-nrow(literature_diets), ]) == 0) |
                        ((colSums(literature_diets[-nrow(literature_diets), ]) > 0.99) & (
                          colSums(literature_diets[-nrow(literature_diets), ]) < 1.01))) == FALSE))[1],
         "\" column.\n  Please change that column.")
  }
}

#' Check an input numeric parameter
#'
#' @param numeric_parameter the numeric parameter to check
#' @param parameter_name its name
#'
#' @keywords internal
#' @noRd

check_numeric_parameter <- function(numeric_parameter, parameter_name){

  if (!(is.double(numeric_parameter) | is.integer(numeric_parameter))){
    stop("You need to enter a number for the \"", parameter_name, "\" parameter, and not a text or anything else",
         ".\n  Please change it.")
  }

  if (length(numeric_parameter) != 1){
    stop("You can enter only one number for the \"", parameter_name, "\" parameter.\n",
         "  Please put only one value for this parameter.")
  }
  
  if (numeric_parameter < 0){
    stop("You have entered a negative number for the \"", parameter_name, "\" parameter.\n",
         "  Please use only a null or positive number.")
  }

}


#' Check and preprocess the data
#' 
#' @description This function preprocesses the data input by the user, checks that the different inputs 
#' have the right format, and creates the data list that will feed the JAGS model.
#' 
#' If an error appears with a clear message, it means that the input needs to be reformatted. Please 
#' follow the instructions in the error message. You can also look at the data examples to guide you.
#'
#' @param biotracer_data A dataframe containing the biotracer data in the specific format: the first column
#'   corresponds to the trophic group or latin species and the remaining columns contains the biotracer
#'   measures
#' @param trophic_discrimination_factor A vector containing the trophic discrimination factors 
#'   corresponding to each column found in the biotracer data (except the group column of course)
#' @param literature_configuration A boolean (TRUE or FALSE) indicating whether the model will have
#'   prior distributions informed by a literature study
#' @param topology A matrix that the user may input if she wants the model to investigate some 
#'   additionnal trophic links (by-default it is set on NULL and defined later from the stomach content 
#'   data and the literature diets if present)
#' @param element_concentration A matrix containing the element concentration for each trophic group and
#'   each biotracer element (listed in the biotracer data). It is a matrix with as many columns
#'   as the number of trophic groups and as many rows as the number of elements. By default the matrix
#'   is filled with ones.
#' @param stomach_data A dataframe containing the stomach content data in a specific format: the first row
#'   contains the names of the prey trophic groups, the headers contains the names of the consumer / 
#'   predator trophic groups, and the rest are the number of the predator's stomachs in which this prey
#'   was found. The last row contains the total number of non-empty stomach for the corresponding
#'   predator.
#' @param rescale_stomach A boolean (TRUE or FALSE) indicating whether the stomach content data will be rescaled.
#'   If TRUE, the stomach occurences are upscaled by dividing them by the maximum occurrences / the number
#'   of non-empty stomach.
#' @param literature_diets A dataframe containing the diet proportions found in the literature
#'   in a format similar to the stomach content data: the first row contains the names of the prey 
#'   trophic groups, the headers contains the names of the consumer / predator trophic groups, 
#'   and the rest are the average proportions of this prey in the predator's diet according to a 
#'   literature study. The last row contains the pedigree score associated to the literature findings 
#'   for each predators, a number between 0 and 1 indicating how much the literature findings are relevant
#'   estimates for the input data.
#' @param nb_literature A vector of one number containing the equivalent number of stomach 
#' for the literature priors on the eta variable
#' @param literature_slope A vector of one number containing the slope of the linear relationship 
#'   between the pedigree scores and the PIs' coefficients of variation (CVs)
#'
#' @return A list of preprocessed data, ready to be fed to the EcoDiet model
#' 
#' @examples
#' 
#' example_biotracer_data <- read.csv(system.file("extdata", "example_biotracer_data.csv",
#'                                                package = "EcoDiet"))
#' example_stomach_data <- read.csv(system.file("extdata", "example_stomach_data.csv",
#'                                              package = "EcoDiet"))
#'
#' data <- preprocess_data(biotracer_data = example_biotracer_data,
#'                         trophic_discrimination_factor = c(0.8, 3.4),
#'                         literature_configuration = FALSE,
#'                         stomach_data = example_stomach_data)
#'                         
#' example_literature_diets <- read.csv(system.file("extdata", "example_literature_diets.csv",
#'                                                  package = "EcoDiet"))
#'                         
#' data2 <- preprocess_data(biotracer_data = example_biotracer_data,
#'                          trophic_discrimination_factor = c(0.8, 3.4),
#'                          literature_configuration = literature_configuration,
#'                          stomach_data = example_stomach_data,
#'                          literature_diets = example_literature_diets,
#'                          nb_literature = 10,
#'                          literature_slope = 0.5)
#'
#' @export

preprocess_data <- function(biotracer_data, trophic_discrimination_factor,
                            literature_configuration = FALSE,
                            topology = NULL,
                            element_concentration = 1,
                            stomach_data = NULL,
                            rescale_stomach = FALSE,
                            literature_diets = NULL,
                            nb_literature, literature_slope){
  
  check_boolean_parameter(literature_configuration, "literature_configuration")
  check_boolean_parameter(rescale_stomach,          "rescale_stomach")

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
  
  # Upscale the stomach occurences if rescale_stomach is TRUE
  if (rescale_stomach){
    max_occurences <- apply(stomach_data, FUN = max, 2, na.rm = TRUE)
    for (j in 1:ncol(stomach_data)) {
      if (max_occurences[j] != 0) {
        scaled <- round(stomach_data[, j] * nb_o[j] / max_occurences[j])
      }
      for (i in 1:nrow(stomach_data)) {
        stomach_data[i, j] <- min(scaled[i], nb_o[j])
      }
    }
  }

  # Check the biotracer and trophic enrichement factor data
  check_biotracer_data(biotracer_data, stomach_data)
  check_tef_data(trophic_discrimination_factor, biotracer_data)

  # Rearrange the biotracer data
  biotracer_data <- biotracer_data[order(biotracer_data$group), ]

  nb_group <- length(unique(biotracer_data$group))
  nb_elem <- ncol(biotracer_data) - 1
  nb_y <- as.vector(table(biotracer_data$group))

  y <- array(NA, dim = c(nb_group, max(nb_y), nb_elem))
  rownames(y) <- colnames(stomach_data)
  for (el in 1:nb_elem){
    for (grp in 1:nb_group){
      y[grp, 1:nb_y[grp], el] <- biotracer_data[biotracer_data$group == colnames(stomach_data)[grp], el + 1]
    }
  }

  # Construct the element concentration matrix
  if (length(element_concentration) == 1){
    element_concentration <- matrix(element_concentration, nrow = nb_elem, ncol = nb_group)
  }

  if (literature_configuration){
    # Check that the user entered a literature diets matrix
    if (is.null(literature_diets)){
      stop("You need to enter a literature diets matrix if you want to use ",
           "the function `preprocess_data` with the argument `literature_configuration = TRUE`.")
    }

    # Re-arrange the literature diets matrix
    if (colnames(literature_diets)[1] == "X"){
      row.names(literature_diets) <- literature_diets[, 1]
      literature_diets[, 1] <- NULL
    }

    literature_diets <- literature_diets[, order(colnames(literature_diets))]
    literature_diets <- rbind(literature_diets[order(rownames(literature_diets)[-nrow(literature_diets)]), ],
                              literature_diets[nrow(literature_diets), ])
    literature_diets <- as.matrix(literature_diets)

    # Check the literature diets matrix
    check_literature_diets(literature_diets, biotracer_data)

    # Create the pedigree vector and the literature diets only matrix from it
    literature_pedigrees <- as.vector(literature_diets[nrow(literature_diets), ])
    literature_diets <- literature_diets[-nrow(literature_diets), ]
  }

  # Construct the binary web matrix from the stomachal data (and the literature diets if it is defined)
  if (is.null(topology)){
    if (literature_configuration){
      topology <- 1 * ((stomach_data > 0) | (literature_diets > 0))
    } else {
      topology <- 1 * (stomach_data > 0)
    }
  }

  # Print the trophic links
  message("The model will investigate the following trophic links:\n",
          paste(capture.output(topology), "\n", sep=""))

  # Constructs the model indices from the binary web matrix
  nb_prey  <- colSums(topology)

  list_pred <- as.vector(which(colSums(topology) != 0))
  nb_pred <- length(list_pred)

  list_prey <- matrix(data = NA, nrow = nb_group, ncol = nb_group)
  for (i in 1:nb_group){
    if (sum(topology[, i]) > 0) {
      list_prey[i, 1:nb_prey[i]] <- as.vector(which(topology[, i] != 0))
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
    DELTA      = trophic_discrimination_factor,
    q          = element_concentration,
    ZEROS      = matrix(0, nrow = nb_elem, ncol = nb_group),
    ID         = diag(nb_elem)
  )

  if (literature_configuration){

    check_numeric_parameter(nb_literature, "nb_literature")

    # Compute the zdeta parameter from the literature pedigree and the literature number
    dzeta <- nb_literature * literature_pedigrees

    # Compute the eta hyperparameters from the zeta parameter and the topology matrix
    eta_hyperparam_1 <- matrix(data = NA, nrow = nb_group, ncol = nb_group)
    eta_hyperparam_2 <- matrix(data = NA, nrow = nb_group, ncol = nb_group)
    for (i in list_pred){
      for (k in list_prey[i, 1:nb_prey[i]]){
        eta_hyperparam_1[k, i] <- ifelse(literature_diets[k, i] == 0, 1, (dzeta[i] + 1))
        eta_hyperparam_2[k, i] <- ifelse(literature_diets[k, i] == 0, (dzeta[i] + 1), 1)
      }
    }

    check_numeric_parameter(literature_slope, "literature_slope")
    
    if (literature_slope > 1){
      stop("You cannot use a number above 1 for the \"literature_slope\" parameter.\n",
           "  Please put only a number between 0 and 1.")
      }
    # Create the coefficients of variation from the literature pedigree and the slope parameter
    CVs_literature <- 1 - literature_pedigrees * literature_slope

    # Supplement the data list in the case of priors from the literature
    data_list <- c(data_list, list(
      alpha_lit        = literature_diets,
      eta_hyperparam_1 = eta_hyperparam_1,
      eta_hyperparam_2 = eta_hyperparam_2,
      CVs              = CVs_literature
      ))
  }

  return(data_list)
}
