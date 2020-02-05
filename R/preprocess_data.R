#' Check the format of the stomachal and isotopic data and print an error message
#' if something is not correct
#'
#' @param stomach_data the almost raw stomachal data
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


#' Check that the isotopic data is in a correct format and print an error message if not.
#'
#' @param isotope_data the raw isotopic data
#' @param stomach_data the preprocessed stomachal data
#'
#' @keywords internal
#' @noRd

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
    stop("The trophic groups in the isotopic data and the stomachal data should have the same names.\n",
         "  But here your trophic groups are called: \"", paste(colnames(stomach_data), collapse = ", "),
         "\" in your stomachal data, and: \"", paste(sort(unique(isotope_data$group)), collapse = ", "),
         "\" in your isotopic data.\n",
         "  Please rename them to be consistent.")
  }

  # Check the content of the isotopic data
  if (!is.double(as.matrix(isotope_data[, -1]))){
    stop("The isotope data should only contain numbers, and not text.\n",
         "  Please remove the values that do not correspond to an isotopic measurement.")
  }
}


#' Check that the trophic discrimination factor is in a correct format and print an error message if not.
#'
#' @param trophic_discrimination_factor the raw trophic discrimination factor data
#' @param isotope_data the raw isotopic data
#'
#' @keywords internal
#' @noRd

check_tef_data <- function(trophic_discrimination_factor, isotope_data){

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
         colnames(isotope_data)[-1][which(is.na(trophic_discrimination_factor))[1]], "\" isotope.\n",
         "  Please enter a number instead.")
  }

  # Check whether the length is consistent with the isotopic data
  if (length(trophic_discrimination_factor) != ncol(isotope_data) - 1){
    stop("There should be as many trophic discrimination factors as",
         "there are chemical elements in the isotopic data.\n",
         "  But here there are actually ", length(trophic_discrimination_factor),
         " trophic enrichement factors (\"", paste(trophic_discrimination_factor, collapse = ", "),
         "\") and ", ncol(isotope_data) - 1, " chemical elements (\"",
         paste(colnames(isotope_data)[-1], collapse = ", "), "\") in the isotopic data.")
  }
}

#' Check the literature_configuration argument
#'
#' @param literature_configuration the entered literature configuration argument
#'
#' @keywords internal
#' @noRd

check_literature_configuration <- function(literature_configuration){

  # Check that this is a logical vector
  if (!is.logical(literature_configuration)){
    stop("The literature_configuration should be TRUE or FALSE, not anything else.")
  }

  # Check that this is a vector of length one
  if (length(literature_configuration) != 1){
    stop("The literature_configuration should have only one element and not many.\n",
         "  But here it is a vector of lenght ", length(literature_configuration), ".\n",
         "  Please do not enter a literature_configuration argument or use either:",
         " \"literature_configuration = TRUE\" or \"literature_configuration = FALSE\".")
  }

}


#' Check that the literature diets matrix is in a correct format and print an error message if not.
#'
#' @param literature_diets the preprocessed literature diets matrix
#' @param isotope_data the preprocessed isotopic data
#'
#' @keywords internal
#' @noRd

check_literature_diets <- function(literature_diets, isotope_data){

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

  # Check the coherence between the isotopic data and the literature diets
  if (length(unique(isotope_data$group)) != ncol(literature_diets)){
    stop("You should have the same number of trophic groups in your literature diets and your isotopic data.\n",
         "  But here you have ", ncol(literature_diets), " trophic groups in your literature diets (\"",
         paste(colnames(literature_diets), collapse = ", "), "\"), and ",
         length(unique(isotope_data$group)), " in your isotopic data (\"",
         paste(sort(unique(isotope_data$group)), collapse = ", "), "\").\n",
         "  Please put the same number of trophic groups in both.")
  }
  if (!all(as.vector(sort(unique(isotope_data$group))) == colnames(literature_diets))){
    stop("The trophic groups in the isotopic data and the literature diets should have the same names.\n",
         "  But here your trophic groups are called: \"", paste(colnames(literature_diets), collapse = ", "),
         "\" in your literature diets, and: \"", paste(sort(unique(isotope_data$group)), collapse = ", "),
         "\" in your isotopic data.\n",
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

#' Check that the numeric parameter entered has the correct format
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

}


#' Load and preprocess the data to feed the EcoDiet model
#'
#' @param isotope_data the table containing the isotopic data in the specific format
#' @param trophic_discrimination_factor a vector containing the trophic discrimination factors corresponding
#' to each biotracer found in the biotracer data
#' corresponding to each column of the isotope data table (except the group column)
#' @param literature_configuration a boolean (TRUE or FALSE) indicating whether the model will have
#' prior distributions informed by a literature study
#' @param topology a matrix that the user may input if she wants the model to
#' investigate some additionnal trophic links (by default it is NULL and defined from the stomach
#' data and the alpha priors if they are defined)
#' @param stomach_data the table containing the stomachal data in a specific format
#' @param literature_diets the diet proportions and their associated pedigrees found in the literature
#' @param nb_literature the equivalent number of stomach for the literature priors
#' @param literature_slope the slope of the linear relationship between the pedigrees
#' and the PIs' coefficients of variation (CVs)
#'
#' @return a list of preprocessed data, ready to be run by the EcoDiet model
#'
#' @export

preprocess_data <- function(isotope_data, trophic_discrimination_factor,
                            literature_configuration = FALSE,
                            topology = NULL,
                            element_concentration = 1,
                            stomach_data = NULL,
                            literature_diets = NULL,
                            nb_literature, literature_slope){

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

  # Check the isotopic and trophic enrichement factor data
  check_isotope_data(isotope_data, stomach_data)
  check_tef_data(trophic_discrimination_factor, isotope_data)

  # Rearrange the isotopic data
  isotope_data <- isotope_data[order(isotope_data$group), ]

  nb_group <- length(unique(isotope_data$group))
  nb_elem <- ncol(isotope_data) - 1
  nb_y <- as.vector(table(isotope_data$group))

  y <- array(NA, dim = c(nb_group, max(nb_y), nb_elem))
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

  check_literature_configuration(literature_configuration)

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
    check_literature_diets(literature_diets, isotope_data)

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
        eta_hyperparam_1[k, i] <- ifelse(literature_diets[k,i] == 0, 1, (dzeta[i]))
        eta_hyperparam_2[k, i] <- dzeta[i] + 1 - eta_hyperparam_1[k, i]
      }
    }

    check_numeric_parameter(literature_slope, "literature_slope")
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
