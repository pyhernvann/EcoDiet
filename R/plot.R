#' Plot the biotracer data with one biplot for each combination of 2 biotracers
#'
#' @param biotracer_data the input biotracer data
#'
#' @import ggplot2
#'
#' @keywords internal
#' @noRd

plot_biotracer_data <- function(biotracer_data){

  # If the biotracer data contains 3 elements called d13C, d15N and d125I, then we will plot 3 figures,
  # because there are 3 ways to choose an unordered subset of 2 elements from a fixed set of 3 elements:
  #          d13C vs. d15N,
  #          d13C vs. d125I and
  #          d15N vs. d125I.
  #
  # With the following code, we select all the possible combinations without repetition:

  nb_element <- ncol(biotracer_data) - 1

  for (element1 in 1:nb_element){
    for (element2 in 1:nb_element){

      if (element2 > element1){
        figure <- ggplot(biotracer_data,
                         aes(x = biotracer_data[, element1 + 1],
                             y = biotracer_data[, element2 + 1],
                             colour = group)) +
          ggtitle("Isotopic measurements") +
          xlab(names(biotracer_data)[element1 + 1]) +
          ylab(names(biotracer_data)[element2 + 1]) +
          geom_point(size = 3, na.rm = TRUE) +
          guides(colour = guide_legend()) +
          theme_bw() +
          theme(panel.grid.major = element_line(colour = "grey"),
                panel.grid.minor = element_blank(),
                axis.title = element_text(size = 15),
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(margin = margin(3, 0, 0, 0), size = 12),
                plot.title = element_text(hjust = 0.5))

        plot(figure)
      }
    }
  }

}

#' Plot any matrix data with a raster plot
#'
#' @param matrix the matrix ready to be plotted
#'
#' @import ggplot2
#'
#' @keywords internal
#' @noRd

plot_matrix <- function(matrix, title){

  matrix <- as.data.frame(matrix)

  df <- data.frame(rep(colnames(matrix), each = nrow(matrix)),
                   rep(rownames(matrix), nrow(matrix)),
                   unlist(matrix))
  colnames(df) <- c("pred", "prey", "value")
  df$pred <- as.numeric(df$pred)
  df$prey <- rev(as.numeric(df$prey))

  figure <- ggplot(df, aes(x = pred, y = prey, fill = value)) + geom_raster() + theme_bw() +
    scale_x_continuous(labels = colnames(matrix), breaks = seq(1, ncol(matrix))) +
    scale_y_continuous(labels = rev(rownames(matrix)), breaks = seq(1, nrow(matrix))) +
    scale_fill_gradient(low = "white", high = "blue3", limit = c(0, 1)) +
    ggtitle(title) +
    ylab("Preys") +
    xlab("Predators") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 15),
          axis.text.x = element_text(size = 12, angle = 20, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))

  if (ncol(matrix) < 15){
    figure <- figure + geom_text(data = df[df$value > 0, ], aes(label = round(value, 2)))
  }

  plot(figure)

}


#' Plot the raw data entered by the user.
#'
#' @param biotracer_data the input biotracer data
#' @param stomach_data the raw stomachal data if the user has it.
#' By default this variable is set on NULL and is not ploted.
#'
#' @export

plot_data <- function(biotracer_data = NULL, stomach_data = NULL){

  if (!is.null(stomach_data)){
    # Clean the stomach data similarly as in the preprocess_data function except for the commented parts
    if (colnames(stomach_data)[1] == "X"){
      row.names(stomach_data) <- stomach_data[, 1]
      stomach_data[, 1] <- NULL
    }

    # Divide the number of stomachs by the total number of full stomachs to obtain proportions
    stomach_data[] <- lapply(stomach_data, function(X) X/X[nrow(stomach_data)])
    # Remove the NA caused by division by zero for the trophic groups at the base of the ecosystem
    stomach_data[is.na(stomach_data)] <- 0

    stomach_data <- stomach_data[-nrow(stomach_data), ]
    stomach_data <- stomach_data[, order(colnames(stomach_data))]
    stomach_data <- stomach_data[order(rownames(stomach_data)), ]

    plot_matrix(stomach_data, title = "Proportion of occurences in stomachs")
  }

  if (!is.null(biotracer_data)){
    plot_biotracer_data(biotracer_data)
  }

}


#' Plot the prior probability density(ies) for a given variable and predator
#'
#' @param data the preprocessed data outputed by the preprocess_data() function
#' @param literature_configuration a boolean (TRUE or FALSE) indicating whether the model will have
#' prior distributions informed by a literature study
#' @param pred the predator name for which we want to plot the probability densities
#' @param prey the prey(s) name for which we want to plot the probability densities
#' @param var the variable for which we want to plot the probability densities
#' @param title the title to put (depends on the variable and the predator to plot)
#'
#' @import ggplot2
#'
#' @keywords internal
#' @noRd

plot_prior_distribution <- function(data, literature_configuration, pred, prey, var, title){

  pred_index <- which(colnames(data$o) == pred)
  if (length(pred_index) == 0){
    stop("You did not put a correct predator name in the `pred` argument.\n",
         "  You entered the name \"", pred,"\", while the predator names are actually: \"",
         paste(colnames(data$o), collapse = "\", \""), "\".\n",
         "  Please use one of the above names in the `pred` argument.")
  }
  if (data$nb_prey[pred_index] == 0){
    stop("The predator you have chosen (\"", pred, "\") has no prey and thus cannot be plotted.")
  }

  if (is.null(prey)){
    prey_index <- data$list_prey[pred_index, ]
    prey_index <- prey_index[!is.na(prey_index)]
    prey <- colnames(data$o)[prey_index]
  } else {
    prey_index <- which(colnames(data$o) %in% prey)
    if (length(prey) != length(prey_index)){
      stop("You used an incorrect prey name in the `prey` argument.\n",
           "  You have entered the names: \"", paste(prey, collapse = "\", \""),
           "\".\n  But the prey names are actually: \"",
           paste(colnames(data$o), collapse = "\", \""), "\".\n",
           "  Please put correct names in the `prey` argument.")
    }
    if (sum(prey_index %in% data$list_prey[pred_index, ]) == 0){
      stop("None of the input preys you have entered (\"", paste(prey, collapse = "\", \""),
           "\") are considered as eaten by the input predator (\"", pred ,"\").")
    }
  }

  x <-  seq(0, 1, length = 101)
  df_to_plot <- data.frame(Prey = c(), x = c(), Density = c())

  for (each_prey in prey){
    prey_idx <- which(colnames(data$o) == each_prey)
    if (prey_idx %in% data$list_prey[pred_index, ]){
      if (var == "PI"){
        if (literature_configuration) {
          Density <- dbeta(x, data$alpha_lit[prey_idx, pred_index],
                           colSums(data$alpha_lit)[pred_index] - data$alpha_lit[prey_idx, pred_index])
        } else {
          Density <- dbeta(x, 1, data$nb_prey[pred_index] - 1)
        }
      } else if (var == "eta"){
        if (literature_configuration) {
          Density <- dbeta(x, data$eta_hyperparam_1[prey_idx, pred_index],
                           data$eta_hyperparam_2[prey_idx, pred_index])
        } else {
          Density <- dbeta(x, 1, 1)
        }
      }
      df_to_plot <- rbind(df_to_plot, data.frame(Prey = rep(each_prey, 101), x = x, Density = Density))
    }
  }

  figure <- ggplot(df_to_plot, aes(x = x, y = Density, colour = Prey, linetype = Prey)) +
    geom_line(size = 1.25) +
    ggtitle(paste(title, "\nfor the", pred, "predator")) +
    xlim(0, 1) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5))
  plot(figure)

}

#' Plot the prior means or distributions for the eta and/or PI parameters
#'
#' @param data the preprocessed data list outputed by the preprocess_data() function
#' @param literature_configuration a boolean (TRUE or FALSE) indicating whether the model will have
#' prior distributions informed by a literature study
#' @param pred the predator name for which we want to plot the probability densities
#' @param prey the prey(s) name for which we want to plot the probability densities
#' @param variable the variable(s) for which we want to plot the probability densities
#'
#' @export

plot_prior <- function(data, literature_configuration, pred = NULL, prey = NULL, variable = c("eta", "PI")){

  if (!all(variable %in% c("eta", "PI"))){
    stop("This function can only print a figure for the PI or eta variable.\n",
         "  But you have entered this variable name: \"", variable, "\".\n",
         "  Please use rather `variable = \"PI\"` or `variable = \"eta\"` for this function.")
  }

  for (var in variable){

    title <- switch(var,
                    PI = "Prior diet proportions",
                    eta = "Prior trophic links probabilities")

    if (is.null(pred) & is.null(prey)){

      mean_prior <- matrix(0, ncol = data$nb_group, nrow = data$nb_group)
      colnames(mean_prior) <- rownames(mean_prior) <- colnames(data$o)

      for (i in data$list_pred){
        for (k in data$list_prey[i, 1:data$nb_prey[i]]){
          if (var == "eta"){
            if (literature_configuration){
              mean_prior[k, i] <- (data$eta_hyperparam_1[k, i]/
                                     (data$eta_hyperparam_1[k, i] + data$eta_hyperparam_2[k, i]))
            } else {
                mean_prior[k, i] <- 1/2
            }
          } else if (var == "PI"){
            if (literature_configuration){
              mean_prior[k, i] <- data$alpha_lit[k, i]/colSums(data$alpha_lit)[i]
            } else {
              mean_prior[k, i] <- 1/data$nb_prey[i]
            }
          }
        }
      }

      plot_matrix(mean_prior, title = paste(title, "(means)"))

    } else {

      plot_prior_distribution(data, literature_configuration, pred, prey, var, title)

    }
  }

}


#' Extract the means of the posterior distribution for a specific variable (PI or eta)
#' in a matrix format (with the predators in the columns, and the preys in the rows)
#'
#' @param mcmc_output the mcmc.list object outputed by the run_model() function
#' @param data the preprocessed data outputed by the preprocess_data() function
#' @param variable_to_extract the name of the variable we want to compute the means ("PI" by default)
#'
#' @keywords internal
#' @noRd

extract_mean <- function(mcmc_output, data, variable_to_extract = "PI"){

  # keep only the means for the relevant variable
  raw_means <- colMeans(mcmc_output)
  raw_means <- raw_means[startsWith(names(raw_means), variable_to_extract)]

  # prepare an empty matrix with the correct format
  matrix_mean <- matrix(0, data$nb_group, data$nb_group)
  colnames(matrix_mean) <- rownames(matrix_mean) <- colnames(data$o)

  for (i in seq_along(raw_means)){
    # extract the indices that are between the brackets: "PI[2, 4]" -> "2,4"
    correct_indices <- regmatches(names(raw_means)[i], regexec("\\[(.*?)\\]", names(raw_means)[i]))[[1]][2]
    # re-format the indices: "2,4" -> c(2L, 4L)
    correct_indices <- as.integer(strsplit(correct_indices, ",")[[1]])
    # use the indices to fill the matrix with the correct format
    matrix_mean[correct_indices[1], correct_indices[2]] <- raw_means[i]
  }

  return(matrix_mean)

}

#' Plot the posterior probability density(ies) for a given variable and predator
#'
#' @param mcmc_output the mcmc.list object outputed by the run_model() function
#' @param data the preprocessed data outputed by the preprocess_data() function
#' @param variable_to_extract the name of the variable we want to plot
#' @param title the title to put (depends on the variable and the predator to plot)
#'
#' @import ggplot2
#'
#' @keywords internal
#' @noRd

plot_posterior_distribution <- function(mcmc_output, data, pred, prey,
                                        variable_to_extract, title){

  pred_index <- which(colnames(data$o) == pred)
  if (length(pred_index) == 0){
    stop("You did not put a correct predator name in the `pred` argument.\n",
         "  You entered the name \"", pred,"\", while the predator names are actually: \"",
         paste(colnames(data$o), collapse = "\", \""), "\".\n",
         "  Please use one of the above names in the `pred` argument.")
  }
  if (data$nb_prey[pred_index] == 0){
    stop("The predator you have chosen (\"", pred, "\") has no prey and thus cannot be plotted.")
  }

  if (!is.null(prey)){
    prey_index <- which(colnames(data$o) %in% prey)
    if (length(prey) != length(prey_index)){
      stop("You used an incorrect prey name in the `prey` argument.\n",
           "  You have entered the names: \"", paste(prey, collapse = "\", \""),
           "\".\n  But the prey names are actually: \"",
           paste(colnames(data$o), collapse = "\", \""), "\".\n",
           "  Please put correct names in the `prey` argument.")
    }
    if (sum(prey_index %in% data$list_prey[pred_index, ]) == 0){
      stop("None of the input preys you have entered (\"", paste(prey, collapse = "\", \""),
           "\") are considered as eaten by the input predator (\"", pred ,"\").")
    }
  }

  # Keep only the variable of interest (all the "PI" or all the "eta")
  mcmc_output <- mcmc_output[, startsWith(colnames(mcmc_output), variable_to_extract)]

  # Create a lookup table between the model output's names and the prey's and predator's indices:
  #         names prey pred
  #     1 PI[2,1]    2    1
  #     2 PI[3,1]    3    1
  #     3 PI[3,2]    3    2
  lookup <- sapply(colnames(mcmc_output), function(X) regmatches(X, regexec("\\[(.*?)\\]", X))[[1]][2])
  prey_idx <- sapply(lookup, function(X) strsplit(X, split=',')[[1]][[1]])
  pred_idx <- sapply(lookup, function(X) strsplit(X, split=',')[[1]][[2]])

  lookup_table <- data.frame(names = colnames(mcmc_output),
                             prey = as.integer(prey_idx),
                             pred = as.integer(pred_idx),
                             stringsAsFactors = FALSE)

  # Prepare a data frame with the values for one predator's preys
  if (is.null(prey)){
    variables_to_extract <- lookup_table[lookup_table$pred == pred_index, ]$names
    prey <- colnames(data$o)[lookup_table[lookup_table$pred == pred_index, ]$prey]
  } else {
    variables_to_extract <- lookup_table[(lookup_table$pred == pred_index &
                                            lookup_table$prey %in% prey_index &
                                            lookup_table$prey %in% data$list_prey[pred_index, ]), ]$names
  }
  values_to_extract <- mcmc_output[, variables_to_extract]

  df_to_plot <- data.frame(Prey = rep(prey, each = dim(mcmc_output)[1]),
                           variable_to_plot = c(values_to_extract))

  # Plot these values to represent the approximated probability densities
  figure <- ggplot(df_to_plot) +
    geom_density(aes(x = variable_to_plot, y=..scaled.., fill = Prey),
                 alpha = .3, adjust = 1/5, na.rm = TRUE) +
    geom_density(aes(x = variable_to_plot, y=..scaled.., color = Prey),
                 size = 1.25, adjust = 1/5, na.rm = TRUE) +
    ggtitle(paste(title, "\nfor the", colnames(data$o)[pred_index], "predator")) +
    ylab("Density") +
    scale_shape_manual(values = c(seq(1:10))) +
    guides(colour = guide_legend(byrow = 1, ncol = 1), shape = guide_legend(byrow = 1, ncol = 1)) +
    xlim(0, 1) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot(figure)
}


#' Plot the main results of the EcoDiet model (the means of each variable)
#'
#' @param mcmc_output the mcmc.list object outputed by the run_model() function
#' @param data the preprocessed data outputed by the preprocess_data() function
#' @param pred the predator name for which we want to plot the probability densities
#' @param prey the prey(s) name for which we want to plot the probability densities
#' @param variable the variable(s) for which we want to plot the probability densities
#'
#' @export

plot_results <- function(mcmc_output, data, pred = NULL, prey = NULL, variable = c("eta", "PI")){

  if (!all(variable %in% c("eta", "PI"))){
    stop("This function can only print a figure for the PI or eta variable.\n",
         "  But you have entered this variable name: \"", variable, "\".\n",
         "  Please use rather `variable = \"PI\"` or `variable = \"eta\"` for this function.")
  }

  for (var in variable){

    title <- switch(var,
                    PI = "Posterior diet proportions",
                    eta = "Posterior trophic links probabilities")

    if (is.null(pred) & is.null(prey)){

      mean <- extract_mean(mcmc_output, data, variable_to_extract = var)
      plot_matrix(mean, title = paste(title, "(means)"))
      save(mean, file = paste0(var, "_mean.Rdata"))

    } else {
      plot_posterior_distribution(mcmc_output, data, pred, prey,
                                  variable_to_extract = var, title = title)
    }
  }

}
