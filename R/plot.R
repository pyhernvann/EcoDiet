#' Plot the isotopic data with a biplot
#' 
#' @param isotope_data the raw isotopic data
#' 
#' @import ggplot2
#' 
#' @keywords internal
#' @noRd

plot_isotope_data <- function(isotope_data){
  
  figure <- ggplot(isotope_data,
                   aes(x = isotope_data[, 2],
                       y = isotope_data[, 3],
                       colour = group)) +
    ggtitle("Isotopic measurements") + 
    xlab(names(isotope_data)[2]) + 
    ylab(names(isotope_data)[3]) +
    geom_point(size = 3) +
    scale_colour_brewer(palette = "Paired") +
    guides(colour = guide_legend(byrow = 1, ncol = 1)) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "grey"),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 15),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(margin = margin(3, 0, 0, 0), size = 12),
          plot.title = element_text(hjust = 0.5))
  
  plot(figure)

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
  
  df <- data.frame(rep(colnames(matrix), each=nrow(matrix)),
                   rep(rownames(matrix), nrow(matrix)),
                   unlist(matrix))
  colnames(df) <- c("pred","prey","value")
  df$pred <- as.numeric(df$pred)
  df$prey <- rev(as.numeric(df$prey))
  
  figure <- ggplot(df, aes(x = pred, y = prey, fill = value)) + geom_raster() + theme_bw() +
    scale_x_continuous(labels = colnames(matrix), breaks = seq(1, ncol(matrix))) +
    scale_y_continuous(labels = rev(rownames(matrix)), breaks = seq(1, nrow(matrix))) +
    geom_text(aes(label = round(value, 2))) +
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
  
  plot(figure)
  
}


#' Plot the raw data entered by the user.
#'
#' @param isotope_data the raw isotopic data
#' @param stomach_data the raw stomachal data if the user has it. 
#' By default this variable is set on NULL and is not ploted.
#' @param literature_diets the diet priors from the literature if the user has it.
#' By default this variable is set on NULL and is not ploted.
#'
#' @export

plot_data <- function(isotope_data, stomach_data = NULL, literature_diets = NULL){
  
  plot_isotope_data(isotope_data)
  
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
  
  if (!is.null(literature_diets)){
    
    # re-arrange the literature diets matrix before plotting it
    if (colnames(literature_diets)[1] == "X"){
      row.names(literature_diets) <- literature_diets[, 1]
      literature_diets[, 1] <- NULL
    }

    literature_diets <- literature_diets[, order(colnames(literature_diets))]
    literature_diets <- literature_diets[order(rownames(literature_diets)), ]
    
    plot_matrix(literature_diets, title = "Diet proportions from the literature")
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
  
  for (i in 1:length(raw_means)){
    # extract the indices that are between the brackets: "PI[2, 4]" -> "2,4"
    correct_indices <- regmatches(names(raw_means)[i], regexec("\\[(.*?)\\]", names(raw_means)[i]))[[1]][2]
    # re-format the indices: "2,4" -> c(2L, 4L)
    correct_indices <- as.integer(strsplit(correct_indices, ",")[[1]])
    # use the indices to fill the matrix with the correct format
    matrix_mean[correct_indices[1], correct_indices[2]] <- raw_means[i]
  }
  
  matrix_mean <- as.data.frame(matrix_mean)
  
  return(matrix_mean)
  
}

#' Plot the main results of the EcoDiet model (the means of each variable)
#'
#' @param mcmc_output the mcmc.list object outputed by the run_model() function
#' @param data the preprocessed data outputed by the preprocess_data() function
#'
#' @export

plot_results <- function(mcmc_output, data){
  
  PI_mean <- extract_mean(mcmc_output, data, variable_to_extract = "PI")
  plot_matrix(PI_mean, title = "Posterior diet propotions")
  save(PI_mean, file ="PI_mean.Rdata")
  
  eta_mean <- extract_mean(mcmc_output, data, variable_to_extract = "eta")
  plot_matrix(eta_mean, title = "Posterior trophic links probabilities")
  save(eta_mean, file ="eta_mean.Rdata")
  
}

#' Plot the probability densities for a given variable
#' 
#' @param mcmc_output the mcmc.list object outputed by the run_model() function
#' @param data the preprocessed data outputed by the preprocess_data() function
#' @param variable_to_extract the name of the variable we want to plot ("PI" by default)
#' @param variable_to_extract the title to put (depends on the variable to plot)
#'  
#' @import ggplot2
#' 
#' @keywords internal
#' @noRd

plot_probability_densities <- function(mcmc_output, data, variable_to_extract = "PI", 
                                       title = "Posterior diet proportions"){
  
  # Keep only the variable of interest
  mcmc_output <- mcmc_output[, startsWith(colnames(mcmc_output), variable_to_extract)]
  
  # Create a lookup table between the model output's names and the prey's and predator's indices:
  #         names prey pred
  #     1 PI[2,1]    2    1
  #     2 PI[3,1]    3    1
  #     3 PI[3,2]    3    2
  lookup <- sapply(colnames(mcmc_output), function(X) regmatches(X, regexec("\\[(.*?)\\]", X))[[1]][2])
  lookup_table <- data.frame(names = colnames(mcmc_output), 
                             prey = as.integer(substr(lookup, 1, 1)), 
                             pred = as.integer(substr(lookup, 3, 3)),
                             stringsAsFactors = FALSE)
  
  # Create one plot for each predator
  for (pred_index in which(data$nb_prey > ifelse(variable_to_extract == "PI", 1, 0))){
    
    # Prepare a data frame with the values for the diet proportions for one predator's preys
    values_to_extract <- mcmc_output[, lookup_table[lookup_table$pred == pred_index, ]$names]
    prey_names <- colnames(data$o)[lookup_table[lookup_table$pred == pred_index, ]$prey]
    
    df_to_plot <- data.frame(Prey = rep(prey_names, each = dim(mcmc_output)[1]),
                             variable_to_plot = c(values_to_extract))
    
    # Plot these values to represent the approximated probability densities for the diet proportions
    figure <- ggplot(df_to_plot) +
      geom_density(aes(x = variable_to_plot, y=..scaled.., fill = Prey), alpha = .3) +
      geom_density(aes(x = variable_to_plot, y=..scaled.., color = Prey), size = 1.25) +
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
}



#' Plot the main results of the EcoDiet model (the means of each variable)
#'
#' @param mcmc_output the mcmc.list object outputed by the run_model() function
#' @param data the preprocessed data outputed by the preprocess_data() function
#'
#' @export

plot_full_results <- function(mcmc_output, data){
  
  plot_probability_densities(mcmc_output, data, variable_to_extract = "PI", 
                             title = "Posterior diet proportions")
  
  plot_probability_densities(mcmc_output, data, variable_to_extract = "eta",
                             title = "Posterior trophic links probabilities")

}