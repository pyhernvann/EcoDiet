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
    
    # re-arrange the stomachal data before plotting it
    if (colnames(stomach_data)[1] == "X"){
      stomach_data[-1] <- data.frame(lapply(stomach_data[-1], function(X) X/X[nrow(stomach_data)]))
      row.names(stomach_data) <- stomach_data[, 1]
      stomach_data[, 1] <- NULL
    } else {
      stomach_data <- data.frame(lapply(stomach_data, function(X) X/X[nrow(stomach_data)]))
    }
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
#' @param variable_to_extract the variable to extract and compute the means (by default PIs are extracted)
#' 
#' @keywords internal
#' @noRd

extract_mean <- function(mcmc_output, variable_to_extract = "PI"){
  
  # keep only the means for the relevant variable
  raw_means <- summary(mcmc_output)$statistics[, "Mean"]
  raw_means <- raw_means[startsWith(names(raw_means), variable_to_extract)]
  
  # prepare an empty matrix with the correct format
  matrix_mean <- matrix(0, ncol(data$o), ncol(data$o))
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


#' Plot the results of the EcoDiet model.
#'
#' @param mcmc_output the mcmc.list object outputed by the run_model() function
#'
#' @export

plot_results <- function(mcmc_output){
  
  PI_mean <- extract_mean(mcmc_output, variable_to_extract = "PI")
  plot_matrix(PI_mean, title = "Posterior diet propotions")
  save(PI_mean, file ="PI_mean.Rdata")
  
  eta_mean <- extract_mean(mcmc_output, variable_to_extract = "eta")
  plot_matrix(eta_mean, title = "Posterior trophic links probabilities")
  save(eta_mean, file ="eta_mean.Rdata")
  
}