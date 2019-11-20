#' Rescale the stomachal data
#' 
#' When the occurrences of preys in the stomach of predators are too small, 
#' it is often useful to rescale them. This function artificially increases the number of 
#' ocurrences in stomach for which a prey is found.
#'
#' @param stomach_df the dataframe containing the stomachal data
#' @param isotope_data_path the dataframe containing the number of full stomachs for each predator
#' 
#' @return the rescaled stomach dataframe
#' 
#' @examples 
#' rescaled_stomach <- rescale_stomach_data(ecodiet_example$SCA_data, ecodiet_example$nb_stom_SCA_data)
#'
#' @export

rescale_stomach_data <- function(SCA_data, nb_stom_SCA_data){
  
  maxocc_over_nsca <- apply(SCA_data, FUN = max, 2, na.rm = TRUE)
  for (j in 1:ncol(SCA_data)) {
    if (maxocc_over_nsca[j] != 0) {
      scaled <- round(SCA_data[, j] * as.numeric(as.character((nb_stom_SCA_data[1, j]/maxocc_over_nsca[j]))))
    }
    for (i in 1:nrow(SCA_data)) {
      SCA_data[i, j] <- min(scaled[i], as.numeric(as.character(nb_stom_SCA_data[1, j])))
    }
  }
  
  return(SCA_data)
}