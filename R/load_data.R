#' Load the stomachal and the isotopic data to feed the EcoDiet model.
#'
#' @param stomach_data_path the path to access the stomachal data
#' @param isotope_data_path the path to access the isotopic data
#' @return a list containing the stomachal and isotopic data
#' 
#' @examples
#' data <- load_data("data/example_stomach_data.csv", "data/example_isotope_data.csv")
#' 
#' @export
#' 
#' library(gsubfn)

load_data <- function(stomach_data_path, isotope_data_path){
  
  stomach_data <- utils::read.csv(stomach_data_path)
  isotope_data <- utils::read.csv(isotope_data_path)
  
  return(list(stomach_data = stomach_data, isotope_data = isotope_data))
}