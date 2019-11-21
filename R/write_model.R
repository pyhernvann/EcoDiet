#' Write the EcoDiet model in bugs langage on a file
#'
#' @param file_name the name of the bugs files on which the model will be written
#' @return a string containing the model definition written in bugs syntaw
#' 
#' @examples
#' write_model()
#'
#' @export

write_model <- function(){

  model_def <- "
  Here I will put the model definition.
  This is a new line!"

  return(model_def)
}