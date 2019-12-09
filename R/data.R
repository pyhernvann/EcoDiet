NULL

#' Toy example to test the EcoDiet package
#'
#' This is a simulated toy dataset, made to illustrate the EcoDiet package functions.
#'
#'@format A list of 10 elements:
#'\describe{
#'   \item{isotope_data}{a data frame with 3 variables: \code{Group}, 
#'   \code{d13C} and \code{d15N}.}
#'
#'   \item{stomach_data}{A data frame with 10 variables, 
#'    each corresponding to the name of a trophic group:
#'    \code{Cod}, \code{Pout}, \code{Sardine}, \code{Shrimps},
#'    \code{Crabs}, \code{Bivalves}, \code{Worms}, \code{Zooplankton},
#'    \code{Phytoplankton} and \code{Detritus}.
#'    The number are the number of stomachs in which a specific prey 
#'    (the group on the row) was found inside a predator (the group on the column).}
#'    
#'   \item{number_full_stomachs}{A data frame with 10 variables, 
#'    each corresponding to the name of a trophic group, 
#'    containing the number of full stomachs analyzed for each predator.}
#'    
#'   \item{element_concentration}{The matrix containing the concentration of each element j 
#'   in the prey k. It is supposed to be known in advance.}
#'   
#'}
#'
#'@examples data("ecodiet_example")

"ecodiet_example"
