#' Example stomach data
#'
#' This is an artificial and simple stomachal dataset, made to illustrate the package on a simple case.
#' All tables whose name start by "example" are describing different data from the same trophic groups.
#'
#'@format A table with 5 rows and 5 columns. 
#' The headers contain the predators' names, the first column contains the preys' names.
#' Each cell contains the number of the predator's stomachs in which this prey was found.
#' The last row contains the total number of non-empty stomachs for each predator.
#' 
#' @name example_stomach_data
NULL


#' Example isotope data
#'
#' This is an artificial and simple biotracer dataset, more specifically stable isotope analyses,
#'  made to illustrate the package on a simple case. All tables whose name start by "example" are describing 
#'  different data from the same trophic groups.
#'
#'@format A table with 15 rows and 3 columns. Each row is an isotopic sample from one individual.
#'The columns are:
#'\describe{
#'   \item{group}{the trophic group the individual belonged to}
#'   \item{d13C}{the d13C measurement made on that individual}
#'   \item{d15N}{the d15N measurement made on that individual}
#'}
#'
#'@name example_isotope_data
NULL


#' Example literature diets
#'
#' This is an artificial and simple literature diets dataset, made to illustrate the package on a simple case.
#' All tables whose name start by "example" are describing different data from the same trophic groups.
#'
#'@format A table with 5 rows and 5 columns. 
#' The headers contain the predators' names, the first column contains the preys' names.
#' Each cell contains the average diet proportions fond in the literature for the corresponding predator.
#' The last row contains the average pedigree score for the literature on each predator.
#'
#'@name example_literature_diets
NULL


#' Realistic stomach data
#'
#' This is an artificial and realistic stomachal dataset, made to illustrate the package on a complex dataset.
#' All tables whose name start by "realistic" are describing different data from the same trophic groups.
#'
#'@format A table with 11 rows and 11 columns. 
#' The headers contain the predators' names, the first column contains the preys' names.
#' Each cell contains the number of the predator's stomachs in which this prey was found.
#' The last row contains the total number of non-empty stomachs for each predator.
#' 
#' @name realistic_stomach_data
NULL


#' Realistic isotope data
#'
#' This is an artificial and realistic biotracer dataset, more specifically stable isotope analyses,
#'  made to illustrate the package on a complex dataset. All tables whose name start by "realistic" are 
#'  describing different data from the same trophic groups.
#'
#'@format A table with 300 rows and 3 columns. Each row is an isotopic sample from one individual, and 
#' there are 30 individuals sampled in each trophic group.
#'The columns are:
#'\describe{
#'   \item{group}{the trophic group the individual belonged to}
#'   \item{d13C}{the d13C measurement made on that individual}
#'   \item{d15N}{the d15N measurement made on that individual}
#'}
#'
#'@name realistic_isotope_data
NULL


#' Realistic literature diets
#'
#' This is an artificial and realistic literature diets dataset, made to illustrate the package on a complex dataset.
#' All tables whose name start by "realistic" are describing different data from the same trophic groups.
#'
#'@format A table with 11 rows and 11 columns. 
#' The headers contain the predators' names, the first column contains the preys' names.
#' Each cell contains the average diet proportions fond in the literature for the corresponding predator.
#' The last row contains the average pedigree score for the literature on each predator.
#'
#'@name realistic_literature_diets
NULL
