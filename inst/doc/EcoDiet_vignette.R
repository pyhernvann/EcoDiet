## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("heloisethero/EcoDiet", build_vignettes = TRUE)

## ------------------------------------------------------------------------
library(EcoDiet)


## ------------------------------------------------------------------------
example_stomach_path <- system.file("extdata", "ecodiet_example_stomach.csv",
                                    package = "EcoDiet")
example_isotope_path <- system.file("extdata", "ecodiet_example_isotope.csv",
                                    package = "EcoDiet")
data <- load_data(example_stomach_path, example_isotope_path)

## ---- eval = FALSE-------------------------------------------------------
#  data <- load_data("my_EcoDiet_analysis/my_stomach_daya.csv",
#                    "my_EcoDiet_analysis/my_isotope_daya.csv")

## ------------------------------------------------------------------------
data <- list(stomach_data = ecodiet_example_stomach, isotope_data = ecodiet_example_isotope)


## ------------------------------------------------------------------------
print(ecodiet_example_stomach)


## ------------------------------------------------------------------------
head(ecodiet_example_isotope)


