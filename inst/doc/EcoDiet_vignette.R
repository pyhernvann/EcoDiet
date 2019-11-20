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
#  devtools::install_github("heloisethero/EcoDiet", auth_token='c570f1b55d68a8ae0fb38428155e19a426814d15', build_vignettes = TRUE)

## ------------------------------------------------------------------------
library(EcoDiet)


## ------------------------------------------------------------------------
preprocessed_data <- load_data(ecodiet_example)


## ------------------------------------------------------------------------
preprocessed_data$o
preprocessed_data$n_sca


## ------------------------------------------------------------------------
preprocessed_data$o <- rescale_stomach_data(preprocessed_data$o, preprocessed_data$n_sca)
preprocessed_data$o


