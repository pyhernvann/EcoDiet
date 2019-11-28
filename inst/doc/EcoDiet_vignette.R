## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(EcoDiet)


## ------------------------------------------------------------------------
names(ecodiet_example)


## ------------------------------------------------------------------------
preprocessed_data <- load_data(ecodiet_example)


## ------------------------------------------------------------------------
model_string <- write_model()


## ---- eval = FALSE-------------------------------------------------------
#  cat(model_string, file = "EcoDiet_model.bug")
#  

## ------------------------------------------------------------------------
inits <- initialize_model(preprocessed_data, ecodiet_example$topo_run)


## ------------------------------------------------------------------------
mcmc_output <- run_model(preprocessed_data, inits, textConnection(model_string))


## ---- eval = FALSE-------------------------------------------------------
#  mcmc_output <- run_model(preprocessed_data, inits, "EcoDiet_model.bug")
#  

## ------------------------------------------------------------------------
devtools::session_info()


