## ---- eval = FALSE------------------------------------------------------------
#  load("convergence_diagnostic.Rdata")
#  View(gelman)

## -----------------------------------------------------------------------------
library(EcoDiet)
example_stomach_data_path <- system.file("extdata", "example_stomach_data.csv",
                                    package = "EcoDiet")
example_isotope_data_path <- system.file("extdata", "example_isotope_data.csv",
                                    package = "EcoDiet")

data <- preprocess_data(isotope_data = read.csv(example_isotope_data_path),
                        trophic_enrichment_factor = c(0.8, 3.4),
                        literature_prior = FALSE,
                        stomach_data = read.csv(example_stomach_data_path))

model_string <- write_model(literature_prior = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  mcmc_output <- run_model(textConnection(model_string), data, nb_iter = 1e+08)

## ---- eval = FALSE------------------------------------------------------------
#  mcmc_output <- run_model(textConnection(model_string), data, nb_iter = 1e+06, nb_adapt = 1e+04)

## -----------------------------------------------------------------------------
devtools::session_info()

