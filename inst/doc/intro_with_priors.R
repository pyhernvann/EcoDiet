## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(EcoDiet)

## -----------------------------------------------------------------------------
example_isotope_data_path <- system.file("extdata", "example_isotope_data.csv",
                                    package = "EcoDiet")
example_isotope_data <- read.csv(example_isotope_data_path)
knitr::kable(example_isotope_data)

## -----------------------------------------------------------------------------
example_stomach_data_path <- system.file("extdata", "example_stomach_data.csv",
                                    package = "EcoDiet")
example_stomach_data <- read.csv(example_stomach_data_path)
knitr::kable(example_stomach_data)

## -----------------------------------------------------------------------------
example_literature_diets_path <- system.file("extdata", "example_literature_diets.csv",
                                    package = "EcoDiet")
example_literature_diets <- read.csv(example_literature_diets_path)
knitr::kable(example_literature_diets)

## ---- fig1, fig.height = 4, fig.width = 6, fig.align = "center"---------------
plot_data(isotope_data = example_isotope_data,
          stomach_data = example_stomach_data,
          literature_diets = example_literature_diets)

## ---- eval = FALSE------------------------------------------------------------
#  example_stomach_data     <- read.csv("./data/my_stomach_data.csv",     sep = ";")
#  example_isotope_data     <- read.csv("./data/my_isotope_data.csv",     sep = ";")
#  example_literature_diets <- read.csv("./data/my_literature_diets.csv", sep = ";")

## ---- eval = FALSE------------------------------------------------------------
#  trophic_enrichment_factor = c(0.8, 3.4)

## ---- eval = FALSE------------------------------------------------------------
#  literature_prior = TRUE

## -----------------------------------------------------------------------------
data <- preprocess_data(isotope_data = example_isotope_data,
                        trophic_enrichment_factor = c(0.8, 3.4),
                        literature_prior = TRUE,
                        stomach_data = example_stomach_data,
                        literature_diets = example_literature_diets)

## -----------------------------------------------------------------------------
example_literature_diets[3, 2] <- 0.02
example_literature_diets[1, 2] <- 0.49
example_literature_diets[4, 2] <- 0.49
knitr::kable(example_literature_diets)

## -----------------------------------------------------------------------------
data <- preprocess_data(isotope_data = example_isotope_data,
                        trophic_enrichment_factor = c(0.8, 3.4),
                        literature_prior = TRUE,
                        stomach_data = example_stomach_data,
                        literature_diets = example_literature_diets)

## -----------------------------------------------------------------------------
example_literature_pedigrees_path <- system.file("extdata", "example_literature_pedigrees.csv",
                                    package = "EcoDiet")
example_literature_pedigrees <- read.csv(example_literature_pedigrees_path)
knitr::kable(example_literature_pedigrees)

## -----------------------------------------------------------------------------
nb_literature = 12

## -----------------------------------------------------------------------------
data <- preprocess_data(isotope_data = example_isotope_data,
                        trophic_enrichment_factor = c(0.8, 3.4),
                        literature_prior = TRUE,
                        stomach_data = example_stomach_data,
                        literature_diets = example_literature_diets,
                        literature_pedigrees = example_literature_pedigrees,
                        nb_literature = 12,
                        literature_slope = 0.6)

## -----------------------------------------------------------------------------
model_string <- write_model(literature_prior = TRUE)

## -----------------------------------------------------------------------------
mcmc_output <- run_model(textConnection(model_string), data)

## ---- eval = FALSE------------------------------------------------------------
#  mcmc_output <- run_model(textConnection(model_string), data, nb_iter = 1e+06)

## ---- fig2, fig.height = 4, fig.width = 6, fig.align = "center"---------------
plot_results(mcmc_output, data)

## ---- eval = FALSE------------------------------------------------------------
#  load("PI_mean.Rdata")
#  write.table(PI_mean,  file = "PI_mean.csv",  sep = ",", col.names = NA)
#  
#  load("eta_mean.Rdata")
#  write.table(eta_mean, file = "eta_mean.csv", sep = ",", col.names = NA)

## ---- fig3, fig.height = 4, fig.width = 6, fig.align = "center"---------------
plot_full_results(mcmc_output, data)

## -----------------------------------------------------------------------------
load("mcmc_output.Rdata")
knitr::kable(head(mcmc_output))

## -----------------------------------------------------------------------------
quantiles <- apply(mcmc_output, 2, function(X) quantile(X, probs = c(0.05, 0.5, 0.95)))
quantiles <- signif(quantiles, digits = 2)
knitr::kable(quantiles)

## ---- eval = FALSE------------------------------------------------------------
#  write.table(quantiles, file = "quantiles.csv", sep = ",", col.names = NA)

## -----------------------------------------------------------------------------
devtools::session_info()

