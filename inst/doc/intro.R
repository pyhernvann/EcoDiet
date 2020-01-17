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

## ---- fig1, fig.height = 4, fig.width = 6, fig.align = "center"---------------
plot_data(isotope_data = example_isotope_data,
          stomach_data = example_stomach_data)

## ---- eval = FALSE------------------------------------------------------------
#  example_stomach_data <- read.csv("./data/my_stomach_data.csv",     sep = ";")
#  example_isotope_data <- read.csv("./data/my_isotope_data.csv",     sep = ";")

## ---- eval = FALSE------------------------------------------------------------
#  trophic_enrichment_factor = c(0.8, 3.4)

## ---- eval = FALSE------------------------------------------------------------
#  literature_prior = FALSE

## -----------------------------------------------------------------------------
data <- preprocess_data(isotope_data = example_isotope_data,
                        trophic_enrichment_factor = c(0.8, 3.4),
                        literature_prior = FALSE,
                        stomach_data = example_stomach_data)

## -----------------------------------------------------------------------------
trophic_links <- 1 * (data$o > 0)
print(trophic_links)

## -----------------------------------------------------------------------------
trophic_links["small", "huge"] <- 1
print(trophic_links)

## -----------------------------------------------------------------------------
data <- preprocess_data(isotope_data = example_isotope_data,
                        trophic_enrichment_factor = c(0.8, 3.4),
                        literature_prior = FALSE,
                        trophic_links = trophic_links,
                        stomach_data = example_stomach_data)

## -----------------------------------------------------------------------------
model_string <- write_model(literature_prior = FALSE)

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
plot_results(mcmc_output, data, variables = "PI", pred = "huge")

## ---- fig4, fig.height = 4, fig.width = 6, fig.align = "center"---------------
plot_results(mcmc_output, data, variables = "PI", pred = "huge", prey = "large")

## -----------------------------------------------------------------------------
load("mcmc_output.Rdata")
mcmc_output <- signif(mcmc_output, digits = 2)
knitr::kable(head(mcmc_output))

## -----------------------------------------------------------------------------
quantiles <- apply(mcmc_output, 2, function(X) quantile(X, probs = c(0.05, 0.5, 0.95)))
quantiles <- signif(quantiles, digits = 2)
knitr::kable(quantiles)

## ---- eval = FALSE------------------------------------------------------------
#  write.table(quantiles, file = "quantiles.csv", sep = ",", col.names = NA)

## -----------------------------------------------------------------------------
data$o[] <- NA
data$y[] <- NA
data$nb_y[] <- 0

mcmc_priors <- run_model(textConnection(model_string), data,
                         nb_adapt = 1e2, nb_burnin = 1, nb_iter = 1e5)

## ---- fig5, fig.height = 4, fig.width = 6, fig.align = "center"---------------
plot_results(mcmc_priors, data, variables = "eta", pred = "large")

## ---- fig6, fig.height = 4, fig.width = 6, fig.align = "center"---------------
plot_results(mcmc_priors, data, variables = "PI", pred = "huge")

## -----------------------------------------------------------------------------
mcmc_output <- run_model(textConnection(model_string), data,
                         variables_to_save = c("delta"))

## -----------------------------------------------------------------------------
print(colMeans(mcmc_output))

## -----------------------------------------------------------------------------
devtools::session_info()

