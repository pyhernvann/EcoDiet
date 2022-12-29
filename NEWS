# Changes from Version 1.0.1 to 2.0.0 [31 Dec 2022] #

Major changes brought to the whole package:

-  `EcoDiet` now uses the `jagsUI` package instead of the `rjags` package as an interface with `JAGS`.
`jagsUI` allows for a more flexible design of the model runs and parallelization on multiple cores/servers, which substantially reduces the time required for runs with multiple chains.
The MCMC sampling procedure have also been optimized and now looks closer to what is done by similar packages such as MixSIAR.

-  `run_model` function

    *   Modified to work with the `jagsUI` package.
    *   While it was previously returning a matrix object containing the MCMC samples for the monitored variables, it outputs an object of jagsUI type containing (i) the model parameterization, (ii) the full MCMC samples, (iii) summary of main information and other statistics. It also prints the classical messages returned by `jagsUI` once the model has run.
    *   The provision of a nb_adapt value is mandatory anymore (see the documentation about `jagsUI`) .
    *   A new argument allows the user to parallelize the chains when running the model (on different cores).

-  `write_model` function now allows to (i) specify a name and location for the BUGS model written and saved as a '.txt' file and (ii) print the model in the console. 

-  `diagnose_model` function has been created to generate the Gelman-Rubin diagnostic of the variables. It returns a table of the Gelman-Rubin test results and prints a summary of these statistics. This function also produces and ensemble of diagnostic plots that are directly saved in a pdf document.


Various fixes:

-  Visualization functions has been revised and previous issues have been fixed.


# Changes from Version 1.0.0 to 1.0.1 [25 Sept 2022] #

Fix minor issues with the synthax used in some Vignettes.