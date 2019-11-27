# EcoDiet

*Work in progress!*

## To install the package

1. Download and install/update R

1. Download and install [JAGS](https://sourceforge.net/projects/mcmc-jags/)

To ensure JAGS is found by the R program, precise its path with this command in R:
```
Sys.setenv(JAGS_HOME='****/Program Files/JAGS/JAGS-4.2.0')
```

3. Download the EcoDiet package with this command in R:

```
devtools::install_github("heloisethero/EcoDiet", auth_token='c570f1b55d68a8ae0fb38428155e19a426814d15')
```

## To learn how to use the package

The vignettes will explain to you how to use this package. To access the vignette you can run:
```
browseVignettes("EcoDiet")
```
or this:
```
vignette("EcoDiet_vignette")
```

![](man/figures/logo.PNG)
