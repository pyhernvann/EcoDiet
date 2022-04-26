# EcoDiet <img src="man/figures/logo.PNG" align="right" width="220" />

[![cran version](http://www.r-pkg.org/badges/version/EcoDiet)](https://cran.r-project.org/package=EcoDiet)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/EcoDiet?)](https://github.com/metacran/cranlogs.app)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4081537.svg)](https://doi.org/10.5281/zenodo.4081537)

The EcoDiet package estimates a probabilistic topology matrix (all trophic link probabilities) and a diet matrix (all diet proportions) for food webs by combining biotracer and stomach content analyses in a Bayesian hierarchical model.

The full model and its application to a real dataset are described in *Hernvann et al.* (in press). When using EcoDiet, please cite both Hernvann et al. and the package (`citation("EcoDiet")` will be updated soon):

<blockquote><p>Hernvann, P. Y., Gascuel, D., Kopp, D., Robert, M., & Rivot, E. (2022). EcoDiet: A hierarchical Bayesian model to combine stomach, biotracer, and literature data into diet matrix estimation. Ecological Applications, 32(2), e2521. </p></blockquote>

<blockquote><p>Théro, H., Rivot, E., Robert, M., Guitton, J., Kopp, D., Gascuel, D., et Hernvann, P.Y. (2020) EcoDiet. R package. doi: 10.5281/zenodo.4081537 </p></blockquote>

To access the citation and DOI of the latest development version, check the [Zenodo repository](https://doi.org/10.5281/zenodo.4081537)

## R installation

EcoDiet is a R package so, first and foremost, you should download and install/update R.

## JAGS installation

EcoDiet relies on the JAGS software, so you also need to download and install/update JAGS. You should install the last version of JAGS so that your model will run at maximal speed.

#### For Windows users

Download JAGS from [here](https://sourceforge.net/projects/mcmc-jags/), then follow the indications.

#### For MacOS and Linux users

Try this command if you are on MacOS:
```
sudo brew update
sudo brew install jags
```

or this command if you are on Linux:
```
sudo apt-get update
sudo apt-get install jags
```

If you get an error message during installation, check [this post from Yu-Sung Su's blog](http://yusung.blogspot.com/2009/01/install-jags-and-rjags-in-fedora.html).

## EcoDiet installation

To download the EcoDiet package from GitHub, you need to use the `install_github` function from the `devtools` package to load it. First, install the `devtools` package:

```{r, eval = FALSE}
install.packages("devtools")
```

Once `devtools` has been installed, you can load the EcoDiet package:

```{r, eval = FALSE}
devtools::install_github("pyhernvann/EcoDiet", build_vignettes = TRUE, dependencies = TRUE)
```

Since the 6th of May 2020 EcoDiet is also available on the CRAN. You can directly install it from R and load it:

```{r, eval = FALSE}
install.packages("EcoDiet")

library("EcoDiet")
```

## Learn how to use EcoDiet

Several vignettes explain how to use the package. Run the following in R and click on the HTML button next to "1. Introduction - How to use EcoDiet":
```
browseVignettes("EcoDiet")
```
Or use this if the former does not work:
```
vignette("introduction_EcoDiet")
```
