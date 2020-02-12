# EcoDiet <img src="man/figures/logo.PNG" align="right" width="120" />

The goal of the package is to estimate a probabilistic topology matrix (all trophic link probabilities) and a diet matrix (all diet propotions) by combining biotracers and stomach content analyses in a Bayesian hierarchical model.

The full model and its application on a real dataset are described in *Hernvann et al.* (under review). Use `citation("EcoDiet")` to get the full reference.

## R installation

EcoDiet is an R package, so first and foremost, you should download and install/update R.

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

As the EcoDiet package is only stored on GitHub for now, you need to use the `install_github` function from the `devtools` package to load it. First, install the `devtools` package:

```{r, eval = FALSE}
install.packages("devtools")
```

Once `devtools` has been installed, you can load the EcoDiet package:

```{r, eval = FALSE}
devtools::install_github("pyhernvann/EcoDiet", build_vignettes = TRUE, dependencies = TRUE)
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
