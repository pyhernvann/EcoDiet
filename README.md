# EcoDiet <img src="man/figures/logo.PNG" align="right" width="120" />

*This is a work in progress!*

## R installation

EcoDiet is an R package, so first and foremost, you should download and install/update R.

## JAGS installation

EcoDiet relies on the JAGS program (Plummer, 2013), so you also need to download and install/update JAGS.

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
devtools::install_github("heloisethero/EcoDiet", build_vignettes = TRUE)
```

## Learn how to use EcoDiet

Several vignettes will then explain to you how to use this package depending on the data types available. To access the vignettes run the following in R:
```
browseVignettes("EcoDiet")
```
And click on the HTML button next to the vignette you are interested in, for example "Introduction to EcoDiet (quick start)".
