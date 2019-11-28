# EcoDiet

*This is a work in progress!*

## R installation

EcoDiet is an R package so before anything, you should download and install/update R.

## JAGS installation

EcoDiet relies on the JAGS program to run so you will also need to download and install JAGS.

#### For Windows users

You can:

To ensure JAGS is found by the R program, precise its path with this command in R:
```
Sys.setenv(JAGS_HOME='****/Program Files/JAGS/JAGS-4.2.0')
```

#### For MacOS and Linux users (simple procedure)

Try this command if you are on MacOS:
```
sudo brew install jags
```

or this command if you are on Linux:
```
sudo apt-get install jags
```

If you get an error message during installation, pleasure try the longer procedure described below.

#### For MacOS and Linux users (longer procedure)

* You need to first download JAGS source code from [here](https://sourceforge.net/projects/mcmc-jags/). The file you will get should look like this: `JAGS-4.3.0.tar.gz`. (The version number may be different.)

* You will then need to uncompress the file with this bash command on the terminal:
```
tar -xzf JAGS-4.3.0.tar.gz
```
And go within the JAGS folder:
```
cd JAGS-4.3.0/
```

* Finally you can install JAGS with this serie of commands:
```
./configure
make
sudo make install
```

## rjags installation

Then you need to install the rjags package for R.

#### For Windows users

#### For MacOS and Linux users

First you should run on the terminal:
```
which jags
```

If nothing happens, it means you have not managed to install JAGS, so you should go back to the previous section.

If you see `/usr/bin/jags` appearing, you are safe and you can use this simple command in R:
```
install.packages('rjags')
```

If you see another path appearing such as `/usr/local/bin/jags`, you will need to specify this path to rjags with this command in R:
```
install.packages("rjags", configure.args="--with-jags-include=/usr/local/include/JAGS 
--with-jags-lib=/usr/local/lib/JAGS --with-jags-modules=/usr/local/lib/JAGS/modules")
```

Thanks to [Yu-Sung Su's blog](http://yusung.blogspot.com/2009/01/install-jags-and-rjags-in-fedora.html) for this solution.

## EcoDiet installation

As the EcoDiet package is only stored on GitHub for now, we need to use the `install_github` function from the `devtools` package to load it.

So we will first install and load the `devtools` package:

```{r, eval = FALSE}
install.packages("devtools")
library(devtools)

```

And now we can load the EcoDiet package:

```{r, eval = FALSE}
devtools::install_github("heloisethero/EcoDiet", auth_token='c570f1b55d68a8ae0fb38428155e19a426814d15')
```

## To learn how to use EcoDiet

The vignettes will then explain to you how to use this package. To access the vignette you can run in R:
```
browseVignettes("EcoDiet")
```
or:
```
vignette("EcoDiet_vignette")
```

![](man/figures/logo.PNG)
