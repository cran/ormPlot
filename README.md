# ormPlot

<!-- badges: start -->
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/ormPlot)](https://cran.r-project.org/package=ormPlot)
[![Build Status](https://travis-ci.com/rix133/ormPlot.svg?branch=master)](https://travis-ci.com/rix133/ormPlot)
[![Codecov test coverage](https://codecov.io/gh/rix133/ormPlot/branch/master/graph/badge.svg)](https://codecov.io/gh/rix133/ormPlot?branch=master)
<!-- badges: end -->

# Plotting ordinal regression models from R package rms


The goal of ormPlot is to to extend the plotting capabilities of rms package. 

In particular it aims to provide convenient ways of getting ggplot2 plots 
from orm and lrm models of the rms package.

It provides:
 * prediction plots of orm models for each dependent variable level together
   with coefficient intervals.
 * forest plots of orm/lrm model summaries
 * data about schoolchildren body measurements and their family details like
   socioeconomic status and number of siblings

## Installation

You can install the CRAN release of ormPlot from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ormPlot")
```

To install the latest version do:

``` r
install.packages("remotes")
remotes::install_github("rix133/ormPlot")
```

## Examples


vignette("ormPlot")
help("ormPlot")


See the vigentte and/or help files:

``` r
vignette("ormPlot")
help("ormPlot")
```
To get you started:

``` r
#load the libraries
library(rms)
library(ormPlot)

#make the datadist
dd<-datadist(educ_data)
options(datadist="dd")

#create the model
cran_model <- orm(educ_3 ~ YOBc +Rural + sex + height_rzs + n_siblings  + cran_rzs, data = educ_data)

#show simply the summary plot
forestplot(summary(cran_model))

#show the predictions
plot(cran_model, cran_rzs, plot_cols = Rural, plot_rows = sex)
```

