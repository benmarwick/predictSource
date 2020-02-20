
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predictSource

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/benmarwick/predictSource.svg?branch=master)](https://travis-ci.org/benmarwick/predictSource)
<!-- badges: end -->

The package predictSource provides functions to verify that data can be
used to separate sources of samples and then to predict the sources of
additional samples. Data can be both quantitative and qualitative. The
data used as examples are 5 elements from 113 obsidian samples from five
sources in the Jemez caldera in north central New Mexico USA and the
same 5 elements from 91 artifacts obtained from nearby archaeological
sites.

The package contains functions for exploratory data analysis
(descriptive statistics, 2- and 3-dimensional plots \[the latter can be
rotated\], tests for 1- and 2-dimensional Gaussian distributions
\[helpful in identifying outliers\]) and multivariate analysis
(principal components, classification trees, and random forests). A
proposed analysis strategy is to use random forests to evaluate whether
the data can separate the sources and to identify the most important
predictors if there are many, use a classification tree to understand
how the data are used to sources, use random forests to predict the
sources of unknown samples, and then evaluate the validity of the
predictions by ploting the first two principal components of the
unknowns with the convex hulls of the known sources. The random forest
analysis also produces the probabilities of assignment to each source
for each sample; this can be helpful in identifying unknowns that are
difficult to classify.

Obsidian sources are usually predicted using 2- and 3-dimensional
scatter plots. This methodology is much faster and can identify errors
in assignment (the example data includes one such error). Archaological
information should be used with any analysis procedure.

A detailed vignette provides examples for the use of each function
(using the obsidian data) and some background for classification trees,
random forests, and checking for Gaussian distributions.

## Run in Binder

[![Binder](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/benmarwick/predictSource/master)

Click on the button above to launch RStudio in your browser so you can
test or use this package without installing it locally on your computer.
See these
[instructions](https://github.com/rocker-org/binder#opening-rstudio-once-binder-launches)
for full details on how to to open RStudio.

## Installation

You can install predictSource from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("benmarwick/predictSource")
```
