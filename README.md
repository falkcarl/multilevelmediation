# multilevelmediation

## Overview

multilevelmediation contains functions for computing indirect effects with
multilevel models and obtaining confidence intervals for various effects using
bootstrapping. The ultimate goal is to support 2-2-1, 2-1-1, and 1-1-1 models,
the option of a moderating variable at level 1 or level 2 for either the *a*, *b*,
or both paths, and a wide array of random effects for any coefficients
represented in level 1 equations (including those
for the 1-1-1 model and random effects for the moderator).
Resampling assumes random sampling at both levels. Future
support is intended for other sampling schemes (random sampling of only level 1
or level 2 units) and support for covariates. Currently only continuous mediators
and outcomes are supported. Factors (e.g., for X) must be numerically represented.

## Installation

``` r
# From GitHub:
# install.packages("devtools")
devtools::install_github("falkcarl/multilevelmediation")
```

## Some relevant references

(Forthcoming)

BPG06

Others

