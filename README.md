# multilevelmediation

## Overview

multilevelmediation contains functions for computing indirect effects with
multilevel models and obtaining confidence intervals for various effects using
bootstrapping. The ultimate goal is to support 2-2-1, 2-1-1, and 1-1-1 models,
the option of a moderating variable at level 1 or level 2 for either the *a*, *b*,
or both paths. Currently the 1-1-1 model is supported and several options of
random effects are supported; the underlying initial code has been evaluated in
simulations (see Falk et al in references). Support for Bayesian estimation and
the inclusion of covariates comprises ongoing work. Currently only continuous
mediators and outcomes are supported. Factors (e.g., for X) must be numerically
represented.

## Installation

Note that GitHub contains the development version of the package. If you want
new, sometimes minimally tested features, install from here.

``` r
# From GitHub:
# install.packages("devtools")
devtools::install_github("falkcarl/multilevelmediation")
```

Otherwise, a release should be available on CRAN:

``` r
install.packages("multilevelmediation")
```

## Some relevant references

Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing random indirect effects and moderated mediation in multilevel models: New procedures and recommendations. Psychological Methods, 11(2), 142–163. https://doi.org/10.1037/1082-989X.11.2.142

Carpenter, J. R., Goldstein, H., & Rasbash, J. (2003). A novel bootstrap procedure for assessing the relationship between class size and achievement. Applied Statistics, 52(4), 431-443.

Falk, C. F., Vogel, T., Hammami, S., & Miočević, M. (in press). Multilevel mediation analysis in R: A comparison of bootstrap and Bayesian approaches. Behavior Research Methods. doi: https://doi.org/10.3758/s13428-023-02079-4  Preprint: https://doi.org/10.31234/osf.io/ync34

Hox, J., & van de Schoot, R. (2013). Robust methods for multilevel analysis. In M. A. Scott, J. S. Simonoff & B. D. Marx (Eds.), The SAGE Handbook of Multilevel Modeling (pp. 387-402). SAGE Publications Ltd. doi: 10.4135/9781446247600.n22

Krull, J. L., & MacKinnon, D. P. (2001). Multilevel modeling of individual and group level mediated effects. Multivariate behavioral research, 36(2), 249-277. doi: 10.1207/S15327906MBR3602_06

van der Leeden, R., Meijer, E., & Busing, F. M. T. A. (2008). Resampling multilevel models. In J. de Leeuw & E. Meijer (Eds.), Handbook of Multilevel Analysis (pp. 401-433). Springer.


## FAQ

- How to handle missing data?
    - Missing data handling of the sort that `lme` (the function from the `nlme` package that fits the models) supports is available. Pass an argument (to `modmed.mlm` or any of the bootstrapping functions) for `na.action` that will be passed down to the `lme` function. For example, `na.action = na.omit`.
- Where is support for Bayesian estimation?
    - There is a branch started for use with the `brms` package. When it is finished an update shall be posted.
- I receive an error message with a `tibble` as input
    - Try converting the data to a data frame. Support to automatically do this may eventually be forthcoming, but it should be easy for the end user to do this.

## Updates

- Version 0.4.0
    - Support for `glmmTMB` (resid bootstrap still forthcoming).
    - Bugfix to error handling when covariates have random effects.
    - Bugfix: residual bootstrap assumed data sorted by IDs
    - Probable bugfix: bootstrapping with data input as tibble
    - Ability to omit intercept random effects.
- Version 0.3.1
    - Random number seed for `boot.modmed.mlm.custom` is not set by default (it's `NULL`).
    - Update to docs.
- Version 0.3.0
    - Merged branch for `brms` into master. This means that some support for `brms` is provided. Covariates with `brms` are not yet supported and that code could use some more testing. Also
    protect against possible bug for `boot.modmed.mlm.custom`.
- Version 0.2.1
    - Update to docs so that variables in restacked data are hopefully clearer.
    - Support for arbitrary function applied to data after restacking and prior to model fitting in `modmed.mlm`. Could support additional centering and/or missing data handling.
- Version 0.2.0
    - `boot.modmed.mlm.custom` introduced as a new function to unify all case bootstrapping and residual bootstrapping methods into one function and obtain further gains in speed. This reduces reliance on the `boot` package and appears to be a bit faster. Testing is still in progress, though this function may soon replace `boot.modmed.mlm`.
    - Update so that missing data can be used with `modmed.mlm` and `boot.modmed.mlm`. Pass an argument for `na.action` that will be passed down to the `lme` function. For example, `na.action = na.omit`.

