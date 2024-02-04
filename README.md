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

``` r
# From GitHub:
# install.packages("devtools")
devtools::install_github("falkcarl/multilevelmediation")
```

## Some relevant references

Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing random indirect effects and moderated mediation in multilevel models: New procedures and recommendations. Psychological Methods, 11(2), 142–163. https://doi.org/10.1037/1082-989X.11.2.142

Carpenter, J. R., Goldstein, H., & Rasbash, J. (2003). A novel bootstrap procedure for assessing the relationship between class size and achievement. Applied Statistics, 52(4), 431-443.

Falk, C. F., Vogel, T., Hammami, S., & Miočević, M. (in press). Multilevel mediation analysis in R: A comparison of bootstrap and Bayesian approaches. Behavior Research Methods. doi: https://doi.org/10.3758/s13428-023-02079-4  Preprint: https://doi.org/10.31234/osf.io/ync34

Hox, J., & van de Schoot, R. (2013). Robust methods for multilevel analysis. In M. A. Scott, J. S. Simonoff & B. D. Marx (Eds.), The SAGE Handbook of Multilevel Modeling (pp. 387-402). SAGE Publications Ltd. doi: 10.4135/9781446247600.n22

Krull, J. L., & MacKinnon, D. P. (2001). Multilevel modeling of individual and group level mediated effects. Multivariate behavioral research, 36(2), 249-277. doi: 10.1207/S15327906MBR3602_06

van der Leeden, R., Meijer, E., & Busing, F. M. T. A. (2008). Resampling multilevel models. In J. de Leeuw & E. Meijer (Eds.), Handbook of Multilevel Analysis (pp. 401-433). Springer.


