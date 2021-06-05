#' Custom model fitting function for a 1-1-1 (moderated) mediation for the brms code
#'
#' @param data Data frame in long format.
#' @param L2ID (Character) Name of column that contains grouping variable in \code{data} (e.g., \code{"SubjectID"}).
#' @param X (Character) Name of column that contains the X independent variable in \code{data}.
#' @param Y (Character) Name of column that contains the Y dependent variable in \code{data}.
#' @param M (Character) Name of column that contains the M mediating variable in \code{data}.
#' @param random.a (Logical) Add random slope for 'a' path (i.e,. SmX)?
#' @param random.b (Logical) Add random slope for 'b' path (i.e., SyM)?
#' @param random.cprime (Logical) Add random slope for 'cprime' direct effect path (i.e., SyX)?
#' @param moderator Optional Character that contains name of column that contains the moderator variable in \code{data}
#' @param mod.a (Logical) Add moderator to 'a' path (i.e., SmX:W, where W is the moderator)?
#' @param mod.b (Logical) Add moderator to 'b' path (i.e., SyM:W, where W is the moderator)?
#' @param mod.cprime (Logical) Add moderator to 'c' path (i.e., SyX:W, where W is the moderator)
#' @param covars.m (Character vector) Optional covariates to include in the model for M. Currently set to null for brms
#' @param covars.y (Character vector) Optional covariates to include in the model for Y. Currently set to null for brms
#' @param random.mod.a (Logical) Add random slope for 'a' path moderator?
#' @param random.mod.b (Logical) Add random slope for 'b' path moderator?
#' @param random.mod.cprime (Logical) Add random slope for 'c' path moderator?
#' @param random.mod.m (Logical) Add random slope for effect of moderator on M?
#' @param random.mod.y (Logical) Add random slope for effect of moderator on Y?
#' @param random.covars.m (Logical vector) Add random slopes for covariates on M? Currently set to Null for brms
#' @param random.covars.y (Logical vector) Add random slopes for covariates on Y? Currently set to Null for brms
#' @param chains Argument passed to \code{\link[brms]{brms}} Set the number of chains
#' @param family Argument passed to \code{\link[brms]{brms}} A character string naming the distribution of the response variable to be used in the model
#' @param iter Argument passed to \code{\link[brms]{brms}} Number of total iterations per chain (including warmup; defaults to 2000)
#' @param control Argument passed to \code{\link[brms]{brms}} To decrease (or eliminate at best) the number of divergent transitions that cause a bias in the obtained posterior samples.
#' @param returndata (Logical) Whether to save restructured data in its own slot. Note: nlme may do this automatically. Defaults to \code{FALSE}.
#' Should the chains, iter, family, and control parameters be included in the function as default parameters?
#' @details TO DO. Implements custom function to do moderated mediation with multilevel models in the Bayesian setting.
#'   Capable of doing moderation as well. Need to detail which kinds of moderation. Believed that it currently includes 1-1-1
#'   model moderation at level-1. Can include moderation at any path, but currently, only moderation in the a and b paths was tested.
#'   Does not handle covariates at the moment.
#'   Initially implemented for the BPG06 model for 1-1-1 mediation with moderation...
#' @examples
#' \donttest{
#' TO DO: Include more tests: Bayesian interval (HPD, CI or both? Milica), moderation at various paths, formulas for indirect effect
#' at various values of the moderator. Probably other stuff.
#'
#' Example data for 1-1-1 w/o moderation
#' data(simdat)
#'
#' #Fit model
#' fit<-modmed.mlm.brms(simdat,"id", "x", "y", "m",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE)
#'
#'
#' }
#' @import brms
#' @importFrom matrixcalc vech
#' @importFrom MCMCpack xpnd
#' @importFrom stats as.formula
#' @export modmed.mlm.brms

modmed.mlm.brms<-function(data, L2ID, X, Y, M,
                     moderator = NULL, mod.a = FALSE, mod.b = FALSE, mod.cprime = FALSE,
                     random.a = FALSE, random.b = FALSE, random.cprime = FALSE,
                     random.mod.a = FALSE, random.mod.b = FALSE, random.mod.cprime = FALSE,
                     random.mod.m = FALSE, random.mod.y = FALSE, covars.m = NULL, covars.y = NULL, family = gaussian ,iter = 7000, control = list(adapt_delta=0.95), chains = 4,
                     returndata = FALSE){

  tmp <- stack.bpg(data, L2ID, X, Y, M,
                   moderator=moderator,
                   covars.m = covars.m,
                   covars.y = covars.y
  )

# Create the formula for the fixed effects
  fixed.formula <- "Z ~ 0 + Sm + Sy + SmX + SyX + SyM +" #use the default formula from BPG 2006

  # Create the formula for the random effects
  random.formula <- "( 0 + Sm + Sy"
  if (random.a == TRUE) {random.formula <- paste(random.formula, "+ SmX")}
  if (random.b == TRUE) {random.formula <- paste(random.formula, "+ SyM")}
  if (random.cprime == TRUE) {random.formula <- paste(random.formula, "+ SyX")}

  # Add in the grouping variable after all the variables are entered
  random.formula <- paste(random.formula, "| L2id )")

  # Concatenate the fixed and random formula
  formula <- paste(fixed.formula, random.formula)

  mod_med_brms_tmp <- try(brm(formula = bf(as.formula(formula), sigma ~ 0 + Sm + Sy),
                         data = tmp,
                         family = family,
                         iter = iter,
                         control = control,
                         chains = chains))

  # create output list
  out <- list()

  # some error handling, just in case
  if (class(mod_med_brms_tmp) == "try-error") {
    out$model <- NULL
    out$conv <- FALSE # boolean or some other code?
  } else {
    out$model <- mod_med_brms_tmp
    out$conv <- TRUE
  }

  if(returndata) out$data <- tmp

  #out$call <- match.call()
  out$args<-list(
    L2ID = L2ID,
    X = X,
    Y = Y,
    M = M,
    random.a = random.a,
    random.b = random.b,
    random.cprime = random.cprime,
    family = family,
    iter = iter,
    control = control,
    chains = chains,
    returndata = returndata
  )

  return(out)


}

library(ggplot2)
library(tidyr)
library(Rcpp)
library(StanHeaders)
library(rstan)
library(brms)
library(multilevelmediation)

data("BPG06dat")

fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",
                     random.a=TRUE, random.b=TRUE, random.cprime=TRUE)




