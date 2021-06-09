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
#' at various values of the moderator. Should the default parameters be similar/same as brms or different? Probably other stuff. So far, only tests similar to modmed.mlm are included.
#' Maybe include other tests specific to Bayesian stats.
#'
#' Example data for 1-1-1 w/o moderation
#' data(BPG06dat)
#'
#' Only fixed effects
#' fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y" , "m")
#'
#'
#' #Only random a
#' fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",random.a=TRUE)
#'
#' #Only random b
#' fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",random.b=TRUE)
#'
#' #Random a and b
#' fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",
#'   random.a=TRUE, random.b=TRUE)
#'
#'  #All random
#' fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE)
#'
#'# Example data for 1-1-1 with moderation
#'
#'# Fit model with moderation
#' data(simdat)
#'
#' # moderation for a path
#' fitmoda<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE)
#'
#' # moderation for b path
#' fitmodb<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.b=TRUE)
#'
#' # moderation for both a and b paths
#' fitmodab<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE)
#'
#' # moderation for both a and b paths and random effect for interaction a
#' fitmodab2<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'   random.mod.a = TRUE, random.mod.m = TRUE)
#'
#' # moderation for both a and b paths and random effect for interaction b
#' fitmodab3<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'   random.mod.b = TRUE, random.mod.y = TRUE)
#'
#' # moderation for both a and b paths and random effect for both interactions
#' fitmodab4<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'   random.mod.a = TRUE, random.mod.b = TRUE,
#'   random.mod.m = TRUE, random.mod.y = TRUE)
#'
#'   }
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

  if (is.null(moderator) && any(mod.a, mod.b, mod.cprime)) {
    # Give error if paths indicated as moderated, but no moderator name given
    stop("No moderator was specified for the moderated path(s).")
  }

  tmp <- stack.bpg(data, L2ID, X, Y, M,
                   moderator=moderator,
                   covars.m = covars.m,
                   covars.y = covars.y
  )

# Create the formula for the fixed effects
  fixed.formula <- "Z ~ 0 + Sm + Sy + SmX + SyX + SyM" #use the default formula from BPG 2006

  # Add in the moderator to the paths if necessary
  # Note: interactions w/ "W" must must use selector variables in this way
  if (mod.a == TRUE) {fixed.formula <- paste(fixed.formula, "+ Sm:W + SmX:W")}
  if (mod.b == TRUE || mod.cprime == TRUE) {
    fixed.formula <- paste(fixed.formula, "+ Sy:W") #if b or c path is moderated, Sy component will always be there (prevents adding redundant parameters if both b & c are moderated)
    if (mod.b == TRUE && mod.cprime == TRUE) {fixed.formula <- paste(fixed.formula, "+ SyM:W + SyX:W")}
    if (mod.b == TRUE && mod.cprime == FALSE) {fixed.formula <- paste(fixed.formula, "+ SyM:W")}
    if (mod.b == FALSE && mod.cprime == TRUE) {fixed.formula <- paste(fixed.formula, "+ SyX:W")}
  }

# Create the formula for the random effects
  random.formula <- " + (0 + Sm + Sy"
  if (random.a == TRUE) {random.formula <- paste(random.formula, "+ SmX")}
  if (random.b == TRUE) {random.formula <- paste(random.formula, "+ SyM")}
  if (random.cprime == TRUE) {random.formula <- paste(random.formula, "+ SyX")}

  # Add random effects for moderator here, if any
  if(random.mod.a && mod.a){random.formula <- paste(random.formula, "+ SmX:W")}
  if(random.mod.b && mod.b){random.formula <- paste(random.formula, "+ SyM:W")}
  if(random.mod.cprime && mod.cprime){random.formula <- paste(random.formula, "+ SyX:W")}
  if(random.mod.m && mod.a){random.formula <- paste(random.formula, "+ Sm:W")}
  if(random.mod.y && mod.b){random.formula <- paste(random.formula, "+ Sy:W")}

# Add in the grouping variable after all the variables are entered
  random.formula <- paste(random.formula, "| L2id )")

#Check if formula contains obly fixed-effects.
  if (random.a == FALSE && random.b == FALSE && random.cprime == FALSE){
    formula <-fixed.formula
  }
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

#Some testing for me, but maybe I should put the testing in a testing file?

#library(ggplot2)
#library(tidyr)
#library(Rcpp)
#library(StanHeaders)
#library(rstan)
#library(brms)
#library(multilevelmediation)

#data("BPG06dat")
#data("simdat")

#fit<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M", moderator = "mod", random.mod.a = TRUE ,chains = 2)




