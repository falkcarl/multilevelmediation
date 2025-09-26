#' Model definition and estimation function for two-level (moderated) mediation
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
#' @param covars.m (Character vector) Optional covariates to include in the model for M.
#' @param covars.y (Character vector) Optional covariates to include in the model for Y.
#' @param random.mod.a (Logical) Add random slope for 'a' path moderator?
#' @param random.mod.b (Logical) Add random slope for 'b' path moderator?
#' @param random.mod.cprime (Logical) Add random slope for 'c' path moderator?
#' @param random.mod.m (Logical) Add random slope for effect of moderator on M?
#' @param random.mod.y (Logical) Add random slope for effect of moderator on Y?
#' @param random.covars.m (Character vector) If any covariates specified from \code{covars.m} are present, random effects
#'   are added for them.
#' @param random.covars.y (Character vector) If any covariates specified from \code{covars.y} are present, random effects
#'   are added for them.
#' @param random.int.m (Logical) Add random intercept for M? (defaults to TRUE)
#' @param random.int.y (Logical) Add random intercept for Y? (defaults to TRUE)
#' @param method Argument used to control estimation method. Options are "REML" (default) or "ML".
#' @param estimator (Character) Which program to use to estimate models? \code{\link[nlme]{lme}} is what was originally tested
#'   with the package and publication, but support for \code{\link[glmmTMB]{glmmTMB}} is now available.
#' @param control Argument passed to \code{\link[nlme]{lme}} or \code{\link[glmmTMB]{glmmTMB}} that controls other estimation options.
#'   See those functions for the \code{control} argument. If \code{\link[nlme]{lme}} is chosen for estimation, but nothing is specified
#'   for \code{control}, some defaults values are populated that basically greatly increase the number of admissible
#'   iterations.
#' @param returndata (Logical) Whether to save restructured data in its own slot. Note: nlme may do this automatically. Defaults to \code{FALSE}.
#' @param datmfun (experimental) A function that will do additional data manipulation on the restacked dataset. The function ought to take
#'   the restacked dataset (e.g., done using \code{\link{stack_bpg}}) and return a dataset that can be analyzed using \code{\link{modmed.mlm}} Could be used for
#'   some kind of additional centering strategy after data are restacked (and after bootstrapped) or some other missing data handling strategy.
#'   Either suggestion requires further study.
#' @param data.stacked (experimental) Currently used internally by bootresid.modmed.mlm to feed already stacked data to the function.
#' @param ... Pass any additional options down to \code{link[nlme]{lme}}. Added to handle missing values. e.g., \code{na.action = na.omit}.
#' @details Implements custom function to do 1-1-1 multilevel mediation model following Bauer, Preacher, & Gil (2006).
#'   The basic procedure involves restructuring the data (\code{\link{stack_bpg}}) and then estimating the model using \code{\link[nlme]{lme}}.
#'   The model assumes heteroscedasticity  since the mediator and outcome variable may have different error variances.
#'   The function also supports covariates as predictors of the mediator and/or outcome, as well as moderated mediation.
#'   Currently a single moderator variable is supported and it may moderate any/all paths of the model. However, the
#'   the moderator is assumed continuous. While it may be possible to include moderators that are categorical, it is
#'   not currently automated (i.e., the user will need to manually code the categorical variable as numeric).
#'
#'   For more information for variable labels and how these will correspond to the output coefficients, see the documentation for \code{\link{stack_bpg}},
#'   as those docs contain a description of all of the variables.
#'
#' @return A list with the following elements:
#' \itemize{
#'  \item{\code{model} The fitted model using \code{\link[nlme]{lme}}. Use as you would a fitted model from that package.}
#'  \item{\code{args} Arguments used to call the function. Useful for later automating extraction of the indirect
#'    effect or other quantities.}
#'  \item{\code{conv} Whether estimation appeared to converge.}
#'  \item{\code{data} If you asked for the restructured dataset to be returned, it shall be here.}
#' }
#'
#' @references
#' Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing random indirect effects and moderated mediation in multilevel models: New procedures and recommendations. Psychological Methods, 11(2), 142–163. \doi{10.1037/1082-989X.11.2.142}
#' @examples
#'
#' \donttest{
#' # Example data for 1-1-1 w/o moderation
#' data(BPG06dat)
#'
#' # Fit model
#' fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE)
#'
#' extract.modmed.mlm(fit)
#' extract.modmed.mlm(fit, type="indirect")
#' extract.modmed.mlm(fit, type="a")
#' extract.modmed.mlm(fit, type="b")
#' extract.modmed.mlm(fit, type="covab")
#'
#' # Vector of parameter estimates, including indirect effect
#' #fit$pars
#'
#' # The saved, fitted model following Bauer, Preacher, & Gil (2006)
#' summary(fit$model)
#' }
#'
#' \donttest{
#' # Fit model with moderation
#' data(simdat)
#'
#' # moderation for a path
#' fitmoda<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE)
#'
#' # moderation for b path
#' fitmodb<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.b=TRUE)
#'
#' # moderation for both a and b paths
#' fitmodab<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE)
#'
#' # moderation for both a and b paths and random effect for interaction a
#' fitmodab2<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'   random.mod.a = TRUE, random.mod.m = TRUE)
#'
#' # moderation for both a and b paths and random effect for interaction b
#' fitmodab3<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'   random.mod.b = TRUE, random.mod.y = TRUE)
#'
#' # moderation for both a and b paths and random effect for both interactions
#' fitmodab4<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'   random.mod.a = TRUE, random.mod.b = TRUE,
#'   random.mod.m = TRUE, random.mod.y = TRUE)
#'
#' # compare models?
#' # Apparently anova() is not supported as it's looking for fixed.formula,
#' # as it's not in the current environment
#' # AIC works though
#' AIC(fitmodab$model)
#' AIC(fitmodab2$model)
#' AIC(fitmodab3$model)
#' AIC(fitmodab4$model) # AIC here is best. Great simulated data we have here
#'
#' extract.modmed.mlm(fitmodab4, "indirect")
#' extract.modmed.mlm(fitmodab4, "indirect", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab4, "indirect", modval1=1)
#' extract.modmed.mlm(fitmodab4, "indirect.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab4, "indirect", modval1=0)-
#'   extract.modmed.mlm(fitmodab4, "indirect", modval1=1) # should match prev line
#'
#' extract.modmed.mlm(fitmodab4, "a")
#' extract.modmed.mlm(fitmodab4, "a", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab4, "a", modval1=1)
#' extract.modmed.mlm(fitmodab4, "a.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab4, "a", modval1=0)-
#'   extract.modmed.mlm(fitmodab4, "a", modval1=1)  # should match prev line
#'
#' extract.modmed.mlm(fitmodab4, "b")
#' extract.modmed.mlm(fitmodab4, "b", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab4, "b", modval1=1)
#' extract.modmed.mlm(fitmodab4, "b.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab4, "b", modval1=0)-
#'   extract.modmed.mlm(fitmodab4, "b", modval1=1)  # should match prev line
#'
#' extract.modmed.mlm(fitmodab3, "indirect")
#' extract.modmed.mlm(fitmodab3, "indirect", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab3, "indirect", modval1=1)
#' extract.modmed.mlm(fitmodab3, "indirect.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab3, "indirect", modval1=0)-
#'   extract.modmed.mlm(fitmodab3, "indirect", modval1=1) # should match prev line
#'
#' extract.modmed.mlm(fitmodab3, "a")
#' extract.modmed.mlm(fitmodab3, "a", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab3, "a", modval1=1)
#' extract.modmed.mlm(fitmodab3, "a.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab3, "a", modval1=0)-
#'   extract.modmed.mlm(fitmodab3, "a", modval1=1)  # should match prev line
#'
#' extract.modmed.mlm(fitmodab3, "b")
#' extract.modmed.mlm(fitmodab3, "b", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab3, "b", modval1=1)
#' extract.modmed.mlm(fitmodab3, "b.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab3, "b", modval1=0)-
#'   extract.modmed.mlm(fitmodab3, "b", modval1=1)  # should match prev line
#'
#' extract.modmed.mlm(fitmodab2, "indirect")
#' extract.modmed.mlm(fitmodab2, "indirect", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab2, "indirect", modval1=1)
#' extract.modmed.mlm(fitmodab2, "indirect.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab2, "indirect", modval1=0)-
#'   extract.modmed.mlm(fitmodab2, "indirect", modval1=1) # should match prev line
#'
#' extract.modmed.mlm(fitmodab2, "a")
#' extract.modmed.mlm(fitmodab2, "a", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab2, "a", modval1=1)
#' extract.modmed.mlm(fitmodab2, "a.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab2, "a", modval1=0)-
#'   extract.modmed.mlm(fitmodab2, "a", modval1=1)  # should match prev line
#'
#' extract.modmed.mlm(fitmodab2, "b")
#' extract.modmed.mlm(fitmodab2, "b", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab2, "b", modval1=1)
#' extract.modmed.mlm(fitmodab2, "b.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab2, "b", modval1=0)-
#'   extract.modmed.mlm(fitmodab2, "b", modval1=1)  # should match prev line
#'
#' extract.modmed.mlm(fitmodab, "indirect")
#' extract.modmed.mlm(fitmodab, "indirect", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab, "indirect", modval1=1)
#' extract.modmed.mlm(fitmodab, "indirect.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab, "indirect", modval1=0)-
#'   extract.modmed.mlm(fitmodab, "indirect", modval1=1) # should match prev line
#'
#' extract.modmed.mlm(fitmodab, "a")
#' extract.modmed.mlm(fitmodab, "a", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab, "a", modval1=1)
#' extract.modmed.mlm(fitmodab, "a.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab, "a", modval1=0)-
#'   extract.modmed.mlm(fitmodab, "a", modval1=1)  # should match prev line
#'
#' extract.modmed.mlm(fitmodab, "b")
#' extract.modmed.mlm(fitmodab, "b", modval1=0) # should match above
#' extract.modmed.mlm(fitmodab, "b", modval1=1)
#' extract.modmed.mlm(fitmodab, "b.diff", modval1 = 0, modval2=1)
#' extract.modmed.mlm(fitmodab, "b", modval1=0)-
#'   extract.modmed.mlm(fitmodab, "b", modval1=1)  # should match prev line
#'
#'
#'# Example to not fail when using missing values
#'# Missing data handling is not that great as not all info is used, but is typical
#'# of default missing data handling strategies in MLM. See documentation for
#'# lme function in the nlme package for more options for na.action
#'dat.miss <- BPG06dat
#'dat.miss$m[c(1,2,3,4)]<-NA
#'dat.miss$y[c(5,6,7,8)]<-NA
#'fit<-modmed.mlm(dat.miss,"id", "x", "y", "m",
#'                 random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'                 na.action = na.omit)
#' }
#' @importFrom nlme lmeControl lme fixef getVarCov varIdent
#' @importFrom glmmTMB glmmTMBControl glmmTMB VarCorr
#' @importFrom matrixcalc vech
#' @importFrom MCMCpack xpnd
#' @importFrom stats as.formula
#' @export modmed.mlm
modmed.mlm <- function(
  data,
  L2ID,
  X,
  Y,
  M,
  moderator = NULL,
  mod.a = FALSE,
  mod.b = FALSE,
  mod.cprime = FALSE,
  covars.m = NULL,
  covars.y = NULL,
  random.a = FALSE,
  random.b = FALSE,
  random.cprime = FALSE,
  random.mod.a = FALSE,
  random.mod.b = FALSE,
  random.mod.cprime = FALSE,
  random.mod.m = FALSE,
  random.mod.y = FALSE,
  random.covars.m = NULL,
  random.covars.y = NULL,
  random.int.m = TRUE,
  random.int.y = TRUE,
  estimator = c("lme", "glmmTMB"),
  method = c("REML", "ML"),
  control = NULL,
  returndata = FALSE,
  datmfun = NULL,
  data.stacked = NULL,
  ...
) {
  #TODO: maybe can put all formula args into one list var, then can just pass that
  #to the FE and RE function creations, and just have it passed out at end (either spearate like it is now,
  # or just as a list var that can be subset later??)
  #(but then need to subset that list in the formula funcs below, which maybe just introducing weird dependency?)

  if (is.null(moderator) && any(mod.a, mod.b, mod.cprime)) {
    # Give error if paths indicated as moderated, but no moderator name given
    stop("No moderator was specified for the moderated path(s).")
  }

  estimator <- match.arg(estimator)
  method <- match.arg(method)

  # save default estimation options, for backwards compatibility
  if (estimator == "lme" && is.null(control)) {
    #FIXME: should just default to default lmeControl()?  how to keep this for bw compatibility, but have new stuff be the default for nlme??
    control <- lmeControl(
      maxIter = 10000L,
      msMaxIter = 10000L,
      niterEM = 10000L,
      msMaxEval = 10000L,
      tolerance = 1e-6
    )
  } else if (estimator == "glmmTMB" && is.null(control)) {
    control <- glmmTMBControl()
  }

  if (is.null(data.stacked)) {
    tmp <- stack_bpg(
      data,
      L2ID,
      X,
      Y,
      M,
      moderator = moderator,
      covars.m = covars.m,
      covars.y = covars.y
    )
  } else {
    tmp <- data.stacked
  }

  # further data manipulation, if present
  if (!is.null(datmfun)) {
    tmp <- datmfun(tmp)
  }

  fixed.formula <- make_fixed_formula(
    mod.a = mod.a,
    mod.b = mod.b,
    mod.cprime = mod.cprime,
    covars.m = covars.m,
    covars.y = covars.y
  )
  #FIXME: just pass all args to these? and have the make formaula figure out what is needed? that way don't need to change input args here AND there.
  random.formula <- make_random_formula(
    random.int.m = random.int.m,
    random.int.y = random.int.y,
    random.a = random.a,
    random.b = random.b,
    random.cprime = random.cprime,
    random.mod.a = random.mod.a,
    random.mod.b = random.mod.b,
    random.mod.cprime = random.mod.cprime,
    random.mod.m = random.mod.m,
    random.mod.y = random.mod.y,
    random.covars.m = random.covars.m,
    random.covars.y = random.covars.y,
    mod.a = mod.a,
    mod.b = mod.b,
    mod.cprime = mod.cprime
  )

  if (estimator == "lme") {
    # Run the model through nlme
    mod_med_tmp <- try(lme(
      fixed = as.formula(fixed.formula), # fixed effects
      random = as.formula(random.formula), # random effects
      weights = varIdent(form = ~ 1L | Sm), # heteroskedasticity
      data = tmp,
      method = method,
      control = control,
      subset = NULL,
      ...
    ))
  } else if (estimator == "glmmTMB") {
    #FIXME: should this also be put in a try?
    # some quick fixes to get glmmTMB up and running
    random.formula <- gsub("~ ", "", random.formula, fixed = TRUE)
    random.formula <- paste0("(", random.formula, ")")
    form <- paste0(fixed.formula, "+", random.formula)

    mod_med_tmp <- glmmTMB(
      as.formula(form),
      dispformula = ~ 1L + Sm,
      #dispformula =  ~ 0 + Sm + Sy,
      family = gaussian,
      data = tmp,
      REML = (method == "REML"),
      control = control,
      na.action = getOption("na.action"),
      ...
    )

    #Tangent, can I get asymptotic SEs from glmmTMB?
    #https://stackoverflow.com/questions/47872561/does-glmmtmb-return-the-standard-error-for-random-effect-variance-components-lik
    # var-cov matrix for all, including random effects? yes, but these are on a log scale?
    #mod_med_tmp$sdr
    #sqrt(diag(vcov(mod_med_tmp, full=TRUE)))
  }

  # create output list
  out <- list()

  # some error handling, just in case
  #FIXME: does glmmtmB give a try-error? or is that just when using try()? how to make this most flexible for other packages?
  #FIXME: wouldn't it be better to just pass the failed mod_med_tmp model, to diagnose based on lme/glmmtmb output?
  #FIXME: although would still need to wrap in a try()? Otherwise error might cause it to stop running??
  if (inherits(mod_med_tmp, "try-error")) {
    out$model <- NULL
    out$conv <- FALSE # boolean or some other code?
  } else {
    out$model <- mod_med_tmp
    out$conv <- TRUE
  }

  if (returndata) {
    out$data <- tmp
  }

  #out$call <- match.call()
  #TODO: TV: Have a test that checks that this matches the list of parameters for the function call above?
  #FIXME: ie check that the output args match the input args that are passed (including that those not passed are not included??)
  out$args <- list(
    L2ID = L2ID,
    X = X,
    Y = Y,
    M = M,
    moderator = moderator,
    mod.a = mod.a,
    mod.b = mod.b,
    mod.cprime = mod.cprime,
    covars.m = covars.m,
    covars.y = covars.y,
    random.a = random.a,
    random.b = random.b,
    random.cprime = random.cprime,
    random.mod.a = random.mod.a,
    random.mod.b = random.mod.b,
    random.mod.cprime = random.mod.cprime,
    random.mod.m = random.mod.m,
    random.mod.y = random.mod.y,
    random.covars.m = random.covars.m,
    random.covars.y = random.covars.y,
    random.int.m = random.int.m,
    random.int.y = random.int.y,
    estimator = estimator,
    method = method,
    control = control,
    returndata = returndata
  )

  return(out)
}


#TODO: add documentation using roxygen skeleton
#FIXME: rename variables to something more intuitive? Eg a, b & cprime paths?
make_fixed_formula <- function(mod.a, mod.b, mod.cprime, covars.m, covars.y) {
  fixed.formula <- "Z ~ 0 + Sm + Sy + SmX + SyX + SyM" #use the default formula from BPG 2006

  # Add in the moderator to the paths if necessary
  # Note: interactions w/ "W" must must use selector variables in this way
  if (mod.a) {
    fixed.formula <- paste(fixed.formula, "+ Sm:W + SmX:W")
  }
  #FIXME: do we need 3 unique if statements? why not just check b and then cprime? why need to add both?
  #FIXME: does formula order matter? can check lme and glmmtmb outputs to see
  if (mod.b || mod.cprime) {
    fixed.formula <- paste(fixed.formula, "+ Sy:W") #if b or c path is moderated, Sy component will always be there (prevents adding redundant parameters if both b & c are moderated)
    if (mod.b && mod.cprime) {
      fixed.formula <- paste(fixed.formula, "+ SyM:W + SyX:W")
    }
    if (mod.b && !mod.cprime) {
      fixed.formula <- paste(fixed.formula, "+ SyM:W")
    }
    if (!mod.b && mod.cprime) {
      fixed.formula <- paste(fixed.formula, "+ SyX:W")
    }
  }

  # Add any covariates to the paths if necessary
  if (!is.null(covars.m)) {
    covars.m_formula <- paste0("+ Sm:", covars.m, collapse = " ") #write the formula for each covar specified
    fixed.formula <- paste(fixed.formula, covars.m_formula) #and add to main fixed fx formula
  }
  if (!is.null(covars.y)) {
    covars.y_formula <- paste0("+ Sy:", covars.y, collapse = " ") #write the formula for each covar specified
    fixed.formula <- paste(fixed.formula, covars.y_formula) #and add to main fixed fx formula
  }
  return(fixed.formula)
}

#TODO: add documentation using roxygen skeleton
#FIXME: can maybe force randommod.a to be true only if mod.a is true? to reduce number of args here...
make_random_formula <- function(
  random.int.m,
  random.int.y,
  random.a,
  random.b,
  random.cprime,
  random.mod.a,
  random.mod.b,
  random.mod.cprime,
  random.mod.m,
  random.mod.y,
  random.covars.m,
  random.covars.y,
  mod.a,
  mod.b,
  mod.cprime
) {
  # Create the formula for the random effects
  random.formula <- "~ 0 "
  if (random.int.m) {
    random.formula <- paste(random.formula, "+ Sm")
  }
  if (random.int.y) {
    random.formula <- paste(random.formula, "+ Sy")
  }
  if (random.a) {
    random.formula <- paste(random.formula, "+ SmX")
  }
  if (random.b) {
    random.formula <- paste(random.formula, "+ SyM")
  }
  if (random.cprime) {
    random.formula <- paste(random.formula, "+ SyX")
  }

  # Add random effects for moderator here, if any
  if (random.mod.a && mod.a) {
    random.formula <- paste(random.formula, "+ SmX:W")
  }
  if (random.mod.b && mod.b) {
    random.formula <- paste(random.formula, "+ SyM:W")
  }
  if (random.mod.cprime && mod.cprime) {
    random.formula <- paste(random.formula, "+ SyX:W")
  }
  if (random.mod.m && mod.a) {
    random.formula <- paste(random.formula, "+ Sm:W")
  }
  if (random.mod.y && mod.b) {
    random.formula <- paste(random.formula, "+ Sy:W")
  }
  #TV: would there ever be a situation where Sm or Sy would be moderated, but not their paths? (eg SmX, SyM, etc)
  #TV: eg, would random.mod.m ever be true if random.mod.a was not true?
  #CFF: I would suppose it's possible. Depends on user's theory. Leave in for more flexibilty.

  # Add random effects for covariates here, if any
  #TODO: TV: currently doesn't check whether random covariates have the same
  #name as other variables in the model, or are the same covariates in the fixed fx formula.
  # Is possible to specify random covars not in the fixed formula, but may give an error with lme (although can be implemented, would just have to save random covar to the data above)
  if (!is.null(random.covars.m)) {
    random.covars.m_formula <- paste0("+ Sm:", random.covars.m, collapse = " ")
    random.formula <- paste(random.formula, random.covars.m_formula)
  }
  if (!is.null(random.covars.y)) {
    random.covars.y_formula <- paste0("+ Sy:", random.covars.y, collapse = " ")
    random.formula <- paste(random.formula, random.covars.y_formula)
  }

  # Add in the grouping variable after all the variables are entered
  random.formula <- paste(random.formula, "| L2id")
}
