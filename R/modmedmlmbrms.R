# Initial draft by Sarah Hammami, technical advice by Milica Miocevic

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
#' @param covars.m (Character vector) Optional covariates to include in the model for M. (not yet implemented for brms)
#' @param covars.y (Character vector) Optional covariates to include in the model for Y. (not yet implemented for brms)
#' @param random.mod.a (Logical) Add random slope for 'a' path moderator?
#' @param random.mod.b (Logical) Add random slope for 'b' path moderator?
#' @param random.mod.cprime (Logical) Add random slope for 'c' path moderator?
#' @param random.mod.m (Logical) Add random slope for effect of moderator on M?
#' @param random.mod.y (Logical) Add random slope for effect of moderator on Y?
#' @param random.covars.m (Logical vector) Add random slopes for covariates on M? (not yet implemented for brms)
#' @param random.covars.y (Logical vector) Add random slopes for covariates on Y? (not yet implemented for brms)
#' @param random.int.m (Logical) Add random intercept for M? (defaults to TRUE)
#' @param random.int.y (Logical) Add random intercept for Y? (defaults to TRUE)
#' @param returndata (Logical) Whether to save restructured data in its own slot. Defaults to \code{FALSE}.
#' @param chains Argument passed to \code{\link[brms]{brm}} Set the number of chains
#' @param family Argument passed to \code{\link[brms]{brm}} A character string naming the distribution of the response variable to be used in the model.
#' @param iter Argument passed to \code{\link[brms]{brm}} Number of total iterations.
#' @param ... Additional arguments to pass to \code{\link[brms]{brm}}
#' @param control Argument passed to \code{\link[brms]{brm}} To decrease (or eliminate at best) the number of divergent transitions that cause a bias in the obtained posterior samples.
#' @details Implements custom function to do (moderated) mediation with two-level multilevel models
#'   with Bayesian estimation via the \code{\link[brms]{brms}} package. Does not handle covariates at the moment.
#'   Bayesian estimation using \code{\link[brms]{brms}} was studied by Falk, Vogel, Hammami & Miočević (in press). It is
#'   suggested if you use this function that you also do \code{cite("brms")} to figure out how to cite that package.
#' @return A list with the following elements:
#' \itemize{
#'  \item{\code{model} The fitted model from \code{\link[brms]{brm}}. Use as you would a fitted model from that package.}
#'  \item{\code{args} Arguments used to call the function. Useful for later automating extraction of the indirect
#'    effect or other quantities.}
#'  \item{\code{conv} Whether \code{\link[brms]{brm}} finished estimation, not diagnostic of convergence.}
#' }
#' @references
#' Falk, C. F., Vogel, T., Hammami, S., & Miočević, M. (in press). Multilevel mediation analysis in R: A comparison of bootstrap and Bayesian approaches. Behavior Research Methods. \doi{10.3758/s13428-023-02079-4}  Preprint: \doi{10.31234/osf.io/ync34}
#'
#' Paul-Christian Bürkner (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. Journal of Statistical Software, 80(1), 1-28. doi:10.18637/jss.v080.i01
#'
#' @examples
#' \donttest{
#'
#' # Note: 2000 iterations is just an example so that run time is not too long.
#' # Pick something larger (e.g., 5000+) in practice
#'
#' # Example data for 1-1-1 w/o moderation
#' data(BPG06dat)
#'
#' # random effects for a and b paths (and intercept), no moderation
#' # (For moderation, note that modmed.mlm syntax is typically the same)
#' fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y" , "m", cores=2,
#'                      random.a=TRUE, random.b=TRUE,
#'                      iter = 2000, control = list(adapt_delta=0.95),
#'                      seed = 1234)
#'
#' # Examine model results and some diagnostics
#' summary(fit$model)
#'
#' # Potential scale reduction (PSR) or Rhat guidelines vary but the largest
#' #  should be close to 1 ( < 1.1, < 1.05, < 1.01).
#' # It is also possible to extract all of them.
#' max(brms::rhat(fit$model)) # largest rhat
#'
#' # Fit (loo and WAIC)
#' brms::loo(fit$model)
#' brms::waic(fit$model)
#'
#' # Point and interval estimates, diagnostics, for quantities of interest
#'
#' # Traceplots: TODO, list conversions for how brms represents parameters with
#' # How these are colloquially referred to in mediation literature.
#' plot(fit$model, variable="b_SmX") # this is traceplot for one parameter
#'
#' # Example of extracting/computing intervals for particular quantities
#' res.indirect <- extract.modmed.mlm.brms(fit, "indirect")
#' res.a <- extract.modmed.mlm.brms(fit, "a")
#' res.b <- extract.modmed.mlm.brms(fit, "b")
#' res.cprime <- extract.modmed.mlm.brms(fit, "cprime")
#'
#' # Summary of results is in CI slot, example:
#' res.indirect$CI
#'
#' # 99% CI
#' res.indirect <- extract.modmed.mlm.brms(fit, "indirect", ci.conf = .99)
#' }
#'
#' @import brms
#' @importFrom matrixcalc vech
#' @importFrom MCMCpack xpnd
#' @importFrom stats as.formula gaussian
#' @export
modmed.mlm.brms<-function(data, L2ID, X, Y, M,
                     moderator = NULL, mod.a = FALSE, mod.b = FALSE, mod.cprime = FALSE,
                     covars.m = NULL, covars.y = NULL,
                     random.a = FALSE, random.b = FALSE, random.cprime = FALSE,
                     random.mod.a = FALSE, random.mod.b = FALSE, random.mod.cprime = FALSE,
                     random.mod.m = FALSE, random.mod.y = FALSE,
                     random.covars.m = NULL, random.covars.y = NULL,
                     random.int.m = TRUE, random.int.y = TRUE,
                     returndata = FALSE,
                     family = gaussian, iter = 7000, control = list(adapt_delta=0.95), chains = 4,
                     ...){

  if(!is.null(covars.m) | !is.null(covars.y) | !is.null(random.covars.m) | !is.null(random.covars.y)){
    stop("Covariates not yet supported for brms")
  }

  if (is.null(moderator) && any(mod.a, mod.b, mod.cprime)) {
    # Give error if paths indicated as moderated, but no moderator name given
    stop("No moderator was specified for the moderated path(s).")
  }

  tmp <- stack_bpg(data, L2ID, X, Y, M,
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

  random.formula <- " + (0 "
  if (random.int.m) {random.formula <- paste(random.formula, "+ Sm")}
  if (random.int.y) {random.formula <- paste(random.formula, "+ Sy")}
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
                         chains = chains,
                         ...))

  # create output list
  out <- list()

  # some error handling, just in case
  if (inherits(mod_med_brms_tmp,"try-error")) {
    out$model <- NULL
    out$conv <- FALSE # boolean or some other code?
  } else {
    out$model <- mod_med_brms_tmp
    out$conv <- TRUE # TODO: CFF would prefer this is not here
  }

  if(returndata) out$data <- tmp

  out$call <- match.call()

  return(out)
}

#' Post-processing of results from modmed.mlm.brms
#'
#' @param brms.obj Result of \code{boot.modmed.mlm}
#' @param type Character indicating which piece of information to extract from the model
#'   "indirect": value of the indirect effect.
#'   "a": Current value of a path.
#'   "b": Current value of b path.
#'   "cprime": Current value of c path.
#'   "covab": Random effect covariance between a and b paths, if both paths have associated random effects.
#'   "indirect.diff": difference in indirect effect at two values of the moderator (set by \code{modval1} and \code{modval2}).
#'   "a.diff": difference in a at two values of the moderator (set by \code{modval1} and \code{modval2}).
#'   "b.diff": difference in b at two values of the moderator (set by \code{modval1} and \code{modval2}).
#'   "cprime.diff": difference cprime at two values of the moderator (set by \code{modval1} and \code{modval2}).
#' @param ci.type Character indicating the type of confidence interval to compute. For now, just "ECI" is supported,
#'   an equal-tailed credible interval.
#' @param ci.conf Numeric value indicating the confidence level for the credibility interval.
#' @param modval1 If enabled, other quantities such as the indirect effect, a, b, and cprime, will be computed
#'   at this particular value of the moderator. Otherwise, value of these quantities is directly extracted from
#'   the model output (i.e., these would represent values of the effects when the moderator = 0).
#' @param modval2 Second value of the moderator at which to compute the indirect effect.
#' @details
#'   This function generally assumes that type="all" was used when initially fitting the model, making all necessary
#'   information available for computation of indirect effects, differences between effects, and so on. If type="all"
#'   was not used, there is no guarantee that credibility intervals for the effects of interest can be extracted.
#' @return A list with two elements:
#' \itemize{
#'  \item{\code{CI} Point estimate (mean and median of posterior), sd, mad, credibility interval (quantiles), and other diagnostic information (rhat, ess_bulk, ess_tail).}
#'  \item{\code{draws} Contains \code{\link[posterior]{draws_matrix}} (from the posterior package) for quantity of interest. i.e., all posterior draws, for which the user may do additional work with.}
#' }
#' @export
#' @examples
#' \donttest{
#' data(BPG06dat)
#'
#' # Note: 2000 iterations is just an example so that run time is not too long.
#' # Pick something larger (e.g., 5000+) in practice
#'
#' # Only fixed effects with random intercept
#' fit<-modmed.mlm.brms(BPG06dat,"id", "x", "y" , "m", cores = 2,
#'                      iter = 2000, control = list(adapt_delta=0.95),
#'                      seed = 1234)
#'
#'
#' res.indirect <- extract.modmed.mlm.brms(fit, "indirect")
#' res.a <- extract.modmed.mlm.brms(fit, "a")
#' res.b <- extract.modmed.mlm.brms(fit, "b")
#' res.cprime <- extract.modmed.mlm.brms(fit, "cprime")
#'
#' # Summary of results is in CI slot, example.
#' # Here, 95% credibility interval is denoted by q2.5 and q97.5
#' res.indirect$CI
#'
#' # Matrix of draws in another slot:
#' res.indirect$draws
#'
#'
#' }
#' @importFrom stats median
#' @importFrom posterior summarise_draws sd mad quantile2 rhat ess_bulk ess_tail
extract.modmed.mlm.brms <- function(brms.obj, type=c("indirect","a","b","cprime","covab",
                                                     "indirect.diff","a.diff","b.diff","cprime.diff"), ci.type=c("ECI"), ci.conf=.95,
                                    modval1 = NULL, modval2 = NULL){
  type <- match.arg(type)
  ci.type <- match.arg(ci.type)

  # guess what options were used for modmed.mlm.brms by extracting from it from call
  args <- as.list(args(modmed.mlm.brms))
  calledargs <- as.list(brms.obj$call)
  args[names(calledargs)] <- calledargs

  # Extract just what we want from the model
  draws <- compute.indirect(brms.obj$model, args=args, type=type, modval1=modval1, modval2=modval2, boot=FALSE)
  colnames(draws) <- type

  probs <- c((1-ci.conf)/2, ci.conf+(1-ci.conf)/2)

  # form CI
  if(ci.type=="ECI"){
    ci <- summarise_draws(draws, mean, median, sd, mad, ~quantile2(.x, prob=probs),
                               rhat, ess_bulk, ess_tail)
  } else if(ci.type=="HPD") {
    stop("HPD not yet supported")
  }

  # output
  out<-list(CI = ci,
            draws = draws)

  return(out)

}
