#' Post-processing of a model fit with modmed.mlm
#'
#' @param fit Result of \code{\link{modmed.mlm}}.
#' @param type Character indicating which piece of information to extract from the model
#'   "all": fixed effects and var-cov matrix of random effects, as a single vector.
#'   "fixef": just fixed effects.
#'   "recov": var-cov matrix of random effects, as a matrix.
#'   "recov.vec": var-cov matrix of random effects, as a stacked vector.
#'   "indirect": value of the indirect effect.
#'   "a": Current value of a path.
#'   "b": Current value of b path.
#'   "cprime": Current value of c path.
#'   "covab": Random effect covariance between a and b paths, if both paths have associated random effects.
#'   "indirect.diff": difference in indirect effect at two values of the moderator (set by \code{modval1} and \code{modval2}).
#'   "a.diff": difference in a at two values of the moderator (set by \code{modval1} and \code{modval2}).
#'   "b.diff": difference in b at two values of the moderator (set by \code{modval1} and \code{modval2}).
#'   "cprime.diff": difference cprime at two values of the moderator (set by \code{modval1} and \code{modval2}).
#' @param modval1 If enabled, other quantities such as the indirect effect, a, b, and cprime, will be computed
#'   at this particular value of the moderator. Otherwise, value of these quantities is directly extracted from
#'   the model output (i.e., these would represent values of the effects when the moderator = 0).
#' @param modval2 Second value of the moderator at which to compute the indirect effect.
#' @details
#'   This function extracts relevant parameter estimates from models estimated using \code{\link{modmed.mlm}}.
#'   For any of the .diff values, these are always the value of the effect at modval1 minus modval2.
#' @return A vector or single numeric value corresponding to the parameter estimate(s) of interest is returned.
#' @examples
#' \donttest{
#' # Example data for 1-1-1 w/o moderation
#' data(BPG06dat)
#'
#' # Fit model
#' fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE)
#' extract.modmed.mlm(fit, type="indirect")
#' }
#'
#' @export extract.modmed.mlm
extract.modmed.mlm <- function(
  fit,
  type = c(
    "all",
    "fixef",
    "recov",
    "recov.vec",
    "indirect",
    "a",
    "b",
    "cprime",
    "covab",
    "indirect.diff",
    "a.diff",
    "b.diff",
    "cprime.diff"
  ),
  modval1 = NULL,
  modval2 = NULL
) {
  type <- match.arg(type)
  mod_args <- fit$args

  if (!fit$conv || is.null(fit$model)) {
    # If model fitting was a problem
    # Depending on type and fit$args, it is possible to guess the length of output here
    # This is a bit tenuous if support for more variables changes, however

    # Grab if a, b, c paths were moderated
    moda <- unlist(mod_args[grepl("^mod\\.a", names(mod_args))])
    modb <- unlist(mod_args[grepl("^mod\\.b", names(mod_args))])
    modc <- unlist(mod_args[grepl("^mod\\.c", names(mod_args))])

    # Grab number of covariates for m and y outcomes
    cova <- length(eval(mod_args$covars.m))
    covb <- length(eval(mod_args$covars.y))

    nfixefa <- 2 + ifelse(any(moda), 2, 0) + cova # number of fixed effects first model
    nfixefb <- 3 + ifelse(any(modb) || any(modc), 1, 0) + any(modb) + any(modc) + covb #number of fixed effects second model
    nfex <- nfixefa + nfixefb # combined

    # re due to 2 random intercepts
    nre <- 0
    nre <- nre + sum(unlist(mod_args[grepl("^random\\.int\\.([my])$", names(mod_args))]))

    # re due to abc paths
    nre <- nre + sum(unlist(mod_args[grepl("^random\\.([ab]|cprime)$", names(mod_args))]))

    # re due to covariates
    nre <- nre + length(unlist(mod_args[grepl("^random\\.covars\\.([my])$", names(mod_args))]))

    # re due to moderator effects
    nre <- nre + sum(unlist(mod_args[grepl("^random\\.mod\\.([abym]|cprime)$", names(mod_args))]))

    nre <- nre * nre # duplicates not yet removed

    out <- vector("numeric")

    if (type %in% c("indirect", "a", "b", "cprime", "covab", "indirect.diff", "a.diff", "b.diff", "cprime.diff")) {
      out <- NA
    } else if (type == "all") {
      out <- rep(NA, nfex + nre)
    } else if (type == "fixef") {
      out <- rep(NA, nfex)
    } else if (type == "recov") {
      out <- matrix(NA, nre, nre)
    } else if (type == "recov.vec") {
      out <- rep(NA, nre)
    }
  } else {
    # computations if the model is ok

    # get original arguments
    #args<-as.list(fit$call)

    # extract random effects
    re <- randef.lme(fit$model)
    sig2 <- re$sig2
    sig2vec <- re$sig2vec

    # remove duplicates ?
    #select <- as.vector(lower.tri(sig2, diag=T))
    #sig2vec <- sig2vec[select]

    # extract fixed effects
    if (inherits(fit$model, "glmmTMB")) {
      fixed <- fixef(fit$model)$cond
    } else {
      fixed <- fixef(fit$model)
    }

    # generate output, computing other stuff as necessary
    all <- c(fixed, sig2vec)
    if (type == "all") {
      out <- all
    } else if (type == "fixef") {
      out <- fixed
    } else if (type == "recov") {
      out <- sig2
    } else if (type == "recov.vec") {
      out <- sig2vec
    } else {
      out <- compute.indirect(all, args = mod_args, type = type, modval1 = modval1, modval2 = modval2)
    }
  }
  out
}

randef.lme <- function(model) {
  if (inherits(model, "glmmTMB")) {
    sig2 <- VarCorr(model)$cond$L2id
    attr(sig2, "stdev") <- NULL
    attr(sig2, "correlation") <- NULL
  } else {
    # extract var-cov matrix among random effects
    sig2 <- getVarCov(model)
    class(sig2) <- "matrix"
    attr(sig2, "group.levels") <- NULL
  }

  re.names <- colnames(sig2)

  # rand effects as vector
  sig2vec <- as.vector(sig2)
  elementnames <- expand.grid(re.names, re.names)
  elementnames <- paste0("re.", elementnames[, 1], elementnames[, 2])
  names(sig2vec) <- elementnames

  out <- list(sig2 = sig2, sig2vec = sig2vec)

  return(out)
}

#' Post-processing of bootstrap results from boot.modmed.mlm
#'
#' @param boot.obj Result of \code{boot} using \code{\link{boot.modmed.mlm}}
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
#' @param ci.type Character indicating the type of confidence interval to compute.
#'   Currently only percentile confidence intervals are supported with "perc".
#' @param ci.conf Numeric value indicating the confidence level for the interval.
#' @param modval1 If enabled, other quantities such as the indirect effect, a, b, and cprime, will be computed
#'   at this particular value of the moderator. Otherwise, value of these quantities is directly extracted from
#'   the model output (i.e., these would represent values of the effects when the moderator = 0).
#' @param modval2 Second value of the moderator at which to compute the indirect effect.
#' @details
#'   This is a convenience function that computes point estimates and confidence intervals from multilevel mediation
#'   analysis models where \code{\link{boot.modmed.mlm}} was used along with the \code{boot} package, or \code{\link{bootresid.modmed.mlm}}
#'   was used. This function generally assumes that type="all" was used when initially fitting the model, making all necessary
#'   information available for computation of indirect effects, differences between effects, and so on. If type="all"
#'   was not used, there is no guarantee that confidence intervals for the effects of interest can be extracted.
#' @return A list with the following elements:
#' \itemize{
#'  \item{\code{CI} A vector, typically two elements, that has the lower and upper endpoints requested confidence interval
#'  for the quantity requested by \code{type}.}
#'  \item{\code{est} Point estimate for the quantity requested by \code{type}.}
#' }
#' @export extract.boot.modmed.mlm
#' @examples
#' \donttest{
#' ## Mediation for 1-1-1 model
#' library(boot)
#'
#' data(BPG06dat)
#'
#' set.seed(1234)
#'
#' # Note that R should be be MUCH larger than the value used here (e.g., 1000 or
#' # larger). A small number is chosen just so examples run relatively fast when
#' # tested.
#'
#' # bootstrap all fixed and random effects
#' boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=5,
#'    L2ID = "id", X = "x", Y = "y", M = "m",
#'    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'    type="all",
#'    control=list(opt="nlm"))
#'
#' # Point estimate and 95% CI for indirect effect
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=0.95)
#'
#' }
#' @importFrom stats quantile
extract.boot.modmed.mlm <- function(
  boot.obj,
  type = c("indirect", "a", "b", "cprime", "covab", "indirect.diff", "a.diff", "b.diff", "cprime.diff"),
  ci.type = "perc",
  ci.conf = 0.95,
  modval1 = NULL,
  modval2 = NULL
) {
  type <- match.arg(type)
  ci.type <- match.arg(ci.type)

  # match.call is tricky; could break if this is inside a function?
  # guess what options were used for modmed.mlm by extracting from it and from boot.modmed.mlm call
  args <- as.list(args(modmed.mlm))
  calledargs <- as.list(boot.obj$call)
  args[names(calledargs)] <- calledargs

  # Extract point estimate from model
  point.est <- compute.indirect(boot.obj$t0, args = args, type = type, modval1 = modval1, modval2 = modval2)

  # Obtain vector of estimates for effect of interest
  est <- apply(boot.obj$t, 1, function(x) {
    names(x) <- names(boot.obj$t0)
    compute.indirect(x, args = args, type = type, modval1 = modval1, modval2 = modval2)
  })

  # form CI
  if (ci.type == "perc") {
    probs <- c((1 - ci.conf) / 2, ci.conf + (1 - ci.conf) / 2)
    ci <- quantile(est, probs = probs, na.rm = TRUE)
  }

  # output
  out <- list(CI = ci, est = point.est)

  return(out)
}


# Compute stuff from a vector of fixed effects and random effects
# Toggling b/w boot and brms a bit ad-hoc for now.
# At least best to have all computations in the same place

#' @importFrom brms as_draws_matrix
compute.indirect <- function(
  v,
  args, #TODO: change this to mod_args to avoid overloading base variable name? (although creates bw compatibility issue?)
  type = c("indirect", "a", "b", "cprime", "covab", "indirect.diff", "a.diff", "b.diff", "cprime.diff"),
  modval1 = NULL, #FIXME: better to set these to zero instead of NULL? (since should do the same thing?)
  modval2 = NULL,
  boot = TRUE
) {
  # TODO: need some input checking here. e.g., .diff isn't relevant unless both modval1 and modval2 are specified
  # (And these would not work unless relevant moderation effects are actually estimated mod.a, mod.b, mod.cprime)
  # TODO: raise error sooner if type=="covab" and random.a and random.b are not both TRUE
  # TODO: raise error sooner if type=="indirect.diff" and mod.a and mod.b are not both TRUE

  if (boot) {
    a1 <- a2 <- v["SmX"]
    b1 <- b2 <- v["SyM"]
    cprime1 <- cprime2 <- v["SyX"]
  } else {
    a1 <- a2 <- as_draws_matrix(v, "b_SmX")
    b1 <- b2 <- as_draws_matrix(v, "b_SyM")
    cprime1 <- cprime2 <- as_draws_matrix(v, "b_SyX")
  }

  # If moderation effects, modify a and b and cprime as needed
  #TODO: Should add checking/throw errors for modval inputs? Eg if they are the same or are NA or something
  if (!is.null(args$mod.a) && args$mod.a && !is.null(modval1)) {
    if (boot) {
      a1 <- a1 + v["SmX:W"] * modval1
    } else {
      a1 <- a1 + as_draws_matrix(v, "b_SmX:W") * modval1
    }
  }
  if (!is.null(args$mod.a) && args$mod.a && !is.null(modval2)) {
    if (boot) {
      a2 <- a2 + v["SmX:W"] * modval2
    } else {
      a2 <- a2 + as_draws_matrix(v, "b_SmX:W") * modval2
    }
  }
  if (!is.null(args$mod.b) && args$mod.b && !is.null(modval1)) {
    if (boot) {
      b1 <- b1 + v["SyM:W"] * modval1
    } else {
      b1 <- b1 + as_draws_matrix(v, "b_SyM:W") * modval1
    }
  }
  if (!is.null(args$mod.b) && args$mod.b && !is.null(modval2)) {
    if (boot) {
      b2 <- b2 + v["SyM:W"] * modval2
    } else {
      b2 <- b2 + as_draws_matrix(v, "b_SyM:W") * modval2
    }
  }
  if (!is.null(args$mod.cprime) && args$mod.cprime && !is.null(modval1)) {
    if (boot) {
      cprime1 <- cprime1 + v["SyX:W"] * modval1
    } else {
      cprime1 <- cprime1 + as_draws_matrix(v, "b_SyX:W") * modval1
    }
  }
  if (!is.null(args$mod.cprime) && args$mod.cprime && !is.null(modval2)) {
    if (boot) {
      cprime2 <- cprime2 + v["SyX:W"] * modval2
    } else {
      cprime2 <- cprime2 + as_draws_matrix(v, "b_SyX:W") * modval2
    }
  }

  # compute indirect effect using only fixed effects
  ab1 <- a1 * b1
  if (!is.null(modval2)) {
    ab2 <- a2 * b2
  }

  # additional adjustments to indirect effect from random effects

  # cov among a and b paths
  # FIXME: are these redundant? can just check if random.a and/or random.b are TRUE??
  if (!is.null(args$random.a) && !is.null(args$random.b) && args$random.a && args$random.b) {
    if (boot) {
      covab <- v["re.SmXSyM"]
    } else {
      corab <- as_draws_matrix(v, "cor_L2id__SmX__SyM")
      sda <- as_draws_matrix(v, "sd_L2id__SmX")
      sdb <- as_draws_matrix(v, "sd_L2id__SyM")
      covab <- corab * sda * sdb
    }
    ab1 <- ab1 + covab
    if (!is.null(modval2)) {
      ab2 <- ab2 + covab
    }
  }

  # random effects in case interaction term has random effects
  if (!is.null(modval1)) {
    # FIXME: if random.b is always true or false, do we need is.null()?
    if (
      !is.null(args$random.b) &&
        !is.null(args$mod.a) &&
        !is.null(args$random.mod.a) &&
        args$random.b &&
        args$mod.a &&
        args$random.mod.a
    ) {
      if (boot) {
        ab1 <- ab1 + modval1 * v["re.SyMSmX:W"] # times covariance between re.b and re.mod.a
      } else {
        # correl bw/ re.b and re.mod.a
        # TODO: here and elsewhere, is this specific to brms code? maybe safer to put in `elseif`, rather than just `else`
        correl <- as_draws_matrix(v, "cor_L2id__SyM__SmX:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SyM") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        covre <- correl * sd1 * sd2
        ab1 <- ab1 + modval1 * covre
      }
    }

    if (
      !is.null(args$random.a) &&
        !is.null(args$mod.b) &&
        !is.null(args$random.mod.b) &&
        args$random.a &&
        args$mod.b &&
        args$random.mod.b
    ) {
      if (boot) {
        ab1 <- ab1 + modval1 * v["re.SmXSyM:W"] # times covariance between re.a and re.mod.b
      } else {
        # correl between re.a and re.modm.b
        correl <- as_draws_matrix(v, "cor_L2id__SmX__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl * sd1 * sd2
        ab1 <- ab1 + modval1 * covre
      }
    }
    if (!is.null(args$random.mod.a) && !is.null(args$random.mod.b) && args$random.mod.a && args$random.mod.b) {
      if (boot) {
        ab1 <- ab1 + (modval1^2) * v["re.SmX:WSyM:W"] # times covariance between re.mod.a and re.mod.b
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SmX:W__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl * sd1 * sd2
        ab1 <- ab1 + (modval1^2) * covre
      }
    }
  }
  if (!is.null(modval2)) {
    # FIXME: do we need !is.null()? if it's null, then its false?
    #if (!is.null(args$random.b) && !is.null(args$mod.a) && !is.null(args$random.mod.a) && args$random.b && args$mod.a && args$random.mod.a) {
    if (args$random.b && args$mod.a && args$random.mod.a) {
      if (boot) {
        ab2 <- ab2 + modval2 * v["re.SyMSmX:W"] # times covariance between re.b and re.mod.a
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SyM__SmX:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SyM") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        covre <- correl * sd1 * sd2
        ab2 <- ab2 + modval2 * covre
      }
    }
    # if (
    #   !is.null(args$random.a) &&
    #     !is.null(args$mod.b) &&
    #     !is.null(args$random.mod.b) &&
    #     args$random.a &&
    #     args$mod.b &&
    #     args$random.mod.b
    # ) {
    if (args$random.a && args$mod.b && args$random.mod.b) {
      if (boot) {
        ab2 <- ab2 + modval2 * v["re.SmXSyM:W"] # times covariance between re.a and re.mod.b
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SmX__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl * sd1 * sd2
        ab2 <- ab2 + modval2 * covre
      }
    }
    if (!is.null(args$random.mod.a) && !is.null(args$random.mod.b) && args$random.mod.a && args$random.mod.b) {
      if (boot) {
        ab2 <- ab2 + (modval2^2) * v["re.SmX:WSyM:W"] # times covariance between re.mod.a and re.mod.b
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SmX:W__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl * sd1 * sd2
        ab2 <- ab2 + (modval2^2) * covre
      }
    }
  }

  if (type == "indirect") {
    out <- ab1
  } else if (type == "a") {
    out <- a1
  } else if (type == "b") {
    out <- b1
  } else if (type == "cprime") {
    out <- cprime1
  } else if (type == "covab") {
    out <- covab
  } else if (type == "indirect.diff") {
    out <- ab1 - ab2
  } else if (type == "a.diff") {
    out <- a1 - a2
  } else if (type == "b.diff") {
    out <- b1 - b2
  } else if (type == "cprime.diff") {
    out <- cprime1 - cprime2
  }

  names(out) <- NULL
  out
}
