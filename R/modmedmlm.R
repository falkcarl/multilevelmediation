######################################################################
## Functions for use with bootstrapping
##
## Copyright 2019-2021 Carl F. Falk, Todd Vogel
##
## This program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of
## the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
## <http://www.gnu.org/licenses/>

#' Custom boot function for (moderated) mediation with 2-level multilevel models
#'
#' @param data Data frame in long format.
#' @param indices \code{\link[boot]{boot}} requires the function signature to accept a vector of
#'   index numbers and so this argument is required. If the index numbers are all in order starting at 1,
#'   then the relevant model will be fit to the data without any resampling. If some other vector is supplied,
#'   then resampling is done as described in details.
#' @param L2ID Name of column that contains grouping variable in 'data' (e.g., "SubjectID")
#' @param ... Arguments passed to \code{modmed.mlm} to define the mediation analysis model.
#' @param type Character that defines what information to extract from the model. Default and options are in \code{extract.modmed.mlm}.
#'   As examples, "indirect" will compute the indirect effect, "all" will save all random and fixed effects for possible additional
#'   computations, "indirect.diff" will compute the difference in the indirect effect at two values of a possible moderating variable.
#' @param modval1 (Optional) Numeric. If the model has a moderator, this value will be passed to \code{extract.modmed.mlm}
#'   to compute the indirect effect or other effects at that value. See \code{extract.modmed.mlm} for details.
#' @param modval2 (Optional). If the model has a moderator, it is possible to compute the difference in the indirect
#'   at two values of the moderator. If given and an appropriate option for such a difference is chosen for \code{type},
#'   this value and that of \code{modval1} will be passed to \code{extract.modmed.mlm} to compute and save the difference.
#'   This is useful for obtaining a CI for the difference in the indirect effect at two different levels of the moderator.
#' @param boot.lvl Character that defines at what level resampling should occur. Options are "both", "1", or "2".
#' @details TO DO. Implements custom function to do resampling at level 2, then level 1. For use with boot package.
#'   Capable of doing moderation as well. Need to detail which kinds of moderation, which mediation models (e.g., 1-1-1 only?).
#'   This resamples L2 units, then L1 units within each L2 unit
#' @examples
#' \donttest{
#' ## Mediation for 1-1-1 model
#' data(BPG06dat)
#'
#' # Do bootstrapping... w/ parallel processing
#' # snow appears to work on Windows; something else may be better on Unix/Mac/Linux
#'
#' #library(parallel)
#' #library(boot)
#' #ncpu<-6
#' #cl<-makeCluster(ncpu)
#'
#' # bootstrap just the indirect effect
#' #boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
#' # L2ID = "id", X = "x", Y = "y", M = "m",
#' # random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' # type="indirect",
#' # parallel="snow",ncpus=ncpu,cl=cl)
#'
#' # bootstrap all fixed and random effects (recommended)
#' #boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
#' #   L2ID = "id", X = "x", Y = "y", M = "m",
#' # random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #   type="all",
#' #  parallel="snow",ncpus=ncpu,cl=cl)
#'
#' #stopCluster(cl)
#'
#' #boot.result$t0 # point estimates for everything based on original data
#' #boot.ci(boot.result, index=1, type="perc") # percentile interval of first element
#'
#' # Point estimate and 95% CI for indirect effect
#' #extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' # without cluster
#' # boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=5,
#' #   L2ID = "id", X = "x", Y = "y", M = "m",
#' #  random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #  type="indirect")
#'
#' ## Moderated mediation
#'
#' #data(simdat)
#' #ncpu<-12
#' #cl<-makeCluster(ncpu)
#'
#' # Bootstrap w/ moderation of a and b paths
#'
#' #boot.result2<-boot(simdat, statistic=boot.modmed.mlm, R=10000,
#' # L2ID = "L2id", X = "X", Y = "Y", M = "M",
#' #   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#' #   random.mod.a = TRUE, random.mod.b = TRUE,
#' #   type="all",
#' #   parallel="snow",ncpus=ncpu,cl=cl)
#'
#' #  test<-modmed.mlm(simdat,
#' #  L2ID = "L2id", X = "X", Y = "Y", M = "M",
#' #   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #   moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#' #   random.mod.a = TRUE, random.mod.b = TRUE)
#'
#' # stopCluster(cl)
#'
#' # indirect effect point estimate and 95% CI when moderator = 0
#' #extract.boot.modmed.mlm(boot.result2, type="indirect")
#' #extract.boot.modmed.mlm(boot.result2, type="indirect", modval1=0)
#'
#' # indirect effect point estimate and 95% CI when moderator = 1
#' #extract.boot.modmed.mlm(boot.result2, type="indirect", modval1=1)
#'
#' # indirect effect difference point estimate and 95% CI
#' #extract.boot.modmed.mlm(boot.result2, type="indirect.diff",
#' #   modval1=0, modval2=1)
#'
#' }
#' @export boot.modmed.mlm
boot.modmed.mlm <- function(data, indices, L2ID, ...,
                            type="all", modval1=NULL, modval2=NULL,
                            boot.lvl = c("both","1","2")) {

  # TODO use only default type="all". Otherwise conflicts with extract.boot.modmed.mlm possible (TV: fixed? changed default to "all")

  boot.lvl <- match.arg(boot.lvl)

  # ad-hoc check if this is first run of analysis by comparing to indices
  if (all(indices == (1:nrow(data)))) {
    # do nothing
    rdat <- data[indices, ]
  } else {
    # manually apply case-wise resampling

    if (boot.lvl == "both") {
      # Resample at L2 and then L1 within each L2 unit
      # Resample L2 units
      L2 <- unique(data[, L2ID])
      N <- length(L2)
      L2_indices <- sample(L2, N, replace = TRUE)
      # Resample L1 units
      rdat <- lapply(L2_indices, function(x) {
        L2_sub <- data[data[, L2ID] == x, , drop = FALSE] # in case there is only 1 obs
        n_j <- nrow(L2_sub)
        L1_idx <- sample(1:n_j, n_j, replace = TRUE)
        L2_sub <- L2_sub[L1_idx, ]
      })
      rdat <- do.call("rbind", rdat)

    } else if (boot.lvl == "2") {
      # Resample L2 units
      L2 <- unique(data[, L2ID])
      N <- length(L2)
      L2_indices <- sample(L2, N, replace = TRUE)
      rdat <- lapply(L2_indices, function(x) {
        L2_sub <- data[data[, L2ID] == x, , drop = FALSE] # in case there is only 1 obs
      })
      rdat <- do.call("rbind", rdat)

    } else if (boot.lvl == "1") {
      # Resample L1 units (use given indices from boot function)
      rdat <- data[indices, ]
    }

    row.names(rdat) <- NULL
  }

  result<-modmed.mlm(rdat,L2ID,...)

  return(extract.modmed.mlm(result,type=type,modval1=modval1,modval2=modval2))
}


#' Custom model fitting function for two-level (moderated) mediation
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
#' @param random.covars.m (Logical vector) Add random slopes for covariates on M?
#' @param random.covars.y (Logical vector) Add random slopes for covariates on Y?
#' @param method Argument passed to \code{\link[nlme]{lme}} to control estimation method.
#' @param control Argument passed to \code{\link[nlme]{lme}} that controls other estimation options.
#' @param returndata (Logical) Whether to save restructured data in its own slot. Note: nlme may do this automatically. Defaults to \code{FALSE}.
#' @details TO DO. Implements custom function to do moderated mediation with multilevel models.
#'   Capable of doing moderation as well. Need to detail which kinds of moderation. Believed that it currently supports 2-1-1, 2-2-1, 1-1-1
#'   with moderator at either level and moderator and any paths can have indirect effects.
#'   Initially implemented for the BPG06 model for 1-1-1 mediation with moderation...
#' @examples
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
#'
#'
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
#'
#' #TODO: move this to testing file to ensure package does not break
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
#' }
#' @import nlme
#' @importFrom matrixcalc vech
#' @importFrom MCMCpack xpnd
#' @importFrom stats as.formula
#' @export modmed.mlm
modmed.mlm<-function(data, L2ID, X, Y, M,
                     moderator = NULL, mod.a = FALSE, mod.b = FALSE, mod.cprime = FALSE,
                     covars.m = NULL, covars.y = NULL,
                     random.a = FALSE, random.b = FALSE, random.cprime = FALSE,
                     random.mod.a = FALSE, random.mod.b = FALSE, random.mod.cprime = FALSE,
                     random.mod.m = FALSE, random.mod.y = FALSE,
                     random.covars.m = NULL, random.covars.y = NULL,
                     method="REML", control = lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
                                                         msMaxEval = 10000, tolerance = 1e-6),
                     returndata = FALSE){

  # Some input checking per Todd's code:
  # TV: These input checks will happen at every loop of the bootstrap.
  # TV: Is there a more efficient way? Or is the slowdown negligible?
  # CFF: probably not that slow due to this; restructuring the data might slow things down, but
  #  avoiding that might be tricky to do while bootstrapping

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

  # Add any covariates to the paths if necessary
  #TV: does the order of the variables matter for lme? Here the covariates are after the mod interactions in the formula
  if (!is.null(covars.m)) {
    covars.m_formula <-  paste0("+ Sm:", covars.m, collapse=" ") #write the formula for each covar specified
    fixed.formula <- paste(fixed.formula, covars.m_formula)      #and add to main fixed fx formula
  }
  if (!is.null(covars.y)) {
    covars.y_formula <-  paste0("+ Sy:", covars.y, collapse=" ") #write the formula for each covar specified
    fixed.formula <- paste(fixed.formula, covars.y_formula)      #and add to main fixed fx formula
  }

  # Create the formula for the random effects
  random.formula <- "~ 0 + Sm + Sy"
  if (random.a == TRUE) {random.formula <- paste(random.formula, "+ SmX")}
  if (random.b == TRUE) {random.formula <- paste(random.formula, "+ SyM")}
  if (random.cprime == TRUE) {random.formula <- paste(random.formula, "+ SyX")}

  # Add random effects for moderator here, if any
  if(random.mod.a && mod.a){random.formula <- paste(random.formula, "+ SmX:W")}
  if(random.mod.b && mod.b){random.formula <- paste(random.formula, "+ SyM:W")}
  if(random.mod.cprime && mod.cprime){random.formula <- paste(random.formula, "+ SyX:W")}
  if(random.mod.m && mod.a){random.formula <- paste(random.formula, "+ Sm:W")}
  if(random.mod.y && mod.b){random.formula <- paste(random.formula, "+ Sy:W")}
  #TV: would there ever be a situation where Sm or Sy would be moderated, but not their paths? (eg SmX, SyM, etc)
  #TV: eg, would random.mod.m ever be true if random.mod.a was not true?

  # Add random effects for covariates here, if any
  #TODO: TV: currently doesn't check whether random covariates have the same
  #name as other variables in the model, or are the same covariates in the fixed fx formula.
  # Is possible to specify random covars not in the fixed formula, but may give an error with lme (although can be implemented, would just have to save random covar to the data above)
  if (!is.null(random.covars.m)) {
    random.covars.m_formula <- paste0("+ Sm:", random.covars.m, collapse=" ")
    random.formula <- paste(random.formula, random.covars.m_formula)
  }
  if (!is.null(random.covars.y)) {
    random.covars.y_formula <- paste0("+ Sy:", random.covars.y, collapse=" ")
    random.formula <- paste(random.formula, random.covars.y_formula)
  }

  # Add in the grouping variable after all the variables are entered
  random.formula <- paste(random.formula, "| L2id")


  # Run the model through nlme
  mod_med_tmp <- try(lme(fixed = as.formula(fixed.formula), # fixed effects
                         random = as.formula(random.formula), # random effects
                         weights = varIdent(form = ~ 1 | Sm), # heteroskedasticity
                         data = tmp,
                         method = method,
                         control = control))

  # create output list
  out <- list()

  # some error handling, just in case
  if (class(mod_med_tmp) == "try-error") {
    out$model <- NULL
    out$conv <- FALSE # boolean or some other code?
  } else {
    out$model <- mod_med_tmp
    out$conv <- TRUE
  }

  if(returndata) out$data <- tmp

  out$call <- match.call()

  return(out)

}

#' Post-processing of a model fit with modmed.mlm
#'
#' @param fit Result of \code{modmed.mlm}.
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
#'   For any of the .diff values, these are always the value of the effect at modval1 minus modval2.
#' @export extract.modmed.mlm
extract.modmed.mlm <- function(fit, type=c("all","fixef","recov","recov.vec","indirect","a","b","cprime","covab",
                                           "indirect.diff","a.diff","b.diff","cprime.diff"),
                               modval1 = NULL, modval2 = NULL){

  type <- match.arg(type)

  if(!fit$conv || is.null(fit$model)){

    # If model fitting was a problem
    # Depending on type and fit$call, it is possible to guess the length of output here
    # This is a bit tenuous if support for more variables changes, however

    # https://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-function-in-r-with-names
    # FIXME: this could get broken if we do more than just 2 level bootstrapping w/ boot package
    # Called directly, or from boot?
    if(sys.nframe()>2){
      funcname<-match.call(definition=sys.function(-2),call=sys.call(-2))[[1]]
      if(as.character(funcname) == "boot"){
        # from boot
        calledargs<-match.call(definition=sys.function(-2),call=sys.call(-2))
      } else {
        stop("Can't detect calling arguments for extract.modmed.mlm")
      }
    } else {
      # directly
      calledargs <- as.list(fit$call)
    }

    # Grab the argument values that differ from default
    args <- as.list(args(modmed.mlm))
    args[names(calledargs)] <- calledargs

    # Grab if a, b, c paths were moderated
    moda <- unlist(args[grepl("^mod\\.a",names(args))])
    modb <- unlist(args[grepl("^mod\\.b",names(args))])
    modc <- unlist(args[grepl("^mod\\.c",names(args))])

    # Grab number of covariates for m and y outcomes
    cova = length(eval(args$covars.m))
    covb = length(eval(args$covars.y))

    nfixefa <- 2 + ifelse(any(moda),2,0) + cova # number of fixed effects first model
    nfixefb <- 3 + ifelse(any(modb)||any(modc),1,0) + any(modb) + any(modc) + covb #number of fixed effects second model
    nfex <- nfixefa + nfixefb # combined

    # re due to 2 random intercepts + abc paths
    #nre <- 2 + sum(unlist(args[grepl("^random\\.[abc]$",names(args))]))
    nre <- 2 + sum(unlist(args[grepl("^random\\.([ab]|cprime)$",names(args))]))

    # re due to moderator effects
    nre <- nre + sum(unlist(args[grepl("^random\\.mod\\.([abym]|cprime)$",names(args))]))
    nre <- nre*nre # duplicates not yet removed

    out <- vector("numeric")

    if(type=="indirect"||type=="a"||type=="b"||type=="cprime"||type=="covab"||type=="indirect.diff"||
       type=="a.diff"||type=="b.diff"||type=="cprime.diff"){
      out <- NA
    } else if (type=="all"){
      out <- rep(NA, nfex + nre)
    } else if (type=="fixef"){
      out <- rep(NA, nfex)
    } else if (type=="recov"){
      out <- matrix(NA,nre,nre)
    } else if (type=="recov.vec"){
      out <- rep(NA, nre)
    }
  } else {

    # computations if the model is ok

    # get original arguments
    args<-as.list(fit$call)

    # extract var-cov matrix among random effects
    vc <- VarCorr(fit$model)

    #grab names of random effects
    re.names <- colnames(fit$model[["coefficients"]][["random"]][["L2id"]])
    re.num <- length(re.names) #number of random effects

    # create cov matrix among random effects
    sd <- as.numeric(vc[1:re.num, 2])
    sigma <- cbind(vc[1:re.num, 3:ncol(vc)], 1)
    diag(sigma) <- 1
    sig <- as.numeric(vech(sigma))
    sig <- xpnd(sig)
    D <- diag(sd)
    sig2 <- D %*% sig %*% D
    colnames(sig2) <- rownames(sig2) <- re.names

    # rand effects as vector
    sig2vec <- as.vector(sig2)
    elementnames <- expand.grid(re.names,re.names)
    elementnames <- paste0("re.",elementnames[,1],elementnames[,2])
    names(sig2vec) <- elementnames

    # remove duplicates ?
    #select <- as.vector(lower.tri(sig2, diag=T))
    #sig2vec <- sig2vec[select]

    # extract fixed effects
    fixed <- fixef(fit$model)

    # generate output, computing other stuff as necessary
    all <- c(fixed, sig2vec)
    if(type == "all"){
      out <- all
    } else if (type=="fixef"){
      out <- fixed
    } else if (type=="recov"){
      out <- sig2
    } else if (type=="recov.vec"){
      out <- sig2vec
    } else {
      out <- compute.indirect(all,args=args,type=type,modval1 = modval1, modval2 = modval2)
    }
  }
  out
}

#' Post-processing of bootstrap results from boot.modmed.mlm
#'
#' @param boot.obj Result of \code{\link{boot::boot}} using \code{boot.modmed.mlm}
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
#'   This function generally assumes that type="all" was used when initially fitting the model, making all necessary
#'   information available for computation of indirect effects, differences between effects, and so on. If type="all"
#'   was not used, there is no guarantee that confidence intervals for the effects of interest can be extracted.
#' @export extract.boot.modmed.mlm
#' @examples
#' \donttest{
#'
#' }
#' @importFrom stats quantile
extract.boot.modmed.mlm <- function(boot.obj, type=c("indirect","a","b","cprime","covab",
                                           "indirect.diff","a.diff","b.diff","cprime.diff"), ci.type="perc", ci.conf=.95,
                               modval1 = NULL, modval2 = NULL){
  type <- match.arg(type)
  ci.type <- match.arg(ci.type)

  # guess what options were used for modmed.mlm by extracting from it and from boot.modmed.mlm call
  args <- as.list(args(modmed.mlm))
  calledargs <- as.list(boot.obj$call)
  args[names(calledargs)] <- calledargs

  # Extract point estimate from model
  point.est <- compute.indirect(boot.obj$t0, args=args, type=type, modval1=modval1, modval2=modval2)

  # Obtain vector of estimates for effect of interest
  est <-apply(boot.obj$t, 1, function(x){
    names(x) <- names(boot.obj$t0)
    compute.indirect(x,args=args, type=type, modval1=modval1, modval2=modval2)
  })

  # form CI
  probs <- c((1-ci.conf)/2, ci.conf+(1-ci.conf)/2)
  ci <- quantile(est, probs=probs, na.rm=TRUE)

  # output
  out<-list(CI = ci,
            est = point.est)

  return(out)

}


# Compute stuff from a vector of fixed effects and random effects
compute.indirect <- function(v, args,
                             type=c("indirect","a","b","cprime","covab","indirect.diff","a.diff","b.diff","cprime.diff"),
                             modval1 = NULL, modval2 = NULL){

  # TODO: need some input checking here. e.g., .diff isn't relevant unless both modval1 and modval2 are specified
  # And these would not work unless relevant moderation effects are actually estimated mod.a, mod.b, mod.cprime

  a1 <- a2 <- v["SmX"]
  b1 <- b2 <- v["SyM"]
  cprime1 <- cprime2 <- v["SyX"]

  # If moderation effects, modify a and b
  if(!is.null(args$mod.a) && args$mod.a && !is.null(modval1)){
    a1 <- a1 + v["SmX:W"]*modval1
  }
  if(!is.null(args$mod.a) && args$mod.a && !is.null(modval2)){
    a2 <- a2 + v["SmX:W"]*modval2
  }
  if(!is.null(args$mod.b) && args$mod.b && !is.null(modval1)){
    b1 <- b1 + v["SyM:W"]*modval1
  }
  if(!is.null(args$mod.b) && args$mod.b && !is.null(modval2)){
    b2 <- b2 + v["SyM:W"]*modval2
  }
  if(!is.null(args$mod.cprime) && args$mod.cprime && !is.null(modval1)){
    cprime1 <- cprime1 + v["SyX:W"]*modval1
  }
  if(!is.null(args$mod.cprime) && args$mod.cprime && !is.null(modval2)){
    cprime2 <- cprime2 + v["SyX:W"]*modval2
  }

  # compute indirect effect using only fixed effects
  ab1 <- a1*b1
  if(!is.null(modval2)){ ab2 <-a2*b2 }

  # additional adjustments to indirect effect from random effects

  # cov among a and b paths
  if(!is.null(args$random.a) && !is.null(args$random.b) && args$random.a && args$random.b){
    covab <- v["re.SmXSyM"]
    ab1 <- ab1 + covab
    if(!is.null(modval2)){ ab2 <- ab2 + covab }
  }

  # random effects in case interaction term has random effects
  # TODO: Add options so that these random effects could also be returned?
  #     Mostly for debugging purposes I suppose
  if(!is.null(modval1)){
    if(!is.null(args$random.b) && !is.null(args$mod.a) && !is.null(args$random.mod.a) &&
       args$random.b && args$mod.a && args$random.mod.a){
      ab1 <- ab1 + modval1 * v["re.SyMSmX:W"] # times covariance between re.b and re.mod.a
    }
    if(!is.null(args$random.a) && !is.null(args$mod.b) && !is.null(args$random.mod.b) &&
       args$random.a && args$mod.b && args$random.mod.b){
      ab1 <- ab1 + modval1 * v["re.SmXSyM:W"] # times covariance between re.a and re.mod.b
    }
    if(!is.null(args$random.mod.a) && !is.null(args$random.mod.b) &&
       args$random.mod.a && args$random.mod.b){
      ab1 <- ab1 + (modval1^2) * v["re.SmX:WSyM:W"] # times covariance between re.mod.a and re.mod.b
    }
  }
  if(!is.null(modval2)){
    if(!is.null(args$random.b) && !is.null(args$mod.a) && !is.null(args$random.mod.a) &&
       args$random.b && args$mod.a && args$random.mod.a){
      ab2 <- ab2 + modval2 * v["re.SyMSmX:W"] # times covariance between re.b and re.mod.a
    }
    if(!is.null(args$random.a) && !is.null(args$mod.b) && !is.null(args$random.mod.b) &&
       args$random.a && args$mod.b && args$random.mod.b){
      ab2 <- ab2 + modval2 *  v["re.SmXSyM:W"] # times covariance between re.a and re.mod.b
    }
    if(!is.null(args$random.mod.a) && !is.null(args$random.mod.b) &&
       args$random.mod.a && args$random.mod.b){
      ab2 <- ab2 + (modval2^2) * v["re.SmX:WSyM:W"] # times covariance between re.mod.a and re.mod.b
    }
  }

  if(type=="indirect"){
    out <- ab1
  } else if (type=="a"){
    out <- a1
  } else if (type=="b"){
    out <- b1
  } else if (type=="cprime"){
    out <- cprime1
  } else if (type=="covab"){
    out <- covab
  } else if (type=="indirect.diff"){
    out <- ab1 - ab2
  } else if (type=="a.diff"){
    out <- a1 - a2
  } else if (type=="b.diff"){
    out <- b1 - b2
  } else if (type=="cprime.diff"){
    out <- cprime1 - cprime2
  }

  names(out) <- NULL
  out
}

