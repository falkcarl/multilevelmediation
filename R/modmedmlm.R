######################################################################
## Functions for use with bootstrapping
##
## Copyright 2019-2020 Carl F. Falk, Todd Vogel
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
#' @param ... Arguments passed to modmed.mlm
#' @details TO DO. Implements custom function to do resampling at level 2, then level 1. For use with boot package.
#'   Capable of doing moderation as well. Need to detail which kinds of moderation, which mediation models (e.g., 1-1-1 only?).
#'   This resamples L2 units, then L1 units within each L2 unit
#' @examples
#' \donttest{
#' # Mediation for 1-1-1 model w/o moderation
#' data(BPG06dat)
#'
#' # Do bootstrapping... w/ parallel processing
#' library(parallel)
#' library(boot)
#' ncpu<-4
#' cl<-makeCluster(ncpu)
#'
#' boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
#'   L2ID = "id", X = "x", Y = "y", M = "m",
#'   random.a=TRUE, random.b=TRUE, random.c=TRUE,
#'   parallel="snow",ncpus=ncpu,cl=cl)
#'
#' stopCluster(cl)
#'
#' # without cluster
#' # boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=5,
#' #   L2ID = "id", X = "x", Y = "y", M = "m",
#'   random.a=TRUE, random.b=TRUE, random.c=TRUE)
#'
#' stopCluster(cl)
#'
#' boot.result$t0 # point estimates for everything based on original data
#'
#' boot.ci(boot.result, index=1, type="perc") # percentile interval
#'
#' # snow appears to work on Windows; something else may be better on Unix/Mac/Linux
#'
#' # need code to look at boot results
#'
#' }
#' @export
boot.modmed.mlm <- function(data, indices, L2ID, ...) {

  # ad-hoc check if this is first run of analysis by comparing to indices
  if (all(indices == (1:nrow(data)))) {
    # do nothing
    rdat <- data[indices, ]
  } else {
    # manually apply case-wise resampling
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
      L2_sub
    })

    rdat <- do.call("rbind", rdat)
    row.names(rdat) <- NULL
  }

  result<-modmed.mlm(rdat,L2ID,...)

  return(result$pars)
}


#' Custom model fitting function for two-level (moderated) mediation
#'
#' @param data Data frame in long format.
#' @param L2ID (String) Name of column that contains grouping variable in \code{data} (e.g., \code{"SubjectID"}).
#' @param X (String) Name of column that contains the X independent variable in \code{data}.
#' @param Y (String) Name of column that contains the Y dependent variable in \code{data}.
#' @param M (String) Name of column that contains the M mediating variable in \code{data}.
#' @param random.a (Logical) Add random slope for 'a' path (i.e,. SmX)?
#' @param random.b (Logical) Add random slope for 'b' path (i.e., SyM)?
#' @param random.c (Logical) Add random slope for 'c' path (i.e., SyX)?
#' @param moderator Optional string that contains name of column that contains the moderator variable in \code{data}
#' @param mod.a (Logical) Add moderator to 'a' path (i.e., SmX:W, where W is the moderator)?
#' @param mod.b (Logical) Add moderator to 'b' path (i.e., SyM:W, where W is the moderator)?
#' @param mod.c (Logical) Add moderator to 'c' path (i.e., SyX:W, where W is the moderator)
#' @param method Argument passed to \code{\link[nlme]{lme}} to control estimation method.
#' @param control Argument passed to \code{\link[nlme]{lme}} that controls other estimation options.
#' @details TO DO. Implements custom function to do moderated mediation with multilevel models.
#'   Capable of doing moderation as well. Need to detail which kinds of moderation, which mediation models (e.g., 1-1-1 only?)
#'   Note: does not currently include this moderation as a random effect (the lme model will correctly place the moderator at the appropriate level)
#'   Implements the BPG06 model for 1-1-1 mediation with moderation...
#' @examples
#' \donttest{
#' # Example data for 1-1-1 w/o moderation
#' data(BPG06dat)
#'
#' # Fit model
#' fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
#'   random.a=TRUE, random.b=TRUE, random.c=TRUE)
#'
#' # Vector of parameter estimates, including indirect effect
#' fit$pars
#'
#' # The saved, fitted model following Bauer, Preacher, & Gil (2006)
#' summary(fit$model)
#'
#'
#'
#' # Fit model with moderation
#' data(simdat)
#'
#' # moderation for a path
#' fitmoda<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.c=TRUE,
#'   moderator = "mod", mod.a=TRUE)
#'
#' # moderation for b path
#' fitmodb<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.c=TRUE,
#'   moderator = "mod", mod.b=TRUE)
#'
#' # moderation for both a and b paths
#' fitmodab<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.c=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE)
#'
#' # Do we care about moderation for the c path?
#' fitmodabc<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
#'   random.a=TRUE, random.b=TRUE, random.c=TRUE,
#'   moderator = "mod", mod.a=TRUE, mod.b=TRUE, mod.c=TRUE)
#'
#' }
#' @import nlme
#' @importFrom matrixcalc vech
#' @importFrom tidyr pivot_longer
#' @importFrom MCMCpack xpnd
#' @importFrom stats as.formula
#' @export
modmed.mlm<-function(data, L2ID, X, Y, M,
                     random.a = FALSE, random.b = FALSE, random.c = FALSE,
                     moderator = NULL, mod.a = FALSE, mod.b = FALSE, mod.c = FALSE,
                     method="REML", control = lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
                                                         msMaxEval = 10000, tolerance = 1e-6)){

  # Some input checking per Todd's code:
  #FIXME: THESE MESSGEES HAPPEN EVERY LOOP. ANY WAY TO ONLY DO AT START?

  # Stop if X, Y, or M variables are not specified
  if (is.null(X)) {stop("X is NULL, please specify name of X variable.")}
  if (is.null(Y)) {stop("Y is NULL, please specify name of Y variable.")}
  if (is.null(M)) {stop("M is NULL, please specify name of M variable.")}

  # Stop if X, Y, or M are not in dataset
  if(!all(c(X,Y,M) %in% colnames(data))){stop("Not all variables found in dataset. Please check names of X, Y, and M.")}

  # Stop if X, Y, or M variables in data are not numeric
  # factors not currently able to be used to set up BPG syntax for lme model (is there a possible workaround?)
  if (!is.numeric(data[[X]])) {stop("X is of type ", class(data[[X]]), ". Currently, only numeric X is supported.")}
  if (!is.numeric(data[[Y]])) {stop("Y is of type ", class(data[[Y]]), ". Y must be numeric to fit model.")}
  if (!is.numeric(data[[M]])) {stop("M is of type ", class(data[[M]]), ". M must be numeric to fit model.")}

  # check moderator
  if(!is.null(moderator)){
    if(!(moderator %in% colnames(data))){stop("moderator not found in dataset. Please check specified name.")}
    if(!is.numeric(data[[moderator]])) {stop("moderator is of type ", class(data[[moderator]]), ". Currently, only numeric moderators are supported.")}
  }

  # Check that all random effects/path moderation args are logical values (will skip adding to formula otherwise)
  stopifnot(is.logical(random.a), is.logical(random.b), is.logical(random.c),
            is.logical(mod.a), is.logical(mod.b), is.logical(mod.c))


  #TODO: Have checks that all vars are there and that they are numeric (can convert here, but at least give warning)
  #TODO: Make sure we don't replace variables that already exist?
  # Assign variable names
  data$X = data[[X]]
  data$Y = data[[Y]]
  data$M = data[[M]]
  data$L2id <- data[[L2ID]] # Save copy of the grouping (Level 2) variable
  data$Md <- data$M # save copy of mediator (tv: why is this needed?) CF: it might not be, to check later

  # Save moderator if necessary
  if (!is.null(moderator)) {
    data$W <- data[[moderator]] # Save copy of the moderator
  }

  # restructure data such that both m and y are in the Z column
  tmp <- pivot_longer(data, cols = c(Y, M), names_to = "Outcome",
                      values_to = "Z")

  # create variables similar to Bauer et al syntax
  tmp$Sy <- ifelse(tmp$Outcome == "Y", 1, 0)
  tmp$Sm <- ifelse(tmp$Outcome == "M", 1, 0)
  tmp$SmX <- tmp$Sm * tmp$X
  tmp$SyX <- tmp$Sy * tmp$X
  tmp$SyM <- tmp$Sy * tmp$Md

  # Create the formula for the fixed effects
  fixed.formula <- "Z ~ 0 + Sm + Sy + SmX + SyX + SyM" #use the default formula from BPG 2006

  # Add in the moderator to the paths if necessary
  # Note: interactions w/ W must must use selector variables in this way
  #TODO: add checks here that params are boolean (and not eg string, raise error if not boolean, would skip otherwise)
  if (mod.a == TRUE) {fixed.formula <- paste(fixed.formula, "+ Sm:W + SmX:W")}
  if (mod.b == TRUE || mod.c == TRUE) {
    fixed.formula <- paste(fixed.formula, "+ Sy:W") #if b or c path is moderated, Sy component will always be there (prevents adding redundant parameters if both b & c are moderated)
    if (mod.b == TRUE && mod.c == TRUE) {fixed.formula <- paste(fixed.formula, "+ SyM:W + SyX:W")}
    if (mod.b == TRUE && mod.c == FALSE) {fixed.formula <- paste(fixed.formula, "+ SyM:W")}
    if (mod.b == FALSE && mod.c == TRUE) {fixed.formula <- paste(fixed.formula, "+ SyX:W")}
  }

  # Create the formula for the random effects
  #TODO: Same here, check that random params are boolean and not something else
  random.formula <- "~ 0 + Sm + Sy"
  if (random.a == TRUE) {random.formula <- paste(random.formula, "+ SmX")}
  if (random.b == TRUE) {random.formula <- paste(random.formula, "+ SyM")}
  if (random.c == TRUE) {random.formula <- paste(random.formula, "+ SyX")}
  #TODO: Need to add argument to make 3-level if necessary...(e.g., L2id/W)
  random.formula <- paste(random.formula, "| L2id") # add in the grouping variable after all the variables are entered

  mod_med_tmp <- try(lme(fixed = as.formula(fixed.formula), # fixed effects
                         random = as.formula(random.formula), # random effects
                         weights = varIdent(form = ~ 1 | Sm), # heteroskedasticity
                         data = tmp,
                         method = method,
                         control = control))
  # some error handling, just in case (is for when model doesn't converge?? test it out to see)
  if (class(mod_med_tmp) == "try-error") {
    indirect <- NA
    modindirect <- NA
    modindirecta3b <- NA
    fixestimates <- rep(NA, 10) #change from 10 to dynamic (number of fixed effects in model (10 is for all 5 + moderation on all 5))
  } else {

    vc <- VarCorr(mod_med_tmp)

    #grab names of random effects
    re_names <- colnames(mod_med_tmp[["coefficients"]][["random"]][["L2id"]])
    re_num <- length(re_names) #number of random effects
    #colnames(vc[1:5,]) = c("Variance","StdDev",re_names)

    sd <- as.numeric(vc[1:re_num, 2])
    sigma <- cbind(vc[1:re_num, 3:ncol(vc)], 1)
    diag(sigma) <- 1
    sig <- as.numeric(vech(sigma))
    sig <- xpnd(sig)
    D <- diag(sd)
    sig2 <- D %*% sig %*% D
    colnames(sig2) <- re_names
    rownames(sig2) <- re_names

    #if both a and b are random, add in the covar component between the two
    if (random.a == TRUE && random.b == TRUE) {
      indirect <- fixed.effects(mod_med_tmp)["SmX"] * fixed.effects(mod_med_tmp)["SyM"] + sig2["SmX", "SyM"]
    } else {
      indirect <- fixed.effects(mod_med_tmp)["SmX"] * fixed.effects(mod_med_tmp)["SyM"]
    }
    names(indirect)<-"indirect"

    # TODO: won't be relevant if we don't ask for moderation
    # TODO: work through possibilites for moderation with dichotomous moderator, possibly continuous moderator
    #modindirect<-fixed.effects(mod_med_tmp)["SmX:W"]*fixed.effects(mod_med_tmp)["SyM:W"]
    modindirect <- (fixed.effects(mod_med_tmp)["SmX:W"] + fixed.effects(mod_med_tmp)["SmX"]) * (fixed.effects(mod_med_tmp)["SyM:W"] + fixed.effects(mod_med_tmp)["SyM"])
    modindirecta3b <- fixed.effects(mod_med_tmp)["SmX:W"] * fixed.effects(mod_med_tmp)["SyM"] #trying to see fx with just a-path being moderated
    fixestimates <- fixef(mod_med_tmp)
  }

  out<-list()
  out$pars<-c(indirect, modindirect, modindirecta3b, fixestimates) # parameter estimates
  out$model<-mod_med_tmp # return fitted model

  return(out)

}
