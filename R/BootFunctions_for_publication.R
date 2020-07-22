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

# Implements the BPG06 model for 1-1-1 mediation with moderation...

# Args:
#   -data (data.frame):
#   -indices ():
#   -L2ID (character): Name of column that contains grouping variable in 'data' (e.g., "SubjectID")
#   -X (character): Name of column that contains the X independent variable in 'data'
#   -Y (character): Name of column that contains the Y dependent variable in 'data'
#   -M (character): Name of column that contains the M mediating variable in 'data'
#   -random.a (logical): Add random slope for 'a' path (i.e,. SmX).
#   -random.b (logical): Add random slope for 'b' path (i.e., SyM).
#   -random.c (logical): Add random slope for 'c' path (i.e., SyX).
#   -moderator (character): Name of column that contains the moderator variable in 'data'
#   -mod.a (logical): Add moderator to 'a' path (i.e., SmX:W, where W is the moderator)
#   -mod.b (logical): Add moderator to 'b' path (i.e., SyM:W, where W is the moderator)
#   -mod.c (logical): Add moderator to 'c' path (i.e., SyX:W, where W is the moderator)

# Returns:
#   asf: boot object??

# Essentially what P-G code does for the resampling part, but with using lme instead
# For resampling, indices determines which cases are resampled. The "strata" argument
# to the "boot" function will allow identification of L2 units. L1 units are resampled within each strata

# More proper resampling approach, and again use of lme
# This resamples L2 units, then L1 units within each L2 unit
#Note: does not currently include this moderation as a random effect (the lme model will correctly place the moderator at the appropriate level)

#' Custom boot function for two level models
#'
#' @param data blah
#' @param indices blah
#' @param L2ID blah
#' @param X blah
#' @param Y blah
#' @param M blah
#' @param random.a blah
#' @param random.b blah
#' @param random.c blah
#' @param moderator blah
#' @param mod.a blah
#' @param mod.b blah
#' @param mod.c blah
#' @param method blah
#' @param control blah
#' @details TO DO. Implements custom function to do resampling at level 2, then level 1. For use with boot package.
#'   Capable of doing moderation as well. Need to detail which kinds of moderation, which mediation models (e.g., 1-1-1 only?)
#' @examples
#' \donttest{
#' # add some example code here
#'
#' }
#' @import nlme
#' @importFrom matrixcalc vech
#' @importFrom tidyr pivot_longer
#' @importFrom MCMCpack xpnd
#' @importFrom stats as.formula
#' @export
boot.mlm2 <- function(data, indices, L2ID, X = NULL, Y = NULL, M = NULL,
                    random.a = FALSE, random.b = FALSE, random.c = FALSE,
                    moderator = NULL, mod.a = FALSE, mod.b = FALSE, mod.c = FALSE,
                    method="REML", control = lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
                                                        msMaxEval = 10000, tolerance = 1e-6)) {

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
      #L2_sub #TV: can remove?
    })

    rdat <- do.call("rbind", rdat)
    row.names(rdat) <- NULL
  }


  #TODO: Have checks that all vars are there and that they are numeric (can convert here, but at least give warning)
  #TODO: Make sure we don't replace variables that already exist?
  # Assign variable names
  rdat$X = rdat[[X]]
  rdat$Y = rdat[[Y]]
  rdat$M = rdat[[M]]
  rdat$L2id <- rdat[[L2ID]] # Save copy of the grouping (Level 2) variable
  rdat$Md <- rdat$M # save copy of mediator (tv: why is this needed?)

  # Save moderator if necessary
  #TODO:ADD warning/error message if moderator is a factor and that is should be numeric
  if (!is.null(moderator)) {
    rdat$W <- rdat[[moderator]] # Save copy of the moderator
    rdat$W <- as.numeric(rdat$W) #will give an error (singularity in backsolve if using a factor because of the way R does formulas...(chnage to numeric to fix, how make compatible if passed as a factor or something else??))
  }

  #TODO:maybe have a check if mod is factor. If yes, convert (with warning) to
  #numeric where levels are 0 and 1. If indirectCI==TRUE then do model at both levels
  #otherwise just center and run the model that way...?

  # restructure data such that both m and y are in the Z column
  tmp <- pivot_longer(rdat, cols = c(Y, M), names_to = "Outcome",
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
  #(Can't have just W as a predictor (gives backsolve errors), must use selector variables)
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

  #Possible to return other stuff? eg formulas used? (usually not if we're doing bootstrapping at the same time)
  # TO DO: but, could write the function that does all of this, returns formulas and stuff. Then, a wrapper that does the bootstrapping and only grabs parameter estimates
  #Will give NA in t0 if the first iteration fails to converge (might have been a problem with the parallel package??)
  return(c(indirect, modindirect, modindirecta3b, fixestimates))
}
