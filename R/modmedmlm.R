######################################################################
## Functions for use with bootstrapping
##
## Copyright 2019-2024 Carl F. Falk, Todd Vogel
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

#' Boot function for (moderated) mediation with 2-level multilevel models
#'
#' @param data Data frame in long format.
#' @param indices \code{boot} requires the function signature to accept a vector of
#'   index numbers and so this argument is required. If the index numbers are all in order starting at 1,
#'   then the relevant model will be fit to the data without any resampling. If some other vector is supplied,
#'   then resampling is done as described in details.
#' @param L2ID Name of column that contains grouping variable in 'data' (e.g., "SubjectID")
#' @param ... Arguments passed to \code{\link{modmed.mlm}} to define the mediation analysis model.
#' @param type Character that defines what information to extract from the model. Default and options are in \code{\link{extract.modmed.mlm}}.
#'   As examples, "indirect" will compute the indirect effect, "all" will save all random and fixed effects for possible additional
#'   computations, "indirect.diff" will compute the difference in the indirect effect at two values of a possible moderating variable.
#' @param modval1 (Optional) Numeric. If the model has a moderator, this value will be passed to \code{\link{extract.modmed.mlm}}
#'   to compute the indirect effect or other effects at that value. See \code{\link{extract.modmed.mlm}} for details.
#' @param modval2 (Optional). If the model has a moderator, it is possible to compute the difference in the indirect
#'   at two values of the moderator. If given and an appropriate option for such a difference is chosen for \code{type},
#'   this value and that of \code{modval1} will be passed to \code{\link{extract.modmed.mlm}} to compute and save the difference.
#'   This is useful for obtaining a CI for the difference in the indirect effect at two different levels of the moderator.
#' @param boot.lvl Character that defines at what level resampling should occur. Options are "both", "1", or "2". "both" will sample L2 units
#'   and then L1 units w/in each cluster. This has been noted to result in unequal sample sizes if the original clusters did not have equal sample sizes.
#'   "2" resamples only L2 units and leaves all L1 units intact. "1" will assume that whatever indices are fed from the boot function will
#'   be used. This probably only makes sense if \code{strata} is specified.
#' @details Implements function to do bootstrapping with the 1-1-1 multilevel mediation analysis models as used in Falk, Vogel,
#'   Hammami & Miočević (in press). For use with boot package. This function aides in implementing case resampling methods
#'   with support for resampling at level 2, level 1, or both (e.g., see Hox and van de Schoot, 2013; van der Leeden, Meijer, & Busing, 2008).
#'   These functions also support moderated mediation. See also \code{\link{modmed.mlm}}. Note that \code{\link{nlm}} was used as the optimizer
#'   for some of the examples below as it was found to be faster for the models/simulations studied by Falk et al (in press).
#' @references
#' Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing random indirect effects and moderated mediation in multilevel models: New procedures and recommendations. Psychological Methods, 11(2), 142–163. \doi{10.1037/1082-989X.11.2.142}
#'
#' Falk, C. F., Vogel, T., Hammami, S., & Miočević, M. (in press). Multilevel mediation analysis in R: A comparison of bootstrap and Bayesian approaches. Behavior Research Methods. \doi{10.3758/s13428-023-02079-4}  Preprint: \doi{10.31234/osf.io/ync34}
#'
#' Hox, J., & van de Schoot, R. (2013). Robust methods for multilevel analysis. In M. A. Scott, J. S. Simonoff & B. D. Marx (Eds.), The SAGE Handbook of Multilevel Modeling (pp. 387-402). SAGE Publications Ltd. \doi{10.4135/9781446247600.n22}
#'
#' van der Leeden, R., Meijer, E., & Busing, F. M. T. A. (2008). Resampling multilevel models. In J. de Leeuw & E. Meijer (Eds.), Handbook of Multilevel Analysis (pp. 401-433). Springer.
#' @examples
#' \dontrun{
#' ## Mediation for 1-1-1 model
#' data(BPG06dat)
#'
#' # Do bootstrapping... w/ parallel processing
#' # snow appears to work on Windows; something else may be better on Unix/Mac/Linux
#'
#'
#' #library(parallel)
#' #library(boot)
#' #ncpu<-6
#' #RNGkind("L'Ecuyer-CMRG") # set type of random number generation that works in parallel
#' #cl<-makeCluster(ncpu)
#' #clusterSetRNGStream(cl, 9912) # set random number seeds for cluster
#'
#' # bootstrap just the indirect effect
#' #boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
#' #  L2ID = "id", X = "x", Y = "y", M = "m",
#' #  random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #  type="indirect",
#' #  control=list(opt="nlm"),
#' #  parallel="snow",ncpus=ncpu,cl=cl)
#'
#'
#'
#' #boot.result$t0 # point estimates for everything based on original data
#' #boot.ci(boot.result, index=1, type="perc") # percentile interval of first element
#'
#' # bootstrap all fixed and random effects (recommended)
#' #boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
#' #  L2ID = "id", X = "x", Y = "y", M = "m",
#' #  random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #  type="all",
#' #  control=list(opt="nlm"),
#' #  parallel="snow",ncpus=ncpu,cl=cl)
#'
#' # Point estimate and 95% CI for indirect effect
#' #extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' #stopCluster(cl)
#'
#' # without cluster
#' # boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=5,
#' #   L2ID = "id", X = "x", Y = "y", M = "m",
#' #   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #   control=list(opt="nlm"),
#' #   type="indirect")
#'
#' ## Moderated mediation
#'
#' #data(simdat)
#' #ncpu<-12
#' #cl<-makeCluster(ncpu)
#'
#' # note: use of nlm apparently fails in this moderated mediation model
#' # default optimizer for lme instead is used
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
#' # Example to not fail when using missing values
#' # Missing data handling is not that great as not all info is used
#' # dat.miss <- BPG06dat
#' # dat.miss$m[c(1,2,3,4)]<-NA
#' # dat.miss$y[c(5,6,7,8)]<-NA
#' # boot.result<-boot(dat.miss, statistic=boot.modmed.mlm, R=100,
#' #  L2ID = "id", X = "x", Y = "y", M = "m",
#' #  random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #  type="all",
#' #  control=list(opt="nlm"),
#' #  na.action = na.omit)
#'
#' }
#' @export boot.modmed.mlm
boot.modmed.mlm <- function(data, indices, L2ID, ...,
                            type="all", modval1=NULL, modval2=NULL,
                            boot.lvl = c("both","1","2")) {
  warning("boot.modmed.mlm is deprecated (programmer speak for will eventually be replaced in favor of boot.modmed.mlm.custom)")

  boot.lvl <- match.arg(boot.lvl)

  # ad-hoc check if this is first run of analysis by comparing to indices
  if (all(indices == (1:nrow(data)))) {
    # do nothing
    rdat <- data[indices,]
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
        return(L2_sub)
      })
      #re-index L2ID names
      rdat <- lapply(seq_along(rdat), function(x) {
        rdat[[x]][[L2ID]] <- x
        return(rdat[[x]])
      })
      rdat <- do.call("rbind", rdat)

    } else if (boot.lvl == "2") {
      # Resample L2 units only
      L2 <- unique(data[, L2ID])
      N <- length(L2)
      L2_indices <- sample(L2, N, replace = TRUE)
      rdat <- lapply(L2_indices, function(x) {
        L2_sub <- data[data[, L2ID] == x, , drop = FALSE] # in case there is only 1 obs
        return(L2_sub)
      })
      #re-index L2ID names
      rdat <- lapply(seq_along(rdat), function(x) {
        rdat[[x]][[L2ID]] <- x
        return(rdat[[x]])
      })
      rdat <- do.call("rbind", rdat)


    } else if (boot.lvl == "1") {
      # Resample L1 units only (based on indices from boot function)
      rdat <- data[indices,]
    }

    row.names(rdat) <- NULL
  }

  result<-modmed.mlm(rdat,L2ID,...)

  return(extract.modmed.mlm(result,type=type,modval1=modval1,modval2=modval2))
}

#' Custom function for residual bootstrap for (moderated) multilevel mediation
#'
#' @param data Data frame in long format.
#' @param L2ID Name of column that contains grouping variable in 'data' (e.g., "SubjectID")
#' @param R Number of resamples
#' @param X (Character) Name of column that contains the X independent variable in \code{data}.
#' @param Y (Character) Name of column that contains the Y dependent variable in \code{data}.
#' @param M (Character) Name of column that contains the M mediating variable in \code{data}.
#' @param moderator Optional Character that contains name of column that contains the moderator variable in \code{data}
#' @param covars.m (Character vector) Optional covariates to include in the model for M.
#' @param covars.y (Character vector) Optional covariates to include in the model for Y.
#' @param ... Arguments passed to \code{\link{modmed.mlm}} to define the mediation analysis model.
#' @param type Character that defines what information to extract from the model. Default and options are in \code{\link{extract.modmed.mlm}}.
#'   As examples, "indirect" will compute the indirect effect, "all" will save all random and fixed effects for possible additional
#'   computations, "indirect.diff" will compute the difference in the indirect effect at two values of a possible moderating variable.
#' @param modval1 (Optional) Numeric. If the model has a moderator, this value will be passed to \code{\link{extract.modmed.mlm}}
#'   to compute the indirect effect or other effects at that value. See \code{extract.modmed.mlm} for details.
#' @param modval2 (Optional). If the model has a moderator, it is possible to compute the difference in the indirect
#'   at two values of the moderator. If given and an appropriate option for such a difference is chosen for \code{type},
#'   this value and that of \code{modval1} will be passed to \code{\link{extract.modmed.mlm}} to compute and save the difference.
#'   This is useful for obtaining a CI for the difference in the indirect effect at two different levels of the moderator.
#' @details This function restructures data following Bauer, Pearcher, & Gil (2006) and then conducts residual-based
#' bootstrapping in order to later obtain confidence intervals for the indirect effect and other coefficients.
#' The residual-based bootstrap is described in Falk, Vogel, Hammami, & Miočević's manuscript (in press), but
#' generally follows the procedure by Carpenter, Goldstein, & Rashbash (2003; See also Lai, 2021). Currently this function
#' does not support parallel processing. See the newer \code{\link{boot.modmed.mlm.custom}} version for a re-write that does.
#'
#' @return A list with the following elements. Note that \code{t0} and \code{t} are intended to trick the \code{\link[boot]{boot}}
#'   package into working with some if its functions.
#' \itemize{
#'  \item{\code{t0} Parameter estimates based on the dataset.}
#'  \item{\code{t} Bootstrap distribution of all parameter estimates.}
#'  \item{\code{model} Fitted model to restructured data as one would obtain from \code{modmed.mlm}.}
#'  \item{\code{call} Call/arguments used when invoking this function. Useful for later extracting things like indirect effect.}
#' }
#'
#' @references
#'
#' Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing random indirect		effects and moderated mediation in multilevel models: new procedures and	recommendations. Psychological Methods, 11(2), 142-163. \doi{10.1037/1082-989X.11.2.142}
#'
#' Carpenter, J. R., Goldstein, H., & Rasbash, J. (2003). A novel bootstrap procedure for assessing the relationship between class size and achievement. Applied Statistics, 52(4), 431-443.
#'
#' Falk, C. F., Vogel, T., Hammami, S., & Miočević, M. (in press). Multilevel mediation analysis in R: A comparison of bootstrap and Bayesian approaches. Behavior Research Methods. \doi{10.3758/s13428-023-02079-4}  Preprint: \doi{10.31234/osf.io/ync34}
#'
#' Lai, M. (2021). Bootstrap confidence intervals for multilevel standardized effect size. Multivariate Behavioral Research, 56(4), 558-578. \doi{10.1080/00273171.2020.1746902}
#'
#' @examples
#' \dontrun{
#' # Example data for 1-1-1 w/o moderation
#' data(BPG06dat)
#'
#' # Fit model
#' fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE)
#'
#' bootresid <- bootresid.modmed.mlm(BPG06dat,L2ID="id", X="x", Y="y", M="m",
#'   R=100, random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   control=list(opt="nlm")
#'   )
#'
#' extract.boot.modmed.mlm(bootresid, type="indirect")
#'
#'
#' }
#' @importFrom stats resid var model.matrix terms
#' @export bootresid.modmed.mlm
bootresid.modmed.mlm <- function(data, L2ID, R=1000, X, Y, M,
                                 moderator=NULL, covars.m=NULL, covars.y=NULL, ...,
                            type="all", modval1=NULL, modval2=NULL) {

  warning("bootresid.modmed.mlm is deprecated (programmer speak for will eventually be replaced in favor of boot.modmed.mlm.custom)")

  # data that's being used
  tmp <- stack_bpg(data, L2ID, X, Y, M,
                   moderator=moderator,
                   covars.m = covars.m,
                   covars.y = covars.y
  )

  # fit initial model
  init.mod <- modmed.mlm(data, L2ID, X, Y, M,
                         moderator=moderator, covars.m=covars.m, covars.y=covars.y, ...)

  ## Extract relevant stuff

  # fixed effects
  fe<-fixef(init.mod$model)

  # L2
  l2groups<-unique(init.mod$model$groups$L2id) # group IDs
  nl2<-length(l2groups) # N at l2
  #l2resid <- coef(init.mod$model) # actually, that's fixed effects + random effects
  l2resid <- random.effects(init.mod$model) # random effects (l2 residuals)
  modvl2 <- randef.lme(init.mod$model)$sig2 # extract var-cov of random effects

  # L1
  l1resid <- resid(init.mod$model) # l1 residuals
  l1sig<-init.mod$model$sigma # l1 error sd for Y
  l1varstruct<-init.mod$model$modelStruct$varStruct # contains info about scaling of error for Y
  l1sds<-(1/attr(l1varstruct,"weights")[1:2])*l1sig # obtain actual l1 error sds
  l1vars<-diag(l1sds^2) # l1 error variances
  l1groups<-attr(l1varstruct,"groups") # indicators for which obs is Y vs M
  # it's critical that modmed.mlm does heteroscedasticity in same way, otherwise next lines break
  yresid<-l1resid[l1groups=="0"] # l1 y residuals
  mresid<-l1resid[l1groups=="1"] # l1 m residuals
  alll1resid<-cbind(yresid,mresid) # all l1 residuals
  nl1<-nrow(alll1resid) # N at l1


  ## Reflate stuff (Carpenter, Goldstein, & Rashbash, 2003)

  # L2
  vl2<-var(l2resid)*(nl2-1)/nl2
  LR<-chol(modvl2, pivot=TRUE)
  LR<-LR[order(attr(LR,"pivot")),order(attr(LR,"pivot"))] # sometimes rank deficient, thus pivot used
  LS<-chol(vl2, pivot=TRUE)
  LS<-LS[order(attr(LS,"pivot")),order(attr(LS,"pivot"))]
  A<-t(t(LR)%*%solve(t(LS)))
  l2resid.infl<-as.matrix(l2resid)%*%A

  # check
  #var(l2resid.infl)*(nl2-1)/nl2 # should be close to modvl2

  # L1

  # reflate L1 resid
  vl1<-var(alll1resid)*(nl1-1)/nl1
  LRl1<-chol(l1vars, pivot=TRUE)
  LRl1<-LRl1[order(attr(LRl1,"pivot")),order(attr(LRl1,"pivot"))]
  LSl1<-chol(vl1, pivot=TRUE)
  LSl1<-LSl1[order(attr(LSl1,"pivot")),order(attr(LSl1,"pivot"))]
  Al1<-t(t(LRl1)%*%solve(t(LSl1)))
  l1resid.infl<-as.matrix(alll1resid)%*%Al1

  # check
  #var(l1resid.infl)*(nl1-1)/nl1

  resmat<-NULL # storage of results

  # now, sample L2 and L1 resid
  for(it in 1:R){

    # sample ids
    L2idxsamp<-sample(1:nl2, nl2, replace=T)
    L1Yidxsamp<-sample(1:nl1, nl1, replace=T)
    L1Midxsamp<-sample(1:nl1, nl1, replace=T)

    # use ids to sample residuals
    l2resid.boot<-l2resid.infl[L2idxsamp,]
    l1Yresid.boot<-l1resid.infl[L1Yidxsamp,1]
    l1Mresid.boot<-l1resid.infl[L1Midxsamp,2]
    l1resid.boot<-rep(NA,nl1*2)
    l1resid.boot[l1groups=="0"]<-l1Yresid.boot
    l1resid.boot[l1groups=="1"]<-l1Mresid.boot

    # add fixed effects to l2 random effects
    bootcoef<-(rep(1,nl2))%*%t(fe)
    bootcoef[,colnames(l2resid.boot)]<- bootcoef[,colnames(l2resid.boot)] + l2resid.boot

    # Then, just directly compute Y and M
    tmp2 <- merge(tmp, as.data.frame(model.matrix(init.mod$model$terms, tmp)), sort=FALSE)
    Zs<-lapply(l2groups, function(grp){
      tmpsub<-as.matrix(tmp2[tmp2$L2id %in% grp, colnames(bootcoef)])
      tmpcoef<-bootcoef[which(l2groups%in%grp), ]
      tmpsub%*%t(t(tmpcoef))
    })
    Zs<-do.call("c",Zs)

    # add Y and M to data frame
    tmp2$Z<-Zs+l1resid.boot
    tmp2 <- tmp2[,c("L2id",names(attr(terms(init.mod$model),"dataClasses")))]

    # fit model
    result<-try(modmed.mlm(NULL,L2ID, X, Y, M,
                           moderator=moderator, covars.m=covars.m, covars.y=covars.y,data.stacked=tmp2,...))

    # extract and save results
    if(!inherits(result, "try-error")){
      if(is.null(resmat)){
        resmat<-extract.modmed.mlm(result,type=type,modval1=modval1,modval2=modval2)
      } else {
        resmat<-rbind(resmat, extract.modmed.mlm(result,type=type,modval1=modval1,modval2=modval2))
      }
    }
  }

  # extract results from initial model
  t0res<-extract.modmed.mlm(init.mod,type=type,modval1=modval1,modval2=modval2)

  out<-list(t0 = t0res,
            t=resmat,
            model = init.mod,
            call = match.call())

  return(out)
}

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
#' @param random.covars.m (Logical vector) Add random slopes for covariates on M?
#' @param random.covars.y (Logical vector) Add random slopes for covariates on Y?
#' @param method Argument used to control estimation method. Options are "REML" (default) or "ML".
#' @param estimator Which program to use to estimate models? \code{\link[nlme]{lme}} is what was originally tested
#'   with the package and publication, but support for \code{\link[glmmTMB]{glmmTMB}} is now available.
#' @param control Argument passed to \code{\link[nlme]{lme}} or \code{\link[glmmTMB]{glmmTMB}} that controls other estimation options.
#'   See those functions for the \code{control} argument. If \code{\link[nlme]{lme}} is chosen for estimation, but nothing is specified
#'   for \code{control}, some defaults values are populated that basically greatly increase the number of admissible
#'   iterations.
#' @param returndata (Logical) Whether to save restructured data in its own slot. Note: nlme may do this automatically. Defaults to \code{FALSE}.
#' @param datmfun (experimental) A function that will do additional data manipulation on the restacked dataset. The function ought to take
#'   the restacked dataset (e.g., done using \code{\link{stack_bpg}}) and return a dataset that can be analyzed using \code{\link{modmed.mlm}} Could be used for
#'   some kind of additional centering strategy after data are restacked (and after bootstrapped) or some other missing data handling strategy.
#'   Either suggestion requires further study.#'
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
#' \dontrun{
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
#'# of default missing data handling strategies in MLM
#'dat.miss <- BPG06dat
#'dat.miss$m[c(1,2,3,4)]<-NA
#'dat.miss$y[c(5,6,7,8)]<-NA
#'fit<-modmed.mlm(dat.miss,"id", "x", "y", "m",
#'                 random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'                 na.action = na.omit)
#' }
#' @import nlme
#' @import glmmTMB
#' @importFrom matrixcalc vech
#' @importFrom MCMCpack xpnd
#' @importFrom stats as.formula
#' @export modmed.mlm
modmed.mlm <- function(data, L2ID, X, Y, M,
                     moderator = NULL, mod.a = FALSE, mod.b = FALSE, mod.cprime = FALSE,
                     covars.m = NULL, covars.y = NULL,
                     random.a = FALSE, random.b = FALSE, random.cprime = FALSE,
                     random.mod.a = FALSE, random.mod.b = FALSE, random.mod.cprime = FALSE,
                     random.mod.m = FALSE, random.mod.y = FALSE,
                     random.covars.m = NULL, random.covars.y = NULL,
                     estimator = c("lme","glmmTMB"),
                     method= c("REML","ML"), control = NULL,
                     returndata = FALSE,
                     datmfun = NULL,
                     data.stacked = NULL,
                     ...){

  if (is.null(moderator) && any(mod.a, mod.b, mod.cprime)) {
    # Give error if paths indicated as moderated, but no moderator name given
    stop("No moderator was specified for the moderated path(s).")
  }

  estimator <- match.arg(estimator)
  method <- match.arg(method)

  # save default estimation options, for backwards compatibility
  if(estimator == "lme" & is.null(control)){
    control <- lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
               msMaxEval = 10000, tolerance = 1e-6)
  } else if (estimator == "glmmTMB" & is.null(control)){
    control <- glmmTMBControl()
  }

  if(is.null(data.stacked)){
    tmp <- stack_bpg(data, L2ID, X, Y, M,
                     moderator=moderator,
                     covars.m = covars.m,
                     covars.y = covars.y
    )
  } else {
    tmp <- data.stacked
  }

  # further data manipulation, if present
  if(!is.null(datmfun)){
    tmp <- datmfun(tmp)
  }

  # Create the formula for the fixed effects
  fixed.formula <- "Z ~ 0 + Sm + Sy + SmX + SyX + SyM" #use the default formula from BPG 2006
  #FIXME: rename variables to something more intuitive? Eg a, b & cprime paths?

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
  #CFF: don't think it matters
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
  #CFF: I would suppose it's possible. Depends on user's theory. Leave in for more flexibilty.

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

  if(estimator == "lme"){
    # Run the model through nlme
    mod_med_tmp <- try(lme(fixed = as.formula(fixed.formula), # fixed effects
                           random = as.formula(random.formula), # random effects
                           weights = varIdent(form = ~ 1 | Sm), # heteroskedasticity
                           data = tmp,
                           method = method,
                           control = control,
                           ...))
  } else if (estimator == "glmmTMB"){
    # some quick fixes to get glmmTMB up and running
    random.formula <- gsub("~ ", "", random.formula)
    random.formula <- paste0("(",random.formula,")")
    form <- paste0(fixed.formula,"+", random.formula)

    mod_med_tmp <- glmmTMB(as.formula(form),
            dispformula =  ~ 1 + Sm,
            #dispformula =  ~ 0 + Sm + Sy,
            family = gaussian,
            data = tmp,
            REML = (method=="REML"),
            control = control,
            ...)
  }


  # create output list
  out <- list()

  # some error handling, just in case
  if (inherits(mod_med_tmp, "try-error")){
    out$model <- NULL
    out$conv <- FALSE # boolean or some other code?
  } else {
    out$model <- mod_med_tmp
    out$conv <- TRUE
  }

  if(returndata) out$data <- tmp

  #out$call <- match.call()
  #TODO: TV: Have a test that checks that this matches the list of parameters for the function call above?
  out$args<-list(
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
    estimator = estimator,
    method = method,
    control = control,
    returndata = returndata
  )

  return(out)

}

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
#' @export extract.modmed.mlm
extract.modmed.mlm <- function(fit, type=c("all","fixef","recov","recov.vec","indirect","a","b","cprime","covab",
                                           "indirect.diff","a.diff","b.diff","cprime.diff"),
                               modval1 = NULL, modval2 = NULL){

  type <- match.arg(type)
  args <- fit$args

  if(!fit$conv || is.null(fit$model)){

    # If model fitting was a problem
    # Depending on type and fit$args, it is possible to guess the length of output here
    # This is a bit tenuous if support for more variables changes, however

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
    #args<-as.list(fit$call)

    # extract random effects
    re<-randef.lme(fit$model)
    sig2 <- re$sig2
    sig2vec <- re$sig2vec

    # remove duplicates ?
    #select <- as.vector(lower.tri(sig2, diag=T))
    #sig2vec <- sig2vec[select]

    # extract fixed effects
    if(inherits(fit$model, "glmmTMB")){
      fixed <- fixef(fit$model)$cond
    } else {
      fixed <- fixef(fit$model)
    }

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

randef.lme <- function(model){

  if(inherits(model, "glmmTMB")){
    sig2 <- VarCorr(model)$cond$L2id
    attr(sig2, "stdev") <- NULL
    attr(sig2, "correlation") <- NULL
  } else {
    # extract var-cov matrix among random effects
    sig2 <- getVarCov(model)
    class(sig2) <- "matrix"
    attr(sig2,"group.levels") <- NULL
  }

  re.names<-colnames(sig2)

  # rand effects as vector
  sig2vec <- as.vector(sig2)
  elementnames <- expand.grid(re.names,re.names)
  elementnames <- paste0("re.",elementnames[,1],elementnames[,2])
  names(sig2vec) <- elementnames


  out<-list(sig2 = sig2,
            sig2vec = sig2vec)

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
#' @export extract.boot.modmed.mlm
#' @examples
#' \donttest{
#' ## Mediation for 1-1-1 model
#' data(BPG06dat)
#'
#' #library(parallel)
#' #library(boot)
#' #ncpu<-6
#' #cl<-makeCluster(ncpu)
#'
#'
#' # bootstrap all fixed and random effects (recommended)
#' #boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
#' #   L2ID = "id", X = "x", Y = "y", M = "m",
#' #   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #   type="all",
#' #   control=list(opt="nlm"),
#' #   parallel="snow",ncpus=ncpu,cl=cl)
#'
#' #stopCluster(cl)
#'
#'
#' # Point estimate and 95% CI for indirect effect
#' #extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' # residual-based bootstrap
#' # bootresid <- bootresid.modmed.mlm(BPG06dat,L2ID="id", X="x", Y="y", M="m",
#' #   R=100, random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#' #   control=list(opt="nlm"))
#' #
#' # interval for the indirect effect
#' #extract.boot.modmed.mlm(bootresid, type="indirect")
#'
#' }
#' @importFrom stats quantile
extract.boot.modmed.mlm <- function(boot.obj, type=c("indirect","a","b","cprime","covab",
                                           "indirect.diff","a.diff","b.diff","cprime.diff"), ci.type="perc", ci.conf=.95,
                               modval1 = NULL, modval2 = NULL){
  type <- match.arg(type)
  ci.type <- match.arg(ci.type)

  # match.call is tricky; could break if this is inside a function?
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
  if(ci.type=="perc"){
    probs <- c((1-ci.conf)/2, ci.conf+(1-ci.conf)/2)
    ci <- quantile(est, probs=probs, na.rm=TRUE)
  }

  # output
  out<-list(CI = ci,
            est = point.est)

  return(out)

}


# Compute stuff from a vector of fixed effects and random effects
# Toggling b/w boot and brms a bit ad-hoc for now.
# At least best to have all computations in the same place

#' @importFrom brms as_draws_matrix
compute.indirect <- function(v, args,
                             type=c("indirect","a","b","cprime","covab","indirect.diff","a.diff","b.diff","cprime.diff"),
                             modval1 = NULL, modval2 = NULL,
                             boot = TRUE){

  # TODO: need some input checking here. e.g., .diff isn't relevant unless both modval1 and modval2 are specified
  # And these would not work unless relevant moderation effects are actually estimated mod.a, mod.b, mod.cprime

  if(boot){
    a1 <- a2 <- v["SmX"]
    b1 <- b2 <- v["SyM"]
    cprime1 <- cprime2 <- v["SyX"]
  } else {
    a1 <- a2 <- as_draws_matrix(v, "b_SmX")
    b1 <- b2 <- as_draws_matrix(v, "b_SyM")
    cprime1 <- cprime2 <- as_draws_matrix(v, "b_SyX")
  }


  # If moderation effects, modify a and b
  if(!is.null(args$mod.a) && args$mod.a && !is.null(modval1)){
    if(boot){
      a1 <- a1 + v["SmX:W"]*modval1
    } else {
      a1 <- a1 + as_draws_matrix(v, "b_SmX:W")*modval1
    }
  }
  if(!is.null(args$mod.a) && args$mod.a && !is.null(modval2)){
    if(boot){
      a2 <- a2 + v["SmX:W"]*modval2
    } else {
      a2 <- a2 + as_draws_matrix(v, "b_SmX:W")*modval2
    }
  }
  if(!is.null(args$mod.b) && args$mod.b && !is.null(modval1)){
    if(boot){
      b1 <- b1 + v["SyM:W"]*modval1
    } else {
      b1 <- b1 + as_draws_matrix(v, "b_SyM:W")*modval1
    }
  }
  if(!is.null(args$mod.b) && args$mod.b && !is.null(modval2)){
    if(boot){
      b2 <- b2 + v["SyM:W"]*modval2
    } else {
      b2 <- b2 + as_draws_matrix(v, "b_SyM:W")*modval2
    }
  }
  if(!is.null(args$mod.cprime) && args$mod.cprime && !is.null(modval1)){
    if(boot){
      cprime1 <- cprime1 + v["SyX:W"]*modval1
    } else {
      cprime1 <- cprime1 + as_draws_matrix(v, "b_SyX:W")*modval1
    }
  }
  if(!is.null(args$mod.cprime) && args$mod.cprime && !is.null(modval2)){
    if(boot){
      cprime2 <- cprime2 + v["SyX:W"]*modval2
    } else {
      cprime2 <- cprime2 + as_draws_matrix(v, "b_SyX:W")*modval2
    }
  }

  # compute indirect effect using only fixed effects
  ab1 <- a1*b1
  if(!is.null(modval2)){ ab2 <-a2*b2 }

  # additional adjustments to indirect effect from random effects

  # cov among a and b paths
  if(!is.null(args$random.a) && !is.null(args$random.b) && args$random.a && args$random.b){
    if(boot){
      covab <- v["re.SmXSyM"]
    } else {
      corab <- as_draws_matrix(v, "cor_L2id__SmX__SyM")
      sda <- as_draws_matrix(v, "sd_L2id__SmX")
      sdb <- as_draws_matrix(v, "sd_L2id__SyM")
      covab <- corab*sda*sdb
    }
    ab1 <- ab1 + covab
    if(!is.null(modval2)){ ab2 <- ab2 + covab }
  }

  # random effects in case interaction term has random effects
  # TODO: Add options so that these random effects could also be returned?
  #     Mostly for debugging purposes I suppose
  if(!is.null(modval1)){
    if(!is.null(args$random.b) && !is.null(args$mod.a) && !is.null(args$random.mod.a) &&
       args$random.b && args$mod.a && args$random.mod.a){
      if(boot){
        ab1 <- ab1 + modval1 * v["re.SyMSmX:W"] # times covariance between re.b and re.mod.a
      } else {

        # correl bw/ re.b and re.mod.a
        correl <- as_draws_matrix(v, "cor_L2id__SyM__SmX:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SyM") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        covre <- correl*sd1*sd2
        ab1 <- ab1 + modval1 * covre
      }
    }
    if(!is.null(args$random.a) && !is.null(args$mod.b) && !is.null(args$random.mod.b) &&
       args$random.a && args$mod.b && args$random.mod.b){
      if(boot){
        ab1 <- ab1 + modval1 * v["re.SmXSyM:W"] # times covariance between re.a and re.mod.b
      } else {
        # correl between re.a and re.modm.b
        correl <- as_draws_matrix(v, "cor_L2id__SmX__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl*sd1*sd2
        ab1 <- ab1 + modval1 * covre
      }
    }
    if(!is.null(args$random.mod.a) && !is.null(args$random.mod.b) &&
       args$random.mod.a && args$random.mod.b){
      if(boot){
        ab1 <- ab1 + (modval1^2) * v["re.SmX:WSyM:W"] # times covariance between re.mod.a and re.mod.b
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SmX:W__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl*sd1*sd2
        ab1 <- ab1 + (modval1^2) * covre
      }
    }
  }
  if(!is.null(modval2)){
    if(!is.null(args$random.b) && !is.null(args$mod.a) && !is.null(args$random.mod.a) &&
       args$random.b && args$mod.a && args$random.mod.a){
      if(boot){
        ab2 <- ab2 + modval2 * v["re.SyMSmX:W"] # times covariance between re.b and re.mod.a
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SyM__SmX:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SyM") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        covre <- correl*sd1*sd2
        ab2 <- ab2 + modval2 * covre
      }
    }
    if(!is.null(args$random.a) && !is.null(args$mod.b) && !is.null(args$random.mod.b) &&
       args$random.a && args$mod.b && args$random.mod.b){
      if(boot){
        ab2 <- ab2 + modval2 *  v["re.SmXSyM:W"] # times covariance between re.a and re.mod.b
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SmX__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl*sd1*sd2
        ab2 <- ab2 + modval2 *  covre
      }
    }
    if(!is.null(args$random.mod.a) && !is.null(args$random.mod.b) &&
       args$random.mod.a && args$random.mod.b){
      if(boot){
        ab2 <- ab2 + (modval2^2) * v["re.SmX:WSyM:W"] # times covariance between re.mod.a and re.mod.b
      } else {
        correl <- as_draws_matrix(v, "cor_L2id__SmX:W__SyM:W")
        sd1 <- as_draws_matrix(v, "sd_L2id__SmX:W") # sd
        sd2 <- as_draws_matrix(v, "sd_L2id__SyM:W") # sd
        covre <- correl*sd1*sd2
        ab2 <- ab2 + (modval2^2) * covre
      }
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

