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
#' @return A vector of parameter estimates, depending on the value of \code{type} specified as input. Once the model is estimated,
#'   \code{\link{extract.modmed.mlm} is used to obtain the parameter estimates.}
#' @references
#' Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing random indirect effects and moderated mediation in multilevel models: New procedures and recommendations. Psychological Methods, 11(2), 142–163. \doi{10.1037/1082-989X.11.2.142}
#'
#' Falk, C. F., Vogel, T., Hammami, S., & Miočević, M. (in press). Multilevel mediation analysis in R: A comparison of bootstrap and Bayesian approaches. Behavior Research Methods. \doi{10.3758/s13428-023-02079-4}  Preprint: \doi{10.31234/osf.io/ync34}
#'
#' Hox, J., & van de Schoot, R. (2013). Robust methods for multilevel analysis. In M. A. Scott, J. S. Simonoff & B. D. Marx (Eds.), The SAGE Handbook of Multilevel Modeling (pp. 387-402). SAGE Publications Ltd. \doi{10.4135/9781446247600.n22}
#'
#' van der Leeden, R., Meijer, E., & Busing, F. M. T. A. (2008). Resampling multilevel models. In J. de Leeuw & E. Meijer (Eds.), Handbook of Multilevel Analysis (pp. 401-433). Springer.
#' @examples
#'
#' \donttest{
#'
#' # Note that for all examples below, R should be increased to something
#' #  MUCH larger (e.g., 1000). Small values here are used only so that the code
#' #  runs relatively quickly when tested.
#'
#' ## Mediation for 1-1-1 model
#' data(BPG06dat)
#'
#' #Setup parallel processing
#' # snow appears to work on Windows; something else may be better on Unix/Mac/Linux
#' library(parallel)
#' library(boot)
#' ncpu<-2
#' RNGkind("L'Ecuyer-CMRG") # set type of random number generation that works in parallel
#' cl<-makeCluster(ncpu)
#' clusterSetRNGStream(cl, 9912) # set random number seeds for cluster
#'
#' # bootstrap all fixed and random effects (type="all")
#' boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=10,
#'   L2ID = "id", X = "x", Y = "y", M = "m",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   type="all",
#'   control=list(opt="nlm"),
#'   parallel="snow",ncpus=ncpu,cl=cl)
#'
#' # Point estimate and 95% CI for indirect effect
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' stopCluster(cl) # shut down cluster
#'
#' ## Moderated mediation
#'
#' data(simdat)
#'
#' # Note: use of nlm apparently fails in this moderated mediation model
#' # default optimizer for lme instead is used
#'
#' ## Bootstrap w/ moderation of a and b paths
#' set.seed(1234)
#'
#' boot.result2<-boot(simdat, statistic=boot.modmed.mlm, R=5,
#'  L2ID = "L2id", X = "X", Y = "Y", M = "M",
#'    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'    moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'    type="all")
#'
#' # indirect effect point estimate and 95% CI when moderator = 0
#' extract.boot.modmed.mlm(boot.result2, type="indirect")
#' extract.boot.modmed.mlm(boot.result2, type="indirect", modval1=0)
#'
#' # indirect effect point estimate and 95% CI when moderator = 1
#' extract.boot.modmed.mlm(boot.result2, type="indirect", modval1=1)
#'
#' # indirect effect difference point estimate and 95% CI
#' extract.boot.modmed.mlm(boot.result2, type="indirect.diff",
#'    modval1=0, modval2=1)
#'
#' # Example to not fail when using missing values
#' # See documentation for lme function from the nlme package for other
#' # options for na.action
#' dat.miss <- BPG06dat
#' dat.miss$m[c(1,2,3,4)]<-NA
#' dat.miss$y[c(5,6,7,8)]<-NA
#' boot.result<-boot(dat.miss, statistic=boot.modmed.mlm, R=5,
#'   L2ID = "id", X = "x", Y = "y", M = "m",
#'   random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   type="all",
#'   control=list(opt="nlm"),
#'   na.action = na.omit)
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
      L2 <- unlist(unique(data[, L2ID], use.names=FALSE))
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
      L2 <- unlist(unique(data[, L2ID], use.names=FALSE))
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
#' \donttest{
#' # Example data for 1-1-1 w/o moderation
#' data(BPG06dat)
#'
#' # Note that R should be set to something MUCH larger, such as 1000 or greater.
#' # A low number here is chosen only so testing this example code goes relatively
#' # quickly
#' bootresid <- bootresid.modmed.mlm(BPG06dat,L2ID="id", X="x", Y="y", M="m",
#'   R=5, random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'   control=list(opt="nlm")
#'   )
#'
#' extract.boot.modmed.mlm(bootresid, type="indirect")
#'
#'
#' }
#' @importFrom nlme random.effects
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

  if(inherits(init.mod$model, "glmmTMB")){
    stop("residual bootstrap not yet supported for glmmTMB")
  }

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
    for(grp in l2groups){
      tmpsub<-as.matrix(tmp2[tmp2$L2id %in% grp, colnames(bootcoef)])
      tmpcoef<-bootcoef[which(l2groups%in%grp), ]
      tmp2[tmp2$L2id %in% grp, "Z"] <- tmpsub%*%t(t(tmpcoef)) # add Y and M to data frame
    }

    # add l1 residuals
    tmp2$Z<-tmp2$Z+l1resid.boot
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