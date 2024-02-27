######################################################################
## Re-write of functions for use with bootstrapping
## These replace those used in development of code for the Falk et al
## publication.
##
## Copyright 2022-2024 Carl F. Falk
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

#' Bootstrapping multilevel mediation model (without boot package)
#'
#' @param data Data frame in long format. The function will do restructuring using \code{\link{stack_bpg}}.
#' @param L2ID Name of column that contains grouping variable in 'data' (e.g., "SubjectID")
#' @param ... Arguments passed to \code{\link{modmed.mlm}} or \code{\link[nlme]{lme}} to define the mediation analysis model or do estimation, respectively.
#' @param return.type Character that defines what information to extract from the model. Default and options are in \code{\link{extract.modmed.mlm}}.
#'   As examples, "indirect" will compute the indirect effect, "all" will save all random and fixed effects for possible additional
#'   computations, "indirect.diff" will compute the difference in the indirect effect at two values of a possible moderating variable.
#' @param modval1 (Optional) Numeric. If the model has a moderator, this value will be passed to \code{\link{extract.modmed.mlm}}
#'   to compute the indirect effect or other effects at that value. See \code{\link{extract.modmed.mlm}} for details.
#' @param modval2 (Optional). If the model has a moderator, it is possible to compute the difference in the indirect
#'   at two values of the moderator. If given and an appropriate option for such a difference is chosen for \code{type},
#'   this value and that of \code{modval1} will be passed to \code{\link{extract.modmed.mlm}} to compute and save the difference.
#'   This is useful for obtaining a CI for the difference in the indirect effect at two different levels of the moderator.
#' @param nrep Number of bootstrap replications to perform. Pick a small number just for testing purposes, something larger (e.g., 1000 or more) for analyses.
#' @param boot.type Character indicating the type of bootstrapping to perform. Options are: "caseboth", "case2", "case1", or "resid".
#' @param parallel.type Character indicating type of parallel processing (if any) to use. Options are "lapply" (no parallel processing),"parallel"
#' (uses \code{\link[parallel]{parallel}} package), or "furrr" (uses \code{\link[furrr]{furrr}} package.
#' @param ncores Integer indicating the number of processing cores to attempt to use.
#' @param seed Integer to set random number seed, for replication purposes. Note that replication may be somewhat R version or platform dependent.
#'
#' @details This function was written to do all four kinds of bootstrapping outlined in Falk, Vogel, Hammami & Miočević (in press):
#'  case resampling at both levels, at level 2 only, at level 1 only, and the residual-based bootstrap (e.g., see Hox and van de Schoot, 2013;
#'  van der Leeden, Meijer, & Busing, 2008). These functions also support moderated mediation. See also \code{\link{modmed.mlm}}.
#'  Note that \code{\link{nlm}} was used as the optimizer for some of the examples below as it was found to be faster for the models/simulations
#'  studied by Falk et al (in press). Note that Level 1 only bootstrapping is typically not recommended. See Falk et al. (in press) for details.
#'
#'  This function is different from the original functions used for the publication and that as of this writing still appear here: \code{\link{boot.modmed.mlm}}
#'  and here: \code{\link{bootresid.modmed.mlm}} . The present function seeks to unify case bootstrapping and residual-based bootstrapping in the same function. Furthermore,
#'  this newer function is also aimed at attempting to bypass the need for using the \code{boot} package to do computations and parallel processing.
#'  Some performance gains in terms of speed have been observed via use of this function instead of \code{boot} in conjunction with \code{\link{boot.modmed.mlm}}.
#'  Although somewhat slower, \code{\link[furrr]{furrr}} can also be used if one would like a progress bar.
#'
#' @return A list with the following elements. Note that \code{t0} and \code{t} are intended to trick the \code{\link[boot]{boot}}
#'   package into working with some if its functions.
#' \itemize{
#'  \item{\code{call} Call/arguments used when invoking this function. Useful for later extracting things like indirect effect.}
#'  \item{\code{t0} Parameter estimates based on the dataset.}
#'  \item{\code{t} Bootstrap distribution of all parameter estimates.}
#'  \item{\code{model} Fitted model to restructured data as one would obtain from \code{\link{modmed.mlm}}.}
#'  \item{\code{conv} Whether model fit to restructured dataset converged.}
#'  \item{\code{args} Arguments used when calling \code{\link{modmed.mlm}}. Useful for later extracting things like indirect effect.}
#' }
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
#' data(BPG06dat)
#'
#' # Note that for all examples below, nrep should be increased to something
#' #  MUCH larger (e.g., 1000). Small values here are used only so that the code
#' #  runs relatively quickly when tested.
#'
#' # double bootstrap, no parallel processing
#' boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=10, L2ID="id", X="x", Y="y", M="m",
#'   boot.type="caseboth",
#'   control=list(opt="nlm"), seed=1234)
#'
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' # residual bootstrap, parallel package
#' boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=10, L2ID="id", X="x", Y="y", M="m",
#'   boot.type="resid", random.a=TRUE, random.b=TRUE,
#'   parallel.type="parallel",ncores=2,seed=2299,
#'   control=list(opt="nlm"))
#'
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#' }
#'
#' \donttest{
#' # Example with moderation
#' data(simdat)
#'
#' # moderation
#' boot.result<-boot.modmed.mlm.custom(simdat, nrep=5, L2ID = "L2id", X = "X", Y = "Y", M = "M",
#'    boot.type="caseboth",
#'    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
#'    moderator = "mod", mod.a=TRUE, mod.b=TRUE,
#'    random.mod.a = TRUE, random.mod.b = TRUE,
#'   parallel.type="parallel",ncores=2,seed=2299)
#'
#' extract.boot.modmed.mlm(boot.result, type="indirect")
#'
#' # indirect effect point estimate and 95% CI when moderator = 0
#' extract.boot.modmed.mlm(boot.result, type="indirect", modval1=0)
#'
#' # indirect effect point estimate and 95% CI when moderator = 1
#'  extract.boot.modmed.mlm(boot.result, type="indirect", modval1=1)
#'
#' # indirect effect difference point estimate and 95% CI
#' extract.boot.modmed.mlm(boot.result, type="indirect.diff",
#'   modval1=0, modval2=1)
#'
#' }
#' @importFrom parallel makeCluster clusterSetRNGStream parLapply stopCluster
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multicore multisession
#' @importFrom parallelly supportsMulticore
#' @export boot.modmed.mlm.custom
boot.modmed.mlm.custom <- function(data, L2ID, ...,
                                   return.type="all",
                                   modval1=NULL, modval2=NULL,
                                   nrep=500,
                                   boot.type = c("caseboth","case2","case1","resid"),
                                   parallel.type=c("lapply","parallel","furrr"),
                                   ncores=NULL,
                                   seed=NULL)
{

  call <- match.call()

  boot.type <- match.arg(boot.type)
  parallel.type <- match.arg(parallel.type)

  # point estimate
  point<-modmed.mlm(data, L2ID, ..., returndata=T)

  boot.fun<-function(i){
    boot.modmed.mlm2(data, L2ID, ..., type=return.type,
                     modval1 = modval1, modval2 = modval2,
                     boot.type= boot.type,
                     model = point)
  }

  # do bootstrapping
  if(parallel.type=="lapply"){
    set.seed(seed)
    res <- lapply(1:nrep, boot.fun)
    res<-do.call("rbind",res)

  } else if (parallel.type=="parallel") {
    cl <- makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    res<-parLapply(cl, as.list(1:nrep), boot.fun)
    res<-do.call("rbind",res)
    stopCluster(cl)
  } else if (parallel.type=="furrr"){
    if(supportsMulticore()){
      plan(multicore, workers=ncores)
    } else {
      plan(multisession, workers=ncores)
    }
    res<-future_map(1:nrep, boot.fun,
                    .options = furrr_options(seed=seed),
                    .progress=TRUE)
    res<-do.call("rbind",res)
  }

  # output - designed to mimic boot output for compatibility with extract functions
  out <- list(call=call,
              t0 = extract.modmed.mlm(point,type=return.type,modval1=modval1,modval2=modval2),
              t = res,
              model = point$model,
              conv = point$conv,
              args = point$args
  )
}

# Custom model fitting function for two-level (moderated) mediation
# Currently hidden
boot.modmed.mlm2 <- function(data, L2ID, ...,
                             type="all",
                             modval1=NULL, modval2=NULL,
                             boot.type = c("caseboth","case2","case1","resid"),
                             model=NULL) {

  boot.type <- match.arg(boot.type)

  # manually apply case-wise resampling
  if(boot.type=="caseboth"){
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
    rdat <- do.call("rbind", rdat)
    result<-modmed.mlm(rdat,L2ID,...)
  } else if (boot.type == "case2") {
    # Resample L2 units
    L2 <- unique(data[, L2ID])
    N <- length(L2)
    L2_indices <- sample(L2, N, replace = TRUE)
    rdat <- lapply(L2_indices, function(x) {
      L2_sub <- data[data[, L2ID] == x, , drop = FALSE] # in case there is only 1 obs
      return(L2_sub)
    })
    rdat <- do.call("rbind", rdat)
    result<-modmed.mlm(rdat,L2ID,...)
  } else if (boot.type == "case1") {
    # No resampling of L2 units
    L2 <- unique(data[, L2ID])
    N <- length(L2)
    L2_indices <- L2
    # Resample L1 units
    rdat <- lapply(L2_indices, function(x) {
      L2_sub <- data[data[, L2ID] == x, , drop = FALSE] # in case there is only 1 obs
      n_j <- nrow(L2_sub)
      L1_idx <- sample(1:n_j, n_j, replace = TRUE)
      L2_sub <- L2_sub[L1_idx, ]
      return(L2_sub)
    })
    rdat <- do.call("rbind", rdat)
    result<-modmed.mlm(rdat,L2ID,...)
  } else if(boot.type=="resid"){

    ## Extract stuff from model and compute residuals
    #TODO: separate out this logic from that which does resampling?
    # This may require computing all of this stuff and then passing residuals to this function

    # fixed effects
    fe<-fixef(model$model)

    # L2
    l2groups<-unique(model$model$groups$L2id) # group IDs
    nl2<-length(l2groups) # N at l2
    l2resid <- random.effects(model$model) # random effects (l2 residuals)
    modvl2 <- randef.lme(model$model)$sig2 # extract var-cov of random effects

    # L1
    l1resid <- resid(model$model) # l1 residuals
    l1sig<-model$model$sigma # l1 error sd for Y
    l1varstruct<-model$model$modelStruct$varStruct # contains info about scaling of error for Y
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

    ## Do resampling
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
    tmp <- as.data.frame(model.matrix(model$model$terms, model$data))
    tmp$L2id <- model$data$L2id
    Zs<-lapply(l2groups, function(grp){
      tmpsub<-as.matrix(tmp[tmp$L2id %in% grp, colnames(bootcoef)])
      tmpcoef<-bootcoef[which(l2groups%in%grp), ]
      tmpsub%*%t(t(tmpcoef))
    })
    Zs<-do.call("c",Zs)

    # add Y and M to data frame
    rdat <- model$data
    rdat$Z<-Zs+l1resid.boot

    # estimate model
    result<-modmed.mlm(NULL, L2ID, data.stacked=rdat, ...)

  }
  row.names(rdat) <- NULL

  return(extract.modmed.mlm(result,type=type,modval1=modval1,modval2=modval2))
}


