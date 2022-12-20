######################################################################
## Re-write of functions for use with bootstrapping
## These replace those used in development of code for the Falk et al
## publication.
##
## Copyright 2022 Carl F. Falk
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

#' start re-write of bootstrapping functions
#'
#' @param data placeholder
#' @param L2ID placeholder
#' @param ... placeholder
#' @param return.type placeholder
#' @param modval1 placeholder
#' @param modval2 placeholder
#' @param nrep placeholder
#' @param boot.type placeholder
#' @param parallel.type placeholder
#' @param ncores placeholder
#' @param seed placeholder
#'
#' @details This function shall replace initially written bootstrap functions.
#'   The function appears to be finished, but documentation and examples remain.
#'
#' @examples
#' \donttest{
#'
#' data(BPG06dat)
#'
#' # double bootstrap, no parallel processing
#' boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=50, L2ID="id", X="x", Y="y", M="m",
#'   boot.type="caseboth",
#'   control=list(opt="nlm"))
#'
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' # level 2 bootstrap, parallel package
#' boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=100, L2ID="id", X="x", Y="y", M="m",
#'   boot.type="case2",
#'   parallel.type="parallel",ncores=2,seed=2299,
#'   control=list(opt="nlm"))
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' # residual bootstrap, no parallel processing
#' boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=100, L2ID="id", X="x", Y="y", M="m",
#'   boot.type="resid", random.a=TRUE, random.b=TRUE,
#'   control=list(opt="nlm"))
#'
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' # residual bootstrap, parallel package
#' boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=100, L2ID="id", X="x", Y="y", M="m",
#'   boot.type="resid", random.a=TRUE, random.b=TRUE,
#'   parallel.type="parallel",ncores=2,seed=2299,
#'   control=list(opt="nlm"))
#'
#' extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95)
#'
#' # Examples/tests with moderation
#' data(simdat)
#'
#' # moderation
#' boot.result<-boot.modmed.mlm.custom(simdat, nrep=100, L2ID = "L2id", X = "X", Y = "Y", M = "M",
#'    boot.type="resid",
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
                                   seed=1234)
{

  call <- match.call()

  boot.type <- match.arg(boot.type)
  parallel.type <- match.arg(parallel.type)

  # point estimate
  point<-modmed.mlm(data,L2ID,...,returndata=T)

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
# @param data placeholder
# @param L2ID placeholder
# @param ... placeholder
# @param type placeholder
# @param modval1 placeholder
# @param modval2 placeholder
# @param boot.type placeholder
# @param model placeholder
# @importFrom stats model.matrix
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
    })
    rdat <- do.call("rbind", rdat)
    result<-modmed.mlm(rdat,L2ID,...)
  } else if(boot.type=="resid"){

    ## Extract stuff from model and compute residuals
    #TODO: separate out this logic from that which does resampling
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
    result<-modmed.mlm(NULL,L2ID, data.stacked=rdat,...)

  }
  row.names(rdat) <- NULL

  return(extract.modmed.mlm(result,type=type,modval1=modval1,modval2=modval2))
}


