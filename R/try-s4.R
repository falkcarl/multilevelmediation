################################################################################
# Classes

setClass("medmlmDat",
         slots = c(vars = "list",       # vars used to set up stacked data
                   data = "data.frame", # stores the stacked data
                   misc = "list" # contains optional function for manipulation
         )
)

setClass("medmlmDef",
         slots = c(estimator = "character", # program assumed when setting up syntax
                   opts = "list",   # stores information about paths, random effects, etc.
                   formula = "character", # model equations
                   random = "character" # separate model equations for random effects, if needed
         )
)

setClass("medmlmEst",
         slots = c(results = "list",              # stores fitted model
                   estimator = "character",   # program used to estimate model
                   conv = "list",             # information about convergence
                   opts = "list"           # arguments used in estimation
         )
)

setClass("medmlm",
         slots = c(fit     = "medmlmEst", # Store fitted model
                   def       = "medmlmDef", # Model definition
                   dat       = "medmlmDat"  # Restructured data
         ))



################################################################################
# Data

medmlmDat <- function(data, L2ID, X, Y, M,
                      moderator=NULL, covars.m=NULL, covars.y=NULL,
                      data.stacked=NULL,
                      datmfun=NULL){

  # variables should not duplicate reserved variable names
  if(any(c(covars.m,covars.y) %in% c("Sy","Sm","SmX","SyX","SyM","W"))){
    stop("Some covariate names overlap with reserved variable names for the model:
         Sy,Sm,SmX,SyX,SyM,W")
  }

  vars = list(L2ID = L2ID,
                          X = X,
                          Y = Y,
                          M = M,
                          moderator = moderator,
                          covars = union(covars.m,covars.y),
                          covars.m = covars.m,
                          covars.y = covars.y)

  # process setup of data
  if(is.null(data.stacked)){
    stacked.data <- stack_bpg(data=data, L2ID=L2ID, X=X, Y=Y, M=M,
                          moderator=moderator,
                          covars=vars$covars)
  }

  # further data manipulation, if present
  if(!is.null(datmfun)){
    if(!is.function(datmfun)){
      stop("datmfun should be a function")
    }
    stacked.data <- datmfun(stacked.data)
  }

  # some additional checks
  # moderator, if present, should be W in data
  if(!is.null(moderator) & !"W" %in% colnames(stacked.data)){
    stop("moderator not found in stacked dataset")
  }
  # covars should be in data
  if((!is.null(covars.m) | !is.null(covars.y)) & !all(vars$covars %in% colnames(stacked.data))){
    stop("Some covars not found in dataset")
  }

  dat <- new("medmlmDat",
             vars = vars,
             data = stacked.data,
             misc = list(datmfun=datmfun))

  return(dat)

}

################################################################################
# Definition

# Model definition function
#TODO: still needs to be reworked with revamp of medmlmEqs
medmlmDef <- function(obj, estimator = c("lme","glmmTMB","brms"), ...){

  estimator <- match.arg(estimator)

  #opts <- medmlmOpts(obj@dat@vars, ...)

  # Obtain model equations
  #FIXME: options passed to here and returned
  eqs <- medmlmEq(estimator = estimator, ...)
  opts <- eqs$opts

  def <- new("medmlmDef",
     estimator = estimator,
     opts = opts,
     formula = eqs$fixed,
     random = eqs$random)

  return(def)

}

# model equations
#TODO: experiment with equations for separate models
medmlmEq <- function(estimator = c("lme","glmmTMB","brms"),
                     outcome = "Z",
                     L2ID = "L2id",
                     intM = "Sm", intY = "Sy",
                     a = "SmX", b = "SyM", cprime = "SyX",
                     ...
                     ){

  estimator = match.arg(estimator)

  opts <- medmlmOpts(...)

  #TODO: check variable names
  varnames <- list(outcome = outcome,
               L2ID = L2ID,
               intM = intM,
               intY = intY,
               a = a,
               b = b,
               cprime = cprime)

  # Create the formula for the fixed effects
  fixed.formula <- paste(outcome, "~ 0 +", paste(c(intM,intY,a,b,cprime), collapse=" + "))

  # Add in the moderator to the paths if necessary
  # Note: interactions w/ "W" must must use selector variables in this way
  if (opts$mod.a == TRUE) {fixed.formula <- paste(fixed.formula, paste0(intM,":",opts$moderator," + ",a,":",opts$moderator), sep=" + ")}
  if (opts$mod.b == TRUE || opts$mod.cprime == TRUE) {
    #if b or c path is moderated, Sy component will always be there (prevents adding redundant parameters if both b & c are moderated)
    fixed.formula <- paste(fixed.formula, paste0(intY,":",opts$moderator), sep=" + ")
    if (opts$mod.b == TRUE && opts$mod.cprime == TRUE) {fixed.formula <- paste(fixed.formula, paste0(b,":",opts$moderator," + ",cprime,":",opts$moderator), sep=" + ")}
    if (opts$mod.b == TRUE && opts$mod.cprime == FALSE) {fixed.formula <- paste(fixed.formula, paste0(b,":",opts$moderator), sep = " + ")}
    if (opts$mod.b == FALSE && opts$mod.cprime == TRUE) {fixed.formula <- paste(fixed.formula, paste0(cprime,":",opts$moderator), sep = " + ")}
  }

  # Add any covariates to the paths if necessary
  if (length(opts$covars.m)>0) {
    covars.m_formula <-  paste0(intM, ":", opts$covars.m, collapse=" + ") #write the formula for each covar specified
    fixed.formula <- paste(fixed.formula, covars.m_formula, sep = " + ")      # and add to main fixed fx formula
  }
  if (length(opts$covars.y)>0) {
    covars.y_formula <-  paste0(intY, ":", opts$covars.y, collapse=" + ") #write the formula for each covar specified
    fixed.formula <- paste(fixed.formula, covars.y_formula, sep = " + ")      #and add to main fixed fx formula
  }

  # Create the formula for the random effects
  random.formula <- "0"

  if (opts$random.int.m) {random.formula <- paste0(random.formula, " + ", intM)}
  if (opts$random.int.y) {random.formula <- paste0(random.formula, " + ", intY)}
  if (opts$random.a) {random.formula <- paste0(random.formula, " + ", a)}
  if (opts$random.b) {random.formula <- paste0(random.formula, " + ", b)}
  if (opts$random.cprime) {random.formula <- paste0(random.formula, " + ", cprime)}

  # Add random effects for moderator here, if any
  if(opts$random.mod.a && opts$mod.a){random.formula <- paste0(random.formula, " + ", a, ":", opts$moderator)}
  if(opts$random.mod.b && opts$mod.b){random.formula <- paste0(random.formula, " + ", b, ":", opts$moderator)}
  if(opts$random.mod.cprime && opts$mod.cprime){random.formula <- paste0(random.formula, " + ", cprime, ":", opts$moderator)}
  if(opts$random.mod.m && opts$mod.a){random.formula <- paste0(random.formula, " + ", intM, ":", opts$moderator)}
  if(opts$random.mod.y && opts$mod.b){random.formula <- paste0(random.formula, " + ", intY, ":", opts$moderator)}

  # Add random effects for covariates here, if any
  #TODO: TV: currently doesn't check whether random covariates have the same
  # name as other variables in the model, or are the same covariates in the fixed fx formula.
  if (length(opts$random.covars.m)>0) {
    random.covars.m_formula <- paste0(intM, ":", opts$random.covars.m, collapse=" + ")
    random.formula <- paste(random.formula, random.covars.m_formula, sep = " + ")
  }
  if (length(opts$random.covars.y)>0) {
    random.covars.y_formula <- paste0(intY, ":", opts$random.covars.y, collapse=" + ")
    random.formula <- paste(random.formula, random.covars.y_formula, sep = " + ")
  }

  # Add in the grouping variable after all the variables are entered
  random.formula <- paste(random.formula, "|", L2ID)

  if(estimator == "lme"){
    random.formula <- paste("~", random.formula)
  } else if (estimator %in% c("glmmTMB","brms")){
    random.formula <- paste("(",random.formula)
    random.formula <- paste(random.formula,")")
    fixed.formula <- paste(fixed.formula,"+",random.formula)
    random.formula <- character()
  }

  return(list(estimator = estimator,
              opts = opts,
              varnames = varnames,
              fixed = fixed.formula,
              random = random.formula))
}

#' @importFrom utils modifyList
#' @importFrom rlang dots_list
medmlmOpts <- function(...){

  # set up defaults
  opts <- medmlmOpts.default()

  # obtain any passed args
  args <- rlang::dots_list(...)

  # stop if any nonvalid options
  keys <- setdiff(names(args), names(opts))
  if(length(keys)>0){
    stop(paste0("Invalid options found:", keys))
  }

  # override
  opts <- modifyList(opts, args)

  # check for incongruent options
  check.medmlmOpts(opts)

  return(opts)

}

# Options regarding which terms are present in order
# to set up model equations
medmlmOpts.default <- function(){

  opts <- list(

    # re for typical effects
    random.int.m = TRUE,
    random.int.y = TRUE,
    random.a = FALSE,
    random.b = FALSE,
    random.cprime = FALSE,

    # moderation
    moderator = NULL,
    mod.a = FALSE,
    mod.b = FALSE,
    mod.cprime = FALSE,
    random.mod.m = FALSE,
    random.mod.y = FALSE,
    random.mod.a = FALSE,
    random.mod.b = FALSE,
    random.mod.cprime = FALSE,

    # covariates
    covars.m = character(),
    covars.y = character(),
    random.covars.m = character(),
    random.covars.y = character()

  )

  return(opts)
}

check.medmlmOpts <- function(opts){

  # check everything that ought to be logical
  if(with(opts, !all(is.logical(c(random.a,random.b,random.cprime,random.mod.a,random.mod.b,random.mod.cprime,
                                  random.mod.m, random.mod.y,
                                  random.int.m,random.int.y,
                                  mod.a,mod.b,mod.cprime))))){
    stop("One or more options that should be logical is not so.")
  }

  # check everything that ought to be character vector
  if(with(opts, !all(is.character(c(random.covars.m, random.covars.y))))){
    stop("One or more options that should be a character (vector) is not so.")
  }

  # moderator needs to be specified for mod.a, mod.b, mod.cprime to be TRUE
  # still just a single moderator for now
  if(is.null(opts$moderator) & any(c(opts$mod.a,opts$mod.b,opts$mod.cprime))){
    stop("moderator cannot be null if mod.a, mod.b, or mod.cprime are specified as TRUE")
  }
  if(length(opts$moderator) > 1){
    stop("Only one moderator is currently supported")
  }

  # moderation should be enabled for relevant random effects to be TRUE
  if(opts$random.mod.a & !opts$mod.a){stop("random.mod.a specified as TRUE but mod.a is FALSE")}
  if(opts$random.mod.b & !opts$mod.b){stop("random.mod.b specified as TRUE but mod.b is FALSE")}
  if(opts$random.mod.cprime & !opts$mod.cprime){stop("random.mod.cprime specified as TRUE but mod.cprime is FALSE")}
  if(opts$random.mod.m & !opts$mod.a){stop("random.mod.m specified as TRUE but mod.a is FALSE")}
  if(opts$random.mod.y & !opts$mod.b){stop("random.mod.y specified as TRUE but mod.b is FALSE")}

  # covars must be present for relevant random effects to be enabled
  if(length(opts$covars.m) != length(opts$random.covars.m)){stop("random.covars specified as TRUE but covars is empty")}
  if(length(opts$covars.y) != length(opts$random.covars.y)){stop("random.covars.y specified as TRUE but covars.y is FALSE")}

}

################################################################################
# Estimation

medmlmEst <- function(obj, method=c("REML","ML"), control, estopts){

  if(!is(obj, "medmlm")){stop("Object should be of class medmlm")}

  method <- match.arg(method)

  estimator <- obj@def@estimator

  control <- control.defaults(estimator, control)

  if(estimator == "lme"){
    fit <- try(do.call(lme,
               c(list(fixed = as.formula(obj@def@formula), # fixed effects
                      random = as.formula(obj@def@random), # random effects
                      weights = varIdent(form = ~ 1 | Sm), # heteroskedasticity
                      data = obj@dat@data,
                      method = method,
                      control = control),
                 estopts)))
    conv <- ifelse(inherits(fit, "try-error"), FALSE, TRUE)
    # lme will throw an error if not converged

  } else if (estimator == "glmmTMB"){
    fit <- try(do.call(glmmTMB,
                       c(list(formula = as.formula(obj@def@formula),
                       dispformula =  ~ 1 + Sm,
                       #dispformula =  ~ 0 + Sm + Sy,
                       family = gaussian,
                       data = obj@dat@data,
                       REML = (method=="REML"),
                       control = control),
                       estopts)))

    # if not converged, glmmTMB will...
    #fit$fit$convergence # convergence code saved here. 0 for ok, 1 otherwise
    conv <- ifelse(fit$fit$convergence == 0, TRUE, FALSE)

  } else if (estimator == "brms"){
    fit <- try(do.call(brm,
                       c(list(formula = bf(as.formula(obj@def@formula),
                                           sigma ~ 0 + Sm + Sy),
                                data = obj@dat@data,
                                family = gaussian,
                                control = control),
                         estopts)))
    conv <- all(rhat(fit)<1.01)
  }

  out <- new("medmlmEst",
             results = list(model = fit),
             estimator = estimator,
             conv = list(conv = conv),
             opts = list(estopts = estopts,
                         control = control)
             )

  return(out)

}

control.defaults <- function(estimator = c("lme","glmmTMB","brms"), control=NULL){

  estimator <- match.arg(estimator)

  if(estimator == "lme"){
    ctrl <- lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
                          msMaxEval = 10000, tolerance = 1e-6)
    if(!is.null(control)){
      ctrl <- modifyList(ctrl, control)
    }
  } else if (estimator == "glmmTMB"){
    ctrl <- glmmTMBControl()
    if(!is.null(control)){
      ctrl <- modifyList(ctrl, control)
    }
  } else if (estimator == "brms"){
    ctrl <- control
  }

  return(ctrl)
}



################################################################################
# Main constructor

# Creates a modmedmlm object
# - Restructures data, if necessary
# - Then formulas/equations used for all approaches
# - Then optionally fits the model to create the model
#TODO: have spaces for slots to be passed to function
modmed.mlm2 <- function(data, L2ID, X, Y, M,
                       moderator = NULL, covars.m = NULL, covars.y = NULL,
                       mod.a = FALSE, mod.b = FALSE, mod.cprime = FALSE,
                       estimator = c("lme","glmmTMB","brms"),
                       method= c("REML","ML"), control = NULL,
                       estopts = list(),
                       datmfun = NULL,
                       data.stacked = NULL,
                       ...,
                       datslot = NULL,
                       defslot = NULL){

  obj <- new("medmlm")

  # set up data
  if(!is.null(datslot)){
    obj@dat <- datslot
  } else {
    obj@dat <- medmlmDat(data=data, L2ID=L2ID, X=X, Y=Y, M=M,
                         moderator=moderator, covars.m=covars.m, covars.y=covars.y,
                         data.stacked=data.stacked,
                         datmfun=datmfun)
  }

  # set up model definition here
  if(!is.null(defslot)){
    obj$def <- defslot
  } else {
    obj@def <- medmlmDef(obj, estimator, ...)
  }

  # estimate model
  obj@fit <- medmlmEst(obj, method=method, control=control, estopts=estopts)

  return(obj)
}

################################################################################
# Access / summary

inspect.medmlm <- function(obj, what){

  if(!is(obj, "medmlm")){stop("Object should be of class medmlm")}

  # data
  if (what=="data.stacked"){
    out <- obj@dat@data # stacked dataset
  } else if (what=="vars"){
    out <- obj@dat@vars # variables used to set up data
  } else if (what=="datmfun"){
    out <- obj@dat@misc$datmfun # optional function for data manipulation

  # def
  } else if (what=="defest"){
    out <- obj@def@estimator # estimator assumed when setting up model equations
  } else if (what=="defopts"){
    out <- obj@def@opts # options when setting up model equations
  } else if (what=="formula"){
    out <- obj@def@formula # (fixed) formula for the model
  } else if (what=="randformula"){
    out <- obj@def@random # random effects formula for the model

  # model
  } else if(what == "conv"){
    out <- obj@fit@conv$conv # convergence
  } else if (what=="model"){
    out <- obj@fit@results$model # fitted model
  } else if (what == "fitest"){
    out <- obj@fit@estimator # program that was actually used for estimation
  } else if (what == "estopts"){
    out <- obj@fit@opts # estimation options used

  # else
  } else {
    print("Unrecognized option for extraction. Is the value of 'what' correct?")
  }

  return(out)
}



