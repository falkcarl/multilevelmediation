#' Stacks data in the style of Bauer-Preacher-Gil for multilevel mediation
#'
#' @param data Data frame in long format.
#' @param L2ID (String) Name of column that contains grouping variable in \code{data} (e.g., \code{"SubjectID"}).
#' @param X (String) Name of column that contains the X independent variable in \code{data}.
#' @param Y (String) Name of column that contains the Y dependent variable in \code{data}.
#' @param M (String) Name of column that contains the M mediating variable in \code{data}.
#' @param moderator Optional Character that contains name of column that contains the moderator variable in \code{data}
#' @param covars.m (Character vector) Optional covariates to include in the model for M.
#' @param covars.y (Character vector) Optional covariates to include in the model for Y.
#' @details This is a convenience function used primarily internally by the package to restructure data in the style of
#'   Bauer, Preacher, and Gil (2006). The point is to allow both Y and M to be outcomes in a single column ("Z"),
#'   so that both mediator and outcome models can be fit at the same time. This is necessary to estimate the covariance
#'   between "a" and "b" paths at the same time when both have random effects. Two selector variables, "Sy" and "Sm" toggle
#'   whether each row corresponds to the outcome or the mediator, respectively.
#'
#' @return An object that is a subclass of \code{data.frame} is returned. In particular a \code{tbl_df} or "tibble."
#'
#'
#' So that coefficients extracted later from \code{\link{modmed.mlm}} hopefully make more sense, the below lists all variables
#'   (i.e., typical column names) for the data frame.
#'
#' \itemize{
#'  \item{\code{X} Independent variable.}
#'  \item{\code{L2id} Level 2 ID variable.}
#'  \item{\code{Md} Value of the mediator (not necessarily used due to restructuring, however).}
#'  \item{\code{Outcome} Whether the row corresponds to M or Y as the outcome.}
#'  \item{\code{Z} Value of the outcome variable.}
#'  \item{\code{Sy} Indicator variable for Y as outcome. 0 if Y is not outcome for this row. 1 if Y is outcome for this row. In model output, this is the intercept for Y.}
#'  \item{\code{Sm} Indicator variable for M as outcome. 0 if M is not outcome for this row. 1 if M is outcome for this row. In model output, this is the intercept for M.}
#'  \item{\code{SmX} Value of X when M is the outcome (literally X times Sm); will be 0 when Y is outcome. In model output, this is the "a" path.}
#'  \item{\code{SyX} Value of X when Y is the outcome (literally X times Sy); will be 0 when M is outcome. In model output, this is the "cprime" path (direct effect).}
#'  \item{\code{SyM} Value of M when Y is the outcome (literally M times Sy); will be 0 when M is the outcome. In model output, this is the "b" path.}
#'  \item{\code{W} Value of any moderating variable. This may show up in output later on as "SmX:W" (interaction with "a" path) or "SyM:W" (interaction with "b" path) or "SyX:W"
#'     (interaction with "cprime" path) depending on which path it moderates.}
#'  \item{If \code{covars.m} or \code{covars.y} are not null, any additional covariates will also be added to the data frame and their
#'     original names will be retained.}
#' }
#'
#' When \code{\link{modmed.mlm}} is used, any output with an "re" prefix will correspond to a random effect. Note that estimation with brms
#' will result in slightly different output than listed here. Coefficients typically have a "b_" prefix, and random effects are parameterized not
#' such that we end up with covariances, but using correlations and standard deviations for each effect.
#'
#' @references
#' Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing random indirect effects and moderated mediation in multilevel models: new procedures and	recommendations. Psychological Methods, 11(2), 142-163. \doi{10.1037/1082-989X.11.2.142}
#' @examples
#' \donttest{
#'
#' # restructure BPG data
#' data(BPG06dat)
#' dat <- stack_bpg(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
#' head(dat)
#'
#' # restructure simulated data w/ moderator
#' data(simdat)
#' dat2 <- stack_bpg(simdat,
#'   L2ID = "L2id", X = "X", Y = "Y", M = "M",
#'   moderator = "mod"
#' )
#' head(dat2)
#'
#' }
#' @importFrom tidyr pivot_longer
#' @export stack_bpg
#' @usage stack_bpg(data, L2ID, X, Y, M,
#'   moderator = NULL,
#'   covars.m = NULL,
#'   covars.y = NULL)
stack_bpg <- function(data, L2ID, X, Y, M, moderator = NULL, covars.m = NULL, covars.y = NULL) {
  # Check L2ID, X, Y, M exist in data
  if (!(L2ID %in% colnames(data))) {
    stop("L2ID variable not found in dataset. Please check specified name.")
  }
  if (!(X %in% colnames(data))) {
    stop("X variable not found in dataset. Please check specified name.")
  }
  if (!(Y %in% colnames(data))) {
    stop("Y variable not found in dataset. Please check specified name.")
  }
  if (!(M %in% colnames(data))) {
    stop("M variable not found in dataset. Please check specified name.")
  }

  # Stop if X, Y, or M variables in data are not numeric
  # factors not currently able to be used to set up BPG syntax for lme model (is there a possible workaround?)
  if (!is.numeric(data[[X]])) {
    stop("X is of type ", class(data[[X]]), ". Currently, only numeric X is supported.")
  }
  if (!is.numeric(data[[Y]])) {
    stop("Y is of type ", class(data[[Y]]), ". Y must be numeric to fit model.")
  }
  if (!is.numeric(data[[M]])) {
    stop("M is of type ", class(data[[M]]), ". M must be numeric to fit model.")
  }

  # Use data frame with only relevant variables
  tmp <- data.frame(
    X = data[[X]],
    Y = data[[Y]],
    M = data[[M]],
    L2id = data[[L2ID]], # Save copy of the grouping (Level 2) variable
    Md = data[[M]] # save copy of mediator
  )

  # Check and save moderator if necessary
  if (!is.null(moderator)) {
    # Check validity of moderator
    if (length(moderator) > 1L) {
      stop("Only 1 moderator variable is allowed.")
    }
    if (!(moderator %in% colnames(data))) {
      stop("Moderator not found in dataset. Please check specified name.")
    }
    if (!is.numeric(data[[moderator]])) {
      stop("Moderator is of type ", class(data[[moderator]]), ". Currently, only numeric moderators are supported.")
    }
    if (moderator %in% c(X, Y, M, covars.m, covars.y, L2ID)) {
      stop("Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables.")
    }

    # Save copy of the moderator
    tmp$W <- data[[moderator]]
  }

  # Add covariates for M and Y outcome paths
  # (covars.m and covars.y should be a vector of column names in the dataset)
  if (!is.null(covars.m)) {
    # Check validity of m covariates
    if (!all(covars.m %in% colnames(data))) {
      stop("One or more covariates not found in dataset. Please check specified name(s).")
    }
    if (!all(sapply(data[covars.m], is.numeric))) {
      stop("One or more covariates is not numeric. Currently, only numeric covariates are supported.")
    }
    if (any(covars.m %in% c(X, Y, M, moderator, L2ID))) {
      stop("Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.")
    }

    # Save copy of covariate data
    tmp[covars.m] <- data[covars.m] #will add same variable twice if same name in list (eg c("cov1", "cov2", "cov2")) (although renames by adding a ".1" to it, so may not be a problem when lme is run?)
  }
  if (!is.null(covars.y)) {
    # Check validity of y covariates
    if (!all(covars.y %in% colnames(data))) {
      stop("One or more covariates not found in dataset. Please check specified name(s).")
    }
    if (!all(sapply(data[covars.y], is.numeric))) {
      stop("One or more covariates is not numeric. Currently, only numeric covariates are supported.")
    }
    if (any(covars.y %in% c(X, Y, M, moderator, L2ID))) {
      stop("Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.")
    }

    # Save copy of covariate data (still works if same covar is named for both m and y outcomes)
    tmp[covars.y] <- data[covars.y]
  }

  # restructure data such that both m and y are in the Z column
  tmp <- tidyr::pivot_longer(tmp, cols = c(Y, M), names_to = "Outcome", values_to = "Z")

  # create variables similar to Bauer et al. syntax
  tmp$Sy <- as.numeric(tmp$Outcome == "Y")
  tmp$Sm <- as.numeric(tmp$Outcome == "M")
  tmp$SmX <- tmp$Sm * tmp$X
  tmp$SyX <- tmp$Sy * tmp$X
  tmp$SyM <- tmp$Sy * tmp$Md

  return(tmp)
}
