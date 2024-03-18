#' Stacks data in the style of Bauer-Preacher-Gil for multilevel mediation
#'
#' @param data Data frame in long format.
#' @param L2ID (String) Name of column that contains grouping variable in \code{data} (e.g., \code{"SubjectID"}).
#' @param X (String) Name of column that contains the X independent variable in \code{data}.
#' @param Y (String) Name of column that contains the Y dependent variable in \code{data}.
#' @param M (String) Name of column that contains the M mediating variable in \code{data}.
#' @param moderator Optional Character that contains name of column that contains the moderator variable in \code{data}
#' @param covars.m (Character vector; deprecated) Optional covariates to include in the model for M.
#' @param covars.y (Character vector; deprecated) Optional covariates to include in the model for Y.
#' @param covars (Character vector) Optional covariates to include in either the model for M or for Y.
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
#'  \item{\code{SyM} Value of M when Y is the outcome (literally M times Sy); will be 0 when M is the outcome. In model output, this is the "a" path.}
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
#'
#' dat <- stack_bpg(BPG06dat,
#'   "id", "x", "y", "m"
#' )
#'
#' head(dat)
#'
#' # restructure simulated data w/ moderator
#' data(simdat)
#' dat2 <- stack_bpg(simdat,
#'   "L2id", "X", "Y", "M",
#'   moderator = "mod"
#' )
#'
#' head(dat2)
#'
#' }
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select mutate rename
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @export stack_bpg
stack_bpg <-function(data, L2ID, X, Y, M,
                     moderator = NULL,
                     covars.m = NULL,
                     covars.y = NULL,
                     covars = NULL){

  # Stop if X, Y, or M variables in data are not numeric
  # factors not currently able to be used to set up BPG syntax for lme model
  if (!is.numeric(data[[X]])) {stop("X is of type ", class(data[[X]]), ". Currently, only numeric X is supported.")}
  if (!is.numeric(data[[Y]])) {stop("Y is of type ", class(data[[Y]]), ". Y must be numeric to fit model.")}
  if (!is.numeric(data[[M]])) {stop("M is of type ", class(data[[M]]), ". M must be numeric to fit model.")}

  # Save cols that we'll need
  # Selection now at end
  cols <- c(L2ID, X, Y, M)

  # Check and save moderator if necessary
  if (!is.null(moderator)) {
    # Check name and data type of moderator
    if (!(moderator %in% colnames(data))) {stop("Moderator not found in dataset. Please check specified name.")}
    if (!is.numeric(data[[moderator]])) {stop("Moderator is of type ", class(data[[moderator]]), ". Currently, only numeric moderators are supported.")}

    cols <- c(cols, moderator)

  }

  # check and save covariate
  if (!is.null(covars)) {
    # Check if covariate names are the same as existing vars
    if (any(covars %in% c(X, Y, M, moderator))) {
      stop("Covariate(s) have the same name as X, Y, M, or moderator variables.")
    }
    cols <- c(cols, covars)
  }

  # below deprecated, but we want to give time for those who use it to switch to newer syntax

  # Add covariates for M and Y outcome paths
  # (covars.m and covars.y should be a vector of column names in the dataset)
  if (!is.null(covars.m)) {
    warning("covars.m with stack_bpg is deprecated and will be replaced with use of just covars in future package releases")
    # Check if covariate names are the same as existing vars
    if (any(covars.m %in% c(X, Y, M, moderator))) {
      stop("Covariate(s) have the same name as X, Y, M, or moderator variables.")
    }
    # Save copy of covariate data
    #tmp[covars.m] = data[covars.m] #will add same variable twice if same name in list (eg c("cov1", "cov2", "cov2")) (although renames by adding a ".1" to it, so may not be a problem when lme is run?)

    cols <- c(cols, covars.m)
  }
  if (!is.null(covars.y)) {
    warning("covars.y with stack_bpg is deprecated and will be replaced with use of just covars in future package releases")
    # Check if covariate names are the same as existing vars
    if (any(covars.y %in% c(X, Y, M, moderator))) {
      stop("Covariate(s) have the same name as X, Y, M, or moderator variables.")
    }
    # Save copy of covariate data (still works if same covar is named for both m and y outcomes)
    #tmp[covars.y] = data[covars.y]
    cols <- c(cols, covars.y)
  }

  # end deprecated

  # just variables we need
  tmp <- dplyr::select(data, all_of(cols))
  tmp["Md"] = tmp[M] # make copy of  mediator

  # moderator: use naming convention
  if (!is.null(moderator)) {
    tmp <- rename(tmp, W = moderator)
  }

  # restructure data such that both m and y are in the Z column
  tmp <- pivot_longer(tmp, cols = c(Y, M), names_to = "Outcome",
                      values_to = "Z")

  # create variables similar to Bauer et al syntax; enforce naming conventions
  tmp <- rename(tmp, X = X,
                L2id = L2ID)
  tmp <- mutate(tmp,
                Sy = ifelse(.data$Outcome == Y, 1, 0),
                Sm = ifelse(.data$Outcome == M, 1, 0))
  tmp <- mutate(tmp,
                SmX = .data$Sm * .data$X,
                SyX = .data$Sy * .data$X,
                SyM = .data$Sy * .data$Md)

  return(tmp)
}


