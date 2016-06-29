#' Bayesian analysis of categorical data
#'
#' @references
#' Agresti, A. and Hitchcock, D. B. (2005). Bayesian inference for categorical
#'   data analysis. \emph{Statistical Methods & Applications}, \strong{14},
#'   297-330. doi:10.1007/s10260-005-0121-y
#' @importFrom broom tidy
#' @docType package
#' @name BCDA
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("For article references, see ?BCDA")
}
