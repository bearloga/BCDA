#' @title Tidy the model into a summary data.frame
#' @name tidy
#' @rdname tidy
#' @export
#' @importFrom broom tidy
NULL

#' @title Construct a single row summary "glance" of a model, fit, or other object
#' @name glance
#' @rdname glance
#' @export
#' @importFrom broom glance
NULL

#' @name glance
#' @export
glance.beta_binomial_fit <- function(x, ...) {
  return(present_bbfit(x, raw = TRUE, fancy_names = TRUE, ...))
}

#' @export
print.beta_binomial_fit <- function(x, ...) {
  print(tidy(x, ...))
}

#' @export
summary.beta_binomial_fit <- function(x, ...) {
  format(present_bbfit(x, raw = TRUE, ...))
}
