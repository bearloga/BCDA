#' @title Analyze a 2x2 table with a log-linear model.
#' @description Fits a Bayesian Generalized Linear Model to the contingency
#'   table.
#' @param data A two-way table of counts.
#' @param dim_names Names of the variables.
#' @param n_sims Number of simulations, used in Bayes factor computation via
#'   importance sampling.
#' @param n_burn Number of burn-in simulations to throw away.
#' @param n_thin Amount of thinning to use.
#'
#' @examples
#' data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(data) <- c('Safe' ,'Dangerous')
#' rownames(data) <- c('Animals', 'Plants')
#'
#' fit <- log_linear(data, c('kingdom', 'safety'))
#' conting::summary.bcct(fit)
#' @importFrom conting bcct
#' @export
log_linear <- function(data, dim_names, n_sims = 1100, n_burn = 100, n_thin = 1) {
  dim_names <- tolower(sub(' ', '_', dim_names, fixed = TRUE))
  if ( is.matrix(data) ) { data <- as.table(data) }
  data2 <- as.data.frame(data)
  colnames(data2) <- c(dim_names, 'y')
  fit <- bcct(as.formula(sprintf("y ~ (%s)^%.0f",
                                 paste0(dim_names, collapse = " + "),
                                 length(dim_names))),
              data = data2, n.sample = n_sims)
  return(fit)
}
