#' @title Binomial Model with Beta Priors
#'
#' @description
#'
#' @param x A numeric vector of success counts OR a 2x2 table or matrix.
#' @param n A numeric vector of totals. (Optional if x is a table or matrix.)
#' @param prior Parameters for independent beta priors. See \strong{Details}.
#' @param n_sims Number of draws (simulations) to make from the posterior
#'   distributions.
#' @return An S3 object of class "beta_binomial_fit".
#' @details The model assumes
#'   \deqn{x_i ~ Binomial(n_i, p_i); p_1 ~ Beta(a, b); p_2 ~ Beta(c, d)}
#'   for \eqn{i = 1, 2}. The default is \code{a = b = c = d = 1/2} which
#'   corresponds to a Jeffreys prior on \eqn{p_1} and \eqn{p_2}.
#'
#' @examples \dontrun{
#' data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(data) <- c('Safe' ,'Dangerous')
#' rownames(data) <- c('Animals', 'Plants')
#'
#' beta_binom(x = data[, 'Safe'], n = margin.table(data, 1))
#'
#' beta_binom(x = c(200, 250), n = c(350, 550))
#' }
#' @export

beta_binom <- function(x, n = NULL, prior = c(a = 0.5, b = 0.5, c = 0.5, d = 0.5), n_sims = 1e4) {
  data <- check_inputs(x, n); x <- unname(data[, 1]); n <- as.numeric(unname(margin.table(data, 1)))
  new_a <- unname(x[1] + prior['a']); new_b <- unname(n[1] - x[1] + prior['b'])
  new_c <- unname(x[2] + prior['c']); new_d <- unname(n[2] - x[2] + prior['d'])
  sims <- data.frame(p1 = rbeta(n_sims, new_a, new_b),
                     p2 = rbeta(n_sims, new_c, new_d),
                     stringsAsFactors = FALSE)
  sims$`prop_diff` <- sims$p1 - sims$p2
  sims$`relative_risk` <- sims$p1/sims$p2
  sims$odds_ratio <- (sims$p1/(1-sims$p1))/(sims$p2/(1-sims$p2))
  return(structure(list(posterior_simulations = invisible(sims),
                        posterior_parameters = invisible(c('a' = new_a, 'b' = new_b,
                                                           'c' = new_c, 'd' = new_d))),
                   class = "beta_binomial_fit"))
}

#' @title Update the Beta-Binomial model posterior with new data
#' @description Uses the previously computed Beta posterior as prior to compute
#'   a new Beta posterior, which can subsequently be updated as even more data
#'   becomes available.
#' @param object An object of class "beta_binomial_fit".
#' @param x A numeric vector of success counts OR a 2x2 table or matrix.
#' @param n A numeric vector of totals. (Optional if x is a table or matrix.)
#' @param sims Number of draws (simulations) to make from the posterior
#'   distributions.
#' @return An S3 object of class "beta_binomial_fit".
#' @export
update.beta_binomial_fit <- function(object, x, n = NULL, n_sims = 1e3) {
  data <- check_inputs(x, n); x <- unname(data[, 1]); n <- as.numeric(unname(margin.table(data, 1)))
  return(beta_binom(x, n, object$posterior_parameters, n_sims))
}

#' @title Summarize the posterior draws from a fitted Beta-Binomial model
#' @description Outputs a data frame of point estimates and Bayesian confidence
#'   intervals for group proportions, proportion difference, relative risk, and
#'   odds ratio.
#' @param x An object of class "beta_binomial_fit".
#' @param conf_level Probability level for credible intervals. 95\% by default.
#' @param interval_type Method for computing intervals ("quantile" or "HPD").
#' @return A data frame consistent with the output of David Robinson's
#'   \code{broom::tidyMCMC()} tidying method:
#'   \describe{
#'     \item{estimate}{Point estimate (mean of posterior draws).}
#'     \item{std.error}{Standard error.}
#'     \item{conf.low}{Credible interval lower bound.}
#'     \item{conf.high}{Credible interval upper bound.}
#'   }
#' @examples \dontrun{
#' summary(beta_binom(x = c(200, 250), n = c(350, 550)))
#' }
#' @export
summary.beta_binomial_fit <- function(x, conf_level = 0.95, interval_type = c("quantile", "HPD"), ...) {
  if (!requireNamespace("coda", quietly = TRUE) & interval_type[1] == "HPD") {
    message('High posterior density intervals (interval_type = "HPD") require the
coda package to be installed. Using interval_type = "quantile".')
    interval_type <- "quantile"
  }
  if (interval_type[1] == "quantile") {
    interval <- cbind(lower = apply(x$posterior_simulations, 2, quantile, probs = (1-conf_level)/2),
                      upper = apply(x$posterior_simulations, 2, quantile, probs = conf_level + (1-conf_level)/2))
  } else {
    interval <- coda::HPDinterval(coda::as.mcmc(x$posterior_simulations), prob = conf_level)
  }
  output <- as.data.frame(t(apply(x$posterior_simulations, 2, function(term) {
    return(c(estimate = mean(term), std.error = sd(term)))
  })), stringsAsFactors = FALSE)
  return(cbind(output, conf.low = interval[, 'lower'], conf.high = interval[, 'upper']))
}

#' @export
print.beta_binomial_fit <- function(x, ...) {
  print(summary(x))
}

#' @title Visualize posterior draws from a fitted Beta-Binomial model
#' @description Plots the point estimates and 95\% and 80\% credible intervals
#'   for parameters, including relative risk and odds ratio.
#' @param x An object of class "beta_binomial_fit".
#' @param interval_type Method for computing intervals ("quantile" or "HPD").
#' @export
plot.beta_binomial_fit <- function(x, interval_type = c("quantile", "HPD"), ...) {
  if (!requireNamespace("coda", quietly = TRUE) & interval_type[1] == "HPD") {
    message('High posterior density intervals (interval_type = "HPD") require the
coda package to be installed. Defaulting to interval_type = "quantile"')
    interval_type <- "quantile"
  }
  temp95 <- summary(x, conf_level = 0.95, interval_type = interval_type)
  temp80 <- summary(x, conf_level = 0.80, interval_type = interval_type)
  temp95$term <- factor(rownames(temp95),
                        levels = c("p1", "p2", "prop_diff", "relative_risk", "odds_ratio"),
                        labels = c("Prop 1", "Prop 2", "Prop 1 - Prop 2", "Relative Risk", "Odds Ratio"))
  temp80$term <- factor(rownames(temp80),
                        levels = c("p1", "p2", "prop_diff", "relative_risk", "odds_ratio"),
                        labels = c("Prop 1", "Prop 2", "Prop 1 - Prop 2", "Relative Risk", "Odds Ratio"))
  gg <- ggplot2::ggplot(data = temp80, ggplot2::aes(x = estimate, y = term)) +
    ggplot2::geom_segment(data = temp95,
                          ggplot2::aes(x = conf.low, xend = conf.high,
                     y = term, yend = term),
                 color = "black", size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x = conf.low, xend = conf.high,
                                       y = term, yend = term),
                 color = "#e41a1c", size = 1.25) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_discrete(limits = rev(temp80$term))
  if (interval_type[1] == "quantile") {
    gg <- gg + ggplot2::labs(title = "95% and 80% Credible Intervals", y = NULL, x = NULL)
  } else {
    gg <- gg + ggplot2::labs(title = "95% and 80% HPD Intervals", y = NULL, x = NULL)
  }
  return(gg)
}
