#' @title Binomial Model with Beta Priors
#'
#' @description
#'
#' @param x A numeric vector of success counts OR a 2x2 table or matrix.
#' @param n A numeric vector of totals. (Optional if x is a table or matrix.)
#' @param prior Parameters for independent beta priors. See \strong{Details}.
#' @param n_sims Number of draws (simulations) to make from the posterior
#'   distributions.
#' @return An S3 object of class "beta_binomial_fit". It has the following
#'   properties: posterior_simulations, posterior_parameters, successes, totals
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
#' beta_binom(data)
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
                                                           'c' = new_c, 'd' = new_d)),
                        successes = x, totals = n),
                   class = "beta_binomial_fit"))
}

#' @title Update the Beta-Binomial model posterior with new data
#' @description Uses the previously computed Beta posterior as prior to compute
#'   a new Beta posterior, which can subsequently be updated as even more data
#'   becomes available.
#' @param object An object of class "beta_binomial_fit".
#' @param x A numeric vector of success counts OR a 2x2 table or matrix.
#' @param n A numeric vector of totals. (Optional if \code{x} is a table or
#'   matrix.)
#' @param sims Number of draws (simulations) to make from the posterior
#'   distributions.
#' @return An S3 object of class "beta_binomial_fit".
#' @examples \dontrun{
#' data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(data) <- c('Safe' ,'Dangerous')
#' rownames(data) <- c('Animals', 'Plants')
#'
#' fit <- beta_binom(data)
#' fit <- update(fit, x = c(100, 200), n = c(400, 600))
#' }
#' @export
update.beta_binomial_fit <- function(object, x, n = NULL, n_sims = 1e3) {
  data <- check_inputs(x, n); x <- unname(data[, 1]); n <- as.numeric(unname(margin.table(data, 1)))
  fit <- beta_binom(x, n, object$posterior_parameters, n_sims)
  fit$successes <- object$successes + x; fit$totals <- object$totals + n
  return(fit)
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
plot.beta_binomial_fit <- function(x, interval_type = c("quantile", "HPD")) {
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

#' @title Present the posterior results of the Beta-Binomial model
#' @description Outputs a nicely-formatted table suitable for presentations and
#'   reports. Especially useful for combining multiple results into a single
#'   summary table.
#' @param object An object of class "beta_binomial_fit".
#' @param conf_interval A logical flag indicating whether to include Bayesian
#'   confidence intervals in the generated table.
#' @param conf_level Probability level for credible intervals. 95\% by default.
#' @param interval_type Method for computing intervals ("quantile" or "HPD").
#' @param ... Arguments to forward to \code{knitr::kable} (e.g. \code{format}).
#' @return A character vector formatted as Markdown, HTML, or LaTeX. The table
#'   has the following columns:
#'   \describe{
#'     \item{Group 1}{The total count of trials in group 1.}
#'     \item{Group 2}{The total count of trials in group 2.}
#'     \item{Pr(Success) of Group 1}{Point estimate of the probability of success in group 1.}
#'     \item{Pr(Success) of Group 2}{Point estimate of the probability of success in group 2.}
#'     \item{Relative Risk}{How likely trials in group 1 are to succeed relative to group 2.}
#'     \item{Odds Ratio}{The ratio of the odds of success in group 1 vs odds of success in group 2.}
#'   }
#'   The estimated quantities have credible intervals by default but these can
#'   be turned off with the \code{conf_interval} argument.
#' @examples \dontrun{
#' data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(data) <- c('Safe' ,'Dangerous')
#' rownames(data) <- c('Animals', 'Plants')
#'
#' fit <- beta_binom(data)
#' present_bbfit(fit) # uses getOption("digits")
#' present_bbfit(fit, digits = 2)
#' present_bbfit(fit, conf_interval = FALSE, digits = 3)
#' present_bbfit(fit, conf_level = 0.8, interval_type = "HPD", digits = 2)
#' }
#' @export
present_bbfit <- function(object, conf_interval = TRUE, conf_level = 0.95, interval_type = c("quantile", "HPD"), ...) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop('knitr is required for generating a formatted table"')
  }
  if (!requireNamespace("coda", quietly = TRUE) & interval_type[1] == "HPD") {
    message('High posterior density intervals (interval_type = "HPD") require the
            coda package to be installed. Defaulting to interval_type = "quantile"')
    interval_type <- "quantile"
  }
  args <- list(...)
  digits_ <- ifelse("digits" %in% names(args), args$digits[1], getOption("digits"))
  posterior_summaries <- summary(object, conf_level = conf_level, interval_type = interval_type)
  output <- data.frame(n1 = object$totals[1], n2 = object$totals[2],
                       p1 = format_confint(100 * posterior_summaries["p1", "estimate"],
                                           if_else(conf_interval,
                                                   100 * posterior_summaries["p1", c("conf.low", "conf.high")],
                                                   NULL),
                                           digits = digits_, units = "%"),
                       p2 = format_confint(100 * posterior_summaries["p2", "estimate"],
                                           if_else(conf_interval,
                                                   100 * posterior_summaries["p2", c("conf.low", "conf.high")],
                                                   NULL),
                                           digits = digits_, units = "%"),
                       rr = format_confint(posterior_summaries["relative_risk", "estimate"],
                                           if_else(conf_interval,
                                                   100 * posterior_summaries["relative_risk", c("conf.low", "conf.high")],
                                                   NULL),
                                           digits = digits_),
                       or = format_confint(posterior_summaries["odds_ratio", "estimate"],
                                           if_else(conf_interval,
                                                   100 * posterior_summaries["odds_ratio", c("conf.low", "conf.high")],
                                                   NULL),
                                           digits = digits_))
  names(output) <- c("Group 1", "Group 2", "Pr(Success) of Group 1", "Pr(Success) of Group 2", "Relative Risk", "Odds Ratio")
  return(knitr::kable(output, ...))
}
