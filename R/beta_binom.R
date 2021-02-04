#' @title Fit a Binomial model with Beta priors
#'
#' @description Fits a Beta-Binomial model to a 2x2 table/matrix of two groups
#'   and two outcomes/features.
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
#' @examples
#' fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(fake_data) <- c('Safe' ,'Dangerous')
#' rownames(fake_data) <- c('Animals', 'Plants')
#' beta_binom(fake_data)
#'
#' beta_binom(x = c(200, 250), n = c(350, 550))
#' @export
beta_binom <- function(x,
                       n = NULL,
                       prior = c(a = 0.5, b = 0.5, c = 0.5, d = 0.5),
                       n_sims = 1e4)
{
  data <- check_inputs(x, n)
  x <- unname(data[, 1])
  n <- as.numeric(unname(margin.table(data, 1)))

  # Posterior hyperparameters:
  new_a <- unname(x[1] + prior['a'])
  new_b <- unname(n[1] - x[1] + prior['b'])
  new_c <- unname(x[2] + prior['c'])
  new_d <- unname(n[2] - x[2] + prior['d'])

  # Simulations:
  sims <- data.frame(
    p1 = rbeta(n_sims, new_a, new_b),
    p2 = rbeta(n_sims, new_c, new_d),
    stringsAsFactors = FALSE
  )
  sims$prop_diff <- sims$p1 - sims$p2
  sims$relative_risk <- sims$p1/sims$p2
  sims$odds_ratio <- (sims$p1 / (1 - sims$p1)) / (sims$p2 / (1 - sims$p2))

  # Output:
  structure(
    list(
      posterior_simulations = invisible(sims),
      posterior_parameters = invisible(c(
        'a' = new_a, 'b' = new_b,
        'c' = new_c, 'd' = new_d
      )),
      successes = x,
      totals = n
    ),
    class = "beta_binomial_fit"
  )
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
#' fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(fake_data) <- c('Safe' ,'Dangerous')
#' rownames(fake_data) <- c('Animals', 'Plants')
#'
#' fit <- beta_binom(fake_data)
#' fit <- update(fit, x = c(100, 200), n = c(400, 600))
#' }
#' @export
update.beta_binomial_fit <- function(object, x, n = NULL, n_sims = 1e3, ...) {
  data <- check_inputs(x, n)
  x <- unname(data[, 1])
  n <- as.numeric(unname(margin.table(data, 1)))
  fit <- beta_binom(x, n, object$posterior_parameters, n_sims)
  fit$successes <- object$successes + x
  fit$totals <- object$totals + n
  return(fit)
}

#' @title Visualize posterior draws from a fitted Beta-Binomial model
#' @description Plots the point estimates and 95\% and 80\% credible intervals
#'   for parameters, including relative risk and odds ratio.
#' @param x An object of class "beta_binomial_fit".
#' @param interval_type Method for computing intervals ("quantile" or "HPD").
#' @return A ggplot2 object.
#' @import ggplot2
#' @export
plot.beta_binomial_fit <- function(x, interval_type = c("quantile", "HPD"), ...) {
  if (!requireNamespace("coda", quietly = TRUE) & interval_type[1] == "HPD") {
    message('High posterior density intervals (interval_type = "HPD") require the
coda package to be installed. Defaulting to interval_type = "quantile"')
    interval_type <- "quantile"
  }
  temp95 <- tidy(x, conf_level = 0.95, interval_type = interval_type)
  temp80 <- tidy(x, conf_level = 0.80, interval_type = interval_type)
  gg <- ggplot(data = temp80, aes(x = estimate, y = term)) +
    geom_segment(
      data = temp95,
      aes(x = conf.low, xend = conf.high, y = term, yend = term),
      color = "black", size = 0.75
    ) +
    geom_segment(
      aes(x = conf.low, xend = conf.high, y = term, yend = term),
      color = "#e41a1c", size = 1.25
    ) +
    geom_point(size = 2) +
    scale_y_discrete(
      limits = rev(c("p1", "p2", "prop_diff", "relative_risk", "odds_ratio")),
      labels = rev(c("Prop 1", "Prop 2", "Prop 1 - Prop 2", "Relative Risk", "Odds Ratio"))
    )
  if (interval_type[1] == "quantile") {
    gg <- gg + labs(title = "95% and 80% Credible Intervals", y = NULL, x = NULL)
  } else {
    gg <- gg + labs(title = "95% and 80% HPD Intervals", y = NULL, x = NULL)
  }
  return(gg)
}

#' @title Present the summarized posterior results of the Beta-Binomial model
#' @description Outputs a nicely-formatted table suitable for presentations and
#'   reports. Especially useful for combining multiple results into a single
#'   summary table.
#' @param object An object of class "beta_binomial_fit" or a list (named or
#'   unnamed) of "beta_binomial_fit" objects created by \code{beta_binom()}.
#' @param conf_interval A logical flag indicating whether to include Bayesian
#'   confidence intervals in the generated table.
#' @param conf_level Probability level for credible intervals. 95\% by default.
#' @param interval_type Method for computing intervals ("quantile" or "HPD").
#' @param raw A logical flag to return a data frame instead of the character
#'   vector produced by \code{knitr::kable}. Useful for performing additional
#'   data manipulations.
#' @param fancy_names A logical flag to use reader-friendly column names.
#' @param ... Arguments to forward to \code{knitr::kable} (e.g. \code{format}).
#' @return A character vector formatted as Markdown, HTML, or LaTeX. The table
#'   has the following columns:
#'   \describe{
#'     \item{Group 1}{The total count of trials in group 1.}
#'     \item{Group 2}{The total count of trials in group 2.}
#'     \item{Pr(Success) in Group 1}{Point estimate of the probability of success in group 1.}
#'     \item{Pr(Success) in Group 2}{Point estimate of the probability of success in group 2.}
#'     \item{Difference}{Pr(Success) in Group 1 - Pr(Success) in Group 2}
#'     \item{Relative Risk}{How likely trials in group 1 are to succeed relative to group 2.}
#'     \item{Odds Ratio}{The ratio of the odds of success in group 1 vs odds of success in group 2.}
#'   }
#'   The estimated quantities have credible intervals by default but these can
#'   be turned off with the \code{conf_interval} argument.
#' @examples \dontrun{
#' fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(fake_data) <- c('Safe' ,'Dangerous')
#' rownames(fake_data) <- c('Animals', 'Plants')
#'
#' fit <- beta_binom(fake_data)
#' present_bbfit(fit) # uses getOption("digits")
#' present_bbfit(fit, digits = 2)
#' present_bbfit(fit, conf_interval = FALSE, digits = 3)
#' present_bbfit(fit, conf_level = 0.8, interval_type = "HPD", digits = 2)
#'
#' fit_2 <- update(fit, x = c(8, 4), n = c(10, 50))
#' fit_3 <- update(fit_2, x = c(1, 20), n = c(15, 40))
#' fit_4 <- update(fit_3, x = c(20, 13), n = c(80, 45))
#' present_bbfit(list("Day 1" = fit, "Day 2" = fit_2, "Day 3" = fit_3, "Day 4" = fit_4), digits = 2)
#' }
#' @export
present_bbfit <- function(object, conf_interval = TRUE, conf_level = 0.95, interval_type = c("quantile", "HPD"), raw = FALSE, fancy_names = TRUE, ...) {
  interval_type <- interval_type[1]
  if (!interval_type %in% c("quantile", "HPD"))
    stop("interval_type must be 'quantile' or 'HPD' (requires {coda} package)")

  if (!requireNamespace("knitr", quietly = TRUE))
    stop('knitr is required for generating a formatted table"')

  if (!requireNamespace("coda", quietly = TRUE) & interval_type == "HPD") {
    message('High posterior density intervals (interval_type = "HPD") require the coda package to be installed. Defaulting to interval_type = "quantile"')
    interval_type <- "quantile"
  }
  args <- list(...)
  .digits <- ifelse("digits" %in% names(args), args$digits[1], getOption("digits", 3))
  if (class(object) == "list") {
    # Check that all the elements of this list are of class "beta_binomial_fit" before proceeding:
    if (any(vapply(object, class, "") != "beta_binomial_fit")) {
      stop("The list had an object that was not of class 'beta_binomial_fit'")
    }
  } else {
    object <- list(object) # so that we can use purrr::map
  }

  posterior_summaries <- purrr::map_dfr(
    object,
    function(fit) {
     posterior <- tidy(fit, conf_level, interval_type)
     totals <- as.integer(fit[["totals"]])

     tibble::tibble(
       n1 = totals[1],
       n2 = totals[2],
       p1 = format_confint(100 * posterior$estimate[posterior$term == "p1"], if_else(conf_interval, 100 * posterior[posterior$term == "p1", c("conf.low", "conf.high")], NULL), digits = .digits, units = "%"),
       p2 = format_confint(100 * posterior$estimate[posterior$term == "p2"], if_else(conf_interval, 100 * posterior[posterior$term == "p2", c("conf.low", "conf.high")], NULL), digits = .digits, units = "%"),
       pd = format_confint(100 * posterior$estimate[posterior$term == "prop_diff"], if_else(conf_interval, 100 * posterior[posterior$term == "prop_diff", c("conf.low", "conf.high")], NULL), digits = .digits, units = "%"),
       rr = format_confint(100 * posterior$estimate[posterior$term == "relative_risk"], if_else(conf_interval, 100 * posterior[posterior$term == "prop_diff", c("conf.low", "conf.high")], NULL), digits = .digits),
       or = format_confint(100 * posterior$estimate[posterior$term == "odds_ratio"], if_else(conf_interval, 100 * posterior[posterior$term == "prop_diff", c("conf.low", "conf.high")], NULL), digits = .digits)
     )
    },
    .id = "bb_fit"
  )
  if (fancy_names) {
    names(posterior_summaries) <- c("Beta-Binom Fit", "Group 1", "Group 2", "Pr(Success) in Group 1", "Pr(Success) in Group 2", "Difference", "Relative Risk", "Odds Ratio")
  }

  if (raw)
    return(posterior_summaries)

  return(knitr::kable(posterior_summaries, ...))
}

#' @title Tidy results of a Beta-Binomial model fit
#' @description Returns a tidy tibble summarizing posteriors with point
#'   estimates and credible intervals for group proportions, proportion
#'   difference, relative risk, and odds ratio.
#' @param x An object of class "beta_binomial_fit".
#' @param conf_level Probability level for credible intervals. 95\% by default.
#' @param interval_type Method for computing intervals ("quantile" or "HPD").
#' @return A tibble with the following columns:
#' \describe{
#'   \item{term}{Name of the parameter.}
#'   \item{estimate}{Point estimate (median of posterior draws).}
#'   \item{std.error}{Standard error.}
#'   \item{conf.low}{Credible interval lower bound.}
#'   \item{conf.high}{Credible interval upper bound.}
#' }
#' @examples \dontrun{
#' tidy(beta_binom(x = c(200, 250), n = c(350, 550)))
#' }
#' @name tidy.beta_binomial_fit
NULL

#' @rdname tidy.beta_binomial_fit
#' @importFrom magrittr %>%
#' @export
tidy.beta_binomial_fit <- function(x, conf_level = 0.95, interval_type = c("quantile", "HPD"), ...) {
  interval_type <- interval_type[1]
  if (!interval_type %in% c("quantile", "HPD"))
    stop("interval_type must be 'quantile' or 'HPD' (requires {coda} package)")

  if (!requireNamespace("coda", quietly = TRUE) & interval_type == "HPD") {
    message('High posterior density intervals (interval_type = "HPD") require the
coda package to be installed. Using interval_type = "quantile".')
    interval_type <- "quantile"
  }

  estimates <- x$posterior_simulations %>%
    lapply(function(x) tibble::tibble(estimate = mean(x), std.error = sd(x))) %>%
    dplyr::bind_rows(.id = "term")

  if (interval_type == "quantile") {
    intervals <- x$posterior_simulations %>%
      tidyr::pivot_longer(tidyr::everything(), names_to = "term") %>%
      dplyr::group_by(term) %>%
      dplyr::summarize(
        ps = c("conf.low", "conf.high"),
        qs = quantile(value, probs = conf_level * c(1, 0) + ((1 - conf_level) / 2)),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(names_from = "ps", values_from = "qs")
  } else {
    intervals <- x$posterior_simulations %>%
      coda::as.mcmc() %>%
      coda::HPDinterval(prob = conf_level) %>%
      as.data.frame() %>%
      { .$term = rownames(.); rownames(.) <- NULL; . } %>%
      dplyr::rename(conf.low = lower, conf.high = upper)
  }

  dplyr::left_join(estimates, intervals, by = "term") # return
}

#' @title Glance at a fitted Beta-Binomial model.
#' @description Constructs a single row summary "glance" of a Beta-Binomial model fit.
#' @param x An object of class "beta_binomial_fit".
#' @param ... Arguments to pass forward to [present_bbfit()]
#' @examples \dontrun{
#' glance(beta_binom(x = c(200, 250), n = c(350, 550)))
#' }
#' @name glance.beta_binomial_fit
NULL

#' @rdname glance.beta_binomial_fit
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
