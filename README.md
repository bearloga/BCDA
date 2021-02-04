---
title: Bayesian Categorical Data Analysis
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
options(digits = 2)
set.seed(0)
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))
```

# BCDA

This is a set of tools for Bayesian analysis of categorical data, specifically 2×2 contingency tables.

Use:

- `beta_binom()` for analysis using the Beta-Binomial model
  - `print()`, `tidy()`, `glance()`, and `plot()` to view the results
  - `present_bbfit()` if you want a nicely formatted table of summaries to include in a presentation or report
  - `update()` if you have additional data
- `est_multinom()` for estimating multinomial cell probabilities

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bearloga/BCDA")
```
## Example

`beta_binom()` works with a table of the following format:

|         | Success | Failure |
|--------:|:-------:|:-------:|
| Group 1 |   n11   |   n12   |
| Group 2 |   n21   |   n22   |

and uses the Beta-Binomial model to estimate:

|         | Success | Failure |
|--------:|:-------:|:-------:|
| Group 1 |    p1   |  1 - p1 |
| Group 2 |    p2   |  1 - p2 |

which allows inference on the difference (`p1 - p2`), the risk ratio (RR), and the odds ratio (OR), defined as follows:

- `RR = p1 / p2` -- how much more likely success in group 1 is, relative to success in group 2
- `OR = (p1/(1-p1)) / (p2/(1-p2))` -- the ratio of the odds -- where `p1/(1-p1)` is the odds of success in group 1 and `p2/(1-p2)` is the odds of success in group 2

All examples will use the following fake data:

```{r fake-data}
fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
colnames(fake_data) <- c('Safe' ,'Dangerous')
rownames(fake_data) <- c('Animals', 'Plants')
```
```{r, echo=FALSE}
knitr::kable(fake_data)
```

**Note** that `beta_binom()` uses the Jeffreys prior by default and that this README was knit with the seed set to 0 at the start for reproducibility.

```{r example-initial}
library(BCDA)

(fit <- beta_binom(fake_data))
```

The credible intervals above are calculated using quantiles. If we have the **coda** package installed, we can also obtain the high posterior density intervals:

```{r highest-posterior-density}
print(fit, interval_type = "HPD")
```

```{r plotting-params, fig.height=4, fig.width=8, dpi=72, fig.cap='Preview of visualization of the posterior draws.', fig.path='figures/'}
plot(fit)
```

## Presentation of the results

The package includes a variety of functions for looking at the results from fitting a `beta_binom()` model. To aid in functional programming, we implemented the `tidy()` and `glance()` verbs:

```{r tidy-output, results='asis'}
library(magrittr) # for %>%
fit %>%
  tidy %>%
  knitr::kable()
```

When knitting R Markdown to HTML, you can use the [{gt}](https://gt.rstudio.com/) package for creating tables:

```{r tidy-output-gt, eval=FALSE}
library(gt)
fit %>%
  tidy %>%
  gt(rowname_col = "term") %>%
  tab_header(md("Results of `beta_binom()`"))
```

`glance()` is used to

> Construct a single row summary "glance" of a model, fit, or other object

```{r glance_output}
fit %>%
  glance
```

This is perfectly okay in an interactive data analysis scenario, but not when presenting the results in a report. `glance()` is actually a special case of the `present_bbfit()` function which generates all those nicely formatted credible intervals but outputs a Markdown/LaTeX-formatted table by default:

```{r, results='asis'}
present_bbfit(fit)
```

The point estimates include credible intervals by default but these can be turned off:

```{r, results='asis'}
present_bbfit(fit, conf_interval = FALSE, digits = 3)
```

Since the underlying code uses `tidy()` to compute the summaries, we can specify a particular credible level and the type of interval we want (e.g. highest posterior density):

```{r, results='asis'}
present_bbfit(fit, conf_level = 0.89, interval_type = "HPD")
```

It also supports multiple models, which can be provided as a named or an unnamed list. See the example below.

## Updating the posterior

In Bayesian statistics, we can reuse a previously computed posterior as a prior if we have additional data, allowing us to update the parameter estimates as new data becomes available. Suppose we collect 40 observations from 2 groups (20 per group) on the first day of the A/B test, and 10 observations per day for the next 2 weeks. Here we see what happens when we update the posterior with additional data on a daily basis:

![](updating.gif)

### Example Code

```{r updating_posterior, results='asis'}
fit_2 <- update(fit, x = c(100, 200), n = c(400, 600))
present_bbfit(list("Day 1" = fit, "Day 2" = fit_2))
```

## See also

Other packages for Bayesian analysis of A/B tests include: [LearnBayes](https://cran.r-project.org/web/packages/LearnBayes/index.html) (GPL), [conting](https://cran.r-project.org/web/packages/conting/index.html) (GPL), [bandit](https://cran.r-project.org/web/packages/bandit/index.html) (GPL), [testr](https://github.com/ayakubovich/testr) (MIT).

---------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
