# Bayesian Categorical Data Analysis

This is a set of tools for Bayesian analysis of categorical data, specifically 2×2 contingency tables.

Use:

- `beta_binom()` for analysis using the Beta-Binomial model
  - `print()`, `plot()`, and `summary()` to view the results
  - `present_bbfit()` if you want a nicely formatted table of summaries to include in a presentation or report
  - `update()` if you have additional data
- `est_multinom()` for estimating multinomial cell probabilities

For more information, see the Tutorial vignette.

## Installing

```R
install.packages("devtools")
devtools::install_github("bearloga/BCDA")
```

## Usage

Note that `beta_binom()` uses the Jeffreys prior by default.

```R
data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
colnames(data) <- c('Safe' ,'Dangerous')
rownames(data) <- c('Animals', 'Plants')

set.seed(0)
(fit <- beta_binom(data))
```

|              | estimate| std.error| conf.low| conf.high|
|:-------------|--------:|---------:|--------:|---------:|
|p1            |    0.571|     0.027|    0.519|     0.623|
|p2            |    0.455|     0.021|    0.414|     0.496|
|prop_diff     |    0.116|     0.034|    0.050|     0.183|
|relative_risk |    1.259|     0.083|    1.102|     1.430|
|odds_ratio    |    1.614|     0.225|    1.220|     2.102|

The credible intervals above are calculated using quantiles. If we have the **coda** package installed, we can also obtain the high posterior density intervals:

```R
summary(fit, interval_type = "HPD")
```

|              | estimate| std.error| conf.low| conf.high|
|:-------------|--------:|---------:|--------:|---------:|
|p1            |    0.571|     0.027|    0.519|     0.623|
|p2            |    0.455|     0.021|    0.413|     0.496|
|prop_diff     |    0.116|     0.034|    0.051|     0.184|
|relative_risk |    1.259|     0.083|    1.096|     1.422|
|odds_ratio    |    1.614|     0.225|    1.198|     2.068|

```R
plot(fit)
```

![Preview of visualization of the posterior draws.](plot.png)

## Tabular presentation of the results

This package includes a function `present_bbfit()` that creates a nicely formatted table of results from fitting a `beta_bimom()` model:

```R
present_bbfit(fit, digits = 2)
```

| Group 1| Group 2|Pr(Success) in Group 1  |Pr(Success) in Group 2  |Difference             |Relative Risk     |Odds Ratio        |
|-------:|-------:|:-----------------------|:-----------------------|:----------------------|:-----------------|:-----------------|
|     350|     550|57.12% (51.88%, 62.29%) |45.48% (41.38%, 49.63%) |11.64% (4.96%, 18.30%) |1.26 (1.10, 1.43) |1.61 (1.22, 2.10) |

The point estimates include credible intervals by default but these can be turned off:

```R
present_bbfit(fit, conf_interval = FALSE, digits = 3)
```

| Group 1| Group 2|Pr(Success) in Group 1 |Pr(Success) in Group 2 |Difference |Relative Risk |Odds Ratio |
|-------:|-------:|:----------------------|:----------------------|:----------|:-------------|:----------|
|     350|     550|57.122%                |45.479%                |11.643%    |1.259         |1.614      |

Since the underlying code uses `summary()` to compute the summaries, we can specify a particular credible level and the type of interval we want (e.g. highest posterior density):

```R
present_bbfit(fit, conf_level = 0.8, interval_type = "HPD", digits = 2)
```

| Group 1| Group 2|Pr(Success) in Group 1  |Pr(Success) in Group 2  |Difference             |Relative Risk     |Odds Ratio        |
|-------:|-------:|:-----------------------|:-----------------------|:----------------------|:-----------------|:-----------------|
|     350|     550|57.12% (53.68%, 60.50%) |45.48% (42.74%, 48.10%) |11.64% (7.22%, 15.92%) |1.26 (1.15, 1.36) |1.61 (1.30, 1.87) |

## Updating the posterior

In Bayesian statistics, we can reuse a previously computed posterior as a prior if we have additional data, allowing us to update the parameter estimates as new data becomes available. Suppose we collect 40 observations from 2 groups (20 per group) on the first day of the A/B test, and 10 observations per day for the next 2 weeks. Here we see what happens when we update the posterior with additional data on a daily basis:

![](updating.gif)

### Example Code

```R
fit <- update(fit, x = c(100, 200), n = c(400, 600))
present_bbfit(fit, digits = 2)
```

| Group 1| Group 2|Pr(Success) in Group 1  |Pr(Success) in Group 2  |Difference            |Relative Risk     |Odds Ratio        |
|-------:|-------:|:-----------------------|:-----------------------|:---------------------|:-----------------|:-----------------|
|     750|    1150|39.96% (36.45%, 43.24%) |39.17% (36.23%, 42.01%) |0.78% (-3.73%, 5.08%) |1.02 (0.91, 1.13) |1.04 (0.85, 1.24) |

## See also

Other packages for Bayesian analysis of A/B tests include: [LearnBayes](https://cran.r-project.org/web/packages/LearnBayes/index.html) (GPL), [conting](https://cran.r-project.org/web/packages/conting/index.html) (GPL), [bandit](https://cran.r-project.org/web/packages/bandit/index.html) (GPL), [testr](https://github.com/ayakubovich/testr) (MIT).

---------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
