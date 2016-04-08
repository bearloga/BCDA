# Bayesian Categorical Data Analysis

This is a set of tools for Bayesian analysis of categorical data, specifically 2×2 contingency tables.

Use:

- `beta_binom()` for analysis using the Beta-Binomial model
  - `print()`, `plot()`, and `summary()` to view the results
  - `update()` if you have additional data
- `est_multinom()` for estimating multinomial cell probabilities
- `test_indepen()` and `log_linear()` for assessing independence

For more information, see the Tutorial vignette.

## Installing

```R
install.packages("devtools")
devtools::install_github("bearloga/BCDA")
```

## Usage

```R
data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
colnames(data) <- c('Safe' ,'Dangerous')
rownames(data) <- c('Animals', 'Plants')

(fit <- beta_binom(data))
```

|              | estimate| std.error| conf.low| conf.high|
|:-------------|--------:|---------:|--------:|---------:|
|p1            |   0.5712|    0.0263|   0.5190|    0.6217|
|p2            |   0.4549|    0.0213|   0.4123|    0.4972|
|prop_diff     |   0.1163|    0.0340|   0.0494|    0.1824|
|relative_risk |   1.2585|    0.0834|   1.1031|    1.4285|
|odds_ratio    |   1.6135|    0.2248|   1.2191|    2.0972|

```R
summary(fit, interval_type = "HPD")
```

|              | estimate| std.error| conf.low| conf.high|
|:-------------|--------:|---------:|--------:|---------:|
|p1            |   0.5712|    0.0263|   0.5206|    0.6226|
|p2            |   0.4549|    0.0213|   0.4145|    0.4990|
|prop_diff     |   0.1163|    0.0340|   0.0502|    0.1831|
|relative_risk |   1.2585|    0.0834|   1.1031|    1.4285|
|odds_ratio    |   1.6135|    0.2248|   1.1973|    2.0678|

```R
plot(fit)
```

![Preview of visualization of the posterior draws.](plot.png)

## Updating the posterior

In Bayesian statistics, we can reuse a previously computed posterior as a prior if we have additional data, allowing us to update the parameter estimates as new data becomes available. Suppose we collect 40 observations from 2 groups (20 per group) on the first day of the A/B test, and 10 observations per day for the next 2 weeks. Here we see what happens when we update the posterior with additional data on a daily basis:

![](updating.gif)

### Example Code

```R
fit <- update(fit, x = c(100, 200), n = c(400, 600))
```

## See also

Other packages for Bayesian analysis of A/B tests include: [LearnBayes](https://cran.r-project.org/web/packages/LearnBayes/index.html) (GPL), [conting](https://cran.r-project.org/web/packages/conting/index.html) (GPL), [bandit](https://cran.r-project.org/web/packages/bandit/index.html) (GPL), [testr](https://github.com/ayakubovich/testr) (MIT), and [my patched fork of testr](https://github.com/bearloga/testr).

---------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
