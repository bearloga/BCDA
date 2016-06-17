# Bayesian Categorical Data Analysis

This is a set of tools for Bayesian analysis of categorical data, specifically 2×2 contingency tables.

Use:

- `beta_binom()` for analysis using the Beta-Binomial model
  - `print()`, `plot()`, and `summary()` to view the results
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

(fit <- beta_binom(data))
```

|              | estimate| std.error| conf.low| conf.high|
|:-------------|--------:|---------:|--------:|---------:|
|p1            |    0.571|     0.026|    0.519|     0.623|
|p2            |    0.455|     0.021|    0.414|     0.497|
|prop_diff     |    0.116|     0.034|    0.049|     0.182|
|relative_risk |    1.258|     0.083|    1.102|     1.428|
|odds_ratio    |    1.613|     0.225|    1.218|     2.092|

The credible intervals above are calculated using quantiles. If we have the **coda** package installed, we can also obtain the high posterior density intervals:

```R
summary(fit, interval_type = "HPD")
```

|              | estimate| std.error| conf.low| conf.high|
|:-------------|--------:|---------:|--------:|---------:|
|p1            |    0.571|     0.026|    0.518|     0.620|
|p2            |    0.455|     0.021|    0.414|     0.497|
|prop_diff     |    0.116|     0.034|    0.049|     0.182|
|relative_risk |    1.258|     0.083|    1.098|     1.422|
|odds_ratio    |    1.613|     0.225|    1.191|     2.053|

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

Other packages for Bayesian analysis of A/B tests include: [LearnBayes](https://cran.r-project.org/web/packages/LearnBayes/index.html) (GPL), [conting](https://cran.r-project.org/web/packages/conting/index.html) (GPL), [bandit](https://cran.r-project.org/web/packages/bandit/index.html) (GPL), [testr](https://github.com/ayakubovich/testr) (MIT).

---------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
