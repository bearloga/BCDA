library(BCDA)
context("Beta-Binomial Updating")

set.seed(0)

fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
colnames(fake_data) <- c('Safe' ,'Dangerous')
rownames(fake_data) <- c('Animals', 'Plants')

bb.old <- beta_binom(fake_data)

bb.new <- update(bb.old, x = c(100, 200), n = c(400, 600))

estimates <- round(tidy(bb.new)$estimate, 3)

test_that("Estimates from beta_binom are correct", {
  expect_equal(estimates, c(0.4, 0.392, 0.008, 1.021, 1.038))
})
