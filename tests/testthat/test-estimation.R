library(BCDA)
context("Beta-Binomial Model")

set.seed(0)

fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
colnames(fake_data) <- c('Safe' ,'Dangerous')
rownames(fake_data) <- c('Animals', 'Plants')

bb <- beta_binom(fake_data)
estimates <- round(tidy(bb)$estimate, 3)

test_that("estimates from beta_binom are correct", {
  expect_equal(estimates, c(0.571, 0.455, 0.116, 1.259, 1.614))
})

context("Multinomial Model")

test_that("estimates from est_multinom are correct", {
  expect_equal(
    est_multinom(fake_data),
    structure(
      c(0.221429332906905, 0.275399109831827, 0.17063111324325, 0.332540444018017),
      .Dim = c(2L, 2L), .Dimnames = list(c("Animals", "Plants"), c("Safe", "Dangerous"))
    )
  )
})

test_that("est_multinom respects hyperparameters", {
  expect_equal(
    est_multinom(fake_data, prior = matrix(c(0.25, 0.2, 0.15, 0.4), nrow = 2, byrow = TRUE)),
    structure(
      c(0.223188194038574, 0.27333430742256, 0.167825832846289, 0.335651665692577),
      .Dim = c(2L, 2L), .Dimnames = list(c("Animals", "Plants"), c("Safe", "Dangerous"))
    )
  )
})
