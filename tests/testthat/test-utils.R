context("Utility Functions (Input Checking)")

fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
colnames(fake_data) <- c('Safe' ,'Dangerous')
rownames(fake_data) <- c('Animals', 'Plants')

test_that("check_inputs utility returns a correctly formatted matrix", {
  expect_identical(BCDA:::check_inputs(c(1, 2), c(3, 4)), matrix(c(1, 2, 2, 2), nrow = 2))
  expect_identical(BCDA:::check_inputs(fake_data), fake_data)
})

test_that("check_inputs utility errors when it should", {
  expect_error(BCDA:::check_inputs(c(1, 1)))
  expect_error(BCDA:::check_inputs(c(1, 1), 2))
  expect_error(BCDA:::check_inputs(c(1, 1, 1), c(2, 2, 2)))
  expect_error(BCDA::check_inputs(addmargins(fake_data)))
  expect_error(BCDA::check_inputs(fake_data, 1))
})

fake_df <- data.frame(
  group = c("A", "B"),
  yes = c(10, 20),
  no = c(30, 40),
  stringsAsFactors = FALSE
)

context("Utility Functions (Data Manipulation)")

test_that("make_table utility correctly formats a data.frame into a table", {
  expect_identical(
    BCDA::make_table(fake_df),
    structure(
      c(10, 20, 30, 40),
      .Dim = c(2L, 2L),
      .Dimnames = list(c("A", "B"), c("yes", "no")),
      class = "table"
    )
  )
})

test_that("flipping rows and columns work correctly", {
  expect_identical(BCDA::flip_rows(fake_data), fake_data[2:1, ])
  expect_identical(BCDA::flip_cols(fake_data), fake_data[, 2:1])
})

context("Utility Functions (Formatting)")

test_that("format_confint outputs a correctly formatted confidence interval", {
  expect_identical(BCDA:::format_confint(0, c(-1, 1), digits = 1), "0.0 (-1.0, 1.0)")
  expect_identical(BCDA:::format_confint(5, c(4.5, 5.5), units = "%", digits = 1), "5.0% (4.5%, 5.5%)")
})
