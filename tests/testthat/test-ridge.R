context("test-ridge")

### tests for compliance for tidy() behavior specification

library(modeltests)

test_that("tidy.dusty", {
  check_arguments(tidy.dusty)

  fit <- dusty(Sepal.Width ~ ., iris)
  td <- tidy(fit)

  check_tidy_output(td)
})

# not necessary, just included as an example of what a failing test looks like

test_that("example failure", {
  td <- tidy(dusty(Sepal.Width ~ ., iris))
  check_tidy_output(as.data.frame(td))  # fails because expects a tibble
})

### tests specific to this model/package

test_that("models have appropriate class", {
  expect_true(inherits(dusty(Sepal.Width ~ ., iris), "dusty"))
})
