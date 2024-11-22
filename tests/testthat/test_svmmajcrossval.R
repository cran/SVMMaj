library(magrittr)
library(testthat)
library(SVMMaj)
library(kernlab)

n <- 200
k <- 4
beta <- matrix(rnorm(k + 1), ncol = 1)
X.base <- matrix(rnorm(n * k), ncol = k)
y.base <- scale(X.base %*% beta[-1] + beta[1]) > 0

test_that(
  desc = "Only lambda changes in crossvalidation",
  {
    model <- svmmajcrossval(
      X.base, y.base,
      verbose = TRUE,
      search.grid = list(lambda = c(0.01, 0.1, 1)),
      hinge = "quadratic",
      options = list(decomposition = "chol"),
      mc.cores = 1
    )

    expect_is(model, "svmmajcrossval")
  }
)

test_that(
  desc = "Only kernel changes in crossvalidation",
  {
    model <- svmmajcrossval(
      X.base, y.base,
      verbose = TRUE,
      search.grid = list(kernel.sigma = 2^seq(-1, 1), lambda = 10^seq(-2, 2)),
      hinge = "quadratic",
      options = list(decomposition = "chol", convergence = 1e-2),
      kernel = rbfdot,
      mc.cores = 1
    )
    expect_is(model, "svmmajcrossval")
  }
)

## TODO
# - test for single grid.point (including plot?)
