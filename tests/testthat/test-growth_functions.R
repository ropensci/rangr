test_that("growth_functions works", {
  size <- 50
  x <- sample(c(0:100, rep(NaN, 10)), size, replace = TRUE)
  K <- sample(c(0:100, rep(NaN, 10)), size, replace = TRUE)
  A1 <- -5
  A2 <- 5

  # exponential
  expect_true(all(exponential(x, r = log(1.4)) >= 0))
  expect_length(exponential(x, r = log(1.2)), size)

  # ricker
  expect_true(all(ricker(x, r = log(1.4), K) >= 0))
  expect_true(all(ricker(x, r = log(1.4), K, A1) >= 0))
  expect_length(ricker(x, r = log(1.2), K), size)
  expect_length(ricker(x, r = log(1.2), K, A1), size)

  # gompertz
  expect_true(all(gompertz(x, r = log(1.4), K) >= 0))
  expect_true(all(gompertz(x, r = log(1.4), K, A1) >= 0))
  expect_length(gompertz(x, r = log(1.2), K), size)
  expect_length(gompertz(x, r = log(1.2), K, A1), size)
})
