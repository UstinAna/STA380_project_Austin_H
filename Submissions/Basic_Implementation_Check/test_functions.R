source("functions.R")

# Simple helper used in distribution shape tests
kurtosis <- function(x) {
  m2 <- mean((x - mean(x))^2)
  m4 <- mean((x - mean(x))^4)
  m4 / m2^2
}

# ===========================================================================
# perm_test
# ===========================================================================

test_that("perm_test returns expected components", {
  set.seed(1)
  x <- rnorm(20)
  y <- rnorm(20)

  out <- perm_test(x, y, B = 200, seed = 1)

  expect_true(is.list(out))
  expect_true(all(c("obs_stat", "perm_stats", "p_value", "stat") %in% names(out)))
  expect_length(out$perm_stats, 200)
})

test_that("perm_test gives a p-value in (0, 1]", {
  set.seed(2)
  x <- rnorm(15)
  y <- rnorm(15)

  p <- perm_test(x, y, B = 200, seed = 2)$p_value

  # With (count+1)/(B+1) the p-value is always > 0
  expect_true(p > 0 && p <= 1)
})

test_that("perm_test seed argument gives reproducible results", {
  set.seed(99)
  x <- rnorm(20)
  y <- rnorm(20)

  res1 <- perm_test(x, y, B = 200, seed = 7)
  res2 <- perm_test(x, y, B = 200, seed = 7)

  expect_equal(res1$perm_stats, res2$perm_stats)
  expect_equal(res1$p_value,    res2$p_value)
})

test_that("permutation distribution is roughly centered under the null", {
  set.seed(42)
  x <- rnorm(30)
  y <- rnorm(30)

  res <- perm_test(x, y, B = 500)

  expect_lt(abs(mean(res$perm_stats)), 0.2)
})

test_that("perm_test detects a clear location shift (mean_diff)", {
  set.seed(123)
  x <- rnorm(25)
  y <- rnorm(25, mean = 1.5)

  res <- perm_test(x, y, B = 400, stat = "mean_diff")

  expect_lt(res$p_value, 0.05)
})

test_that("perm_test detects a clear location shift (t_stat)", {
  set.seed(123)
  x <- rnorm(25)
  y <- rnorm(25, mean = 1.5)

  res <- perm_test(x, y, B = 400, stat = "t_stat")

  expect_lt(res$p_value, 0.05)
  expect_equal(res$stat, "t_stat")
})

test_that("perm_test detects a clear location shift (median_diff)", {
  set.seed(123)
  x <- rnorm(25)
  y <- rnorm(25, mean = 1.5)

  res <- perm_test(x, y, B = 400, stat = "median_diff")

  expect_lt(res$p_value, 0.05)
  expect_equal(res$stat, "median_diff")
})

test_that("perm_test stat field records the statistic used", {
  set.seed(7)
  x <- rnorm(15)
  y <- rnorm(15)

  expect_equal(perm_test(x, y, stat = "mean_diff")$stat,   "mean_diff")
  expect_equal(perm_test(x, y, stat = "t_stat")$stat,      "t_stat")
  expect_equal(perm_test(x, y, stat = "median_diff")$stat, "median_diff")
})

test_that("perm_test errors on invalid inputs", {
  expect_error(perm_test("x", "y"))
  expect_error(perm_test(rnorm(5), rnorm(5), B = -10))
})

test_that("perm_test works with unequal group sizes", {
  set.seed(10)
  x <- rnorm(15)
  y <- rnorm(40)

  res <- perm_test(x, y, B = 300)

  expect_true(is.numeric(res$p_value))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

# ===========================================================================
# simulate_two_sample
# ===========================================================================

test_that("simulate_two_sample returns numeric vectors of correct length", {
  dat <- simulate_two_sample(10, 12)

  expect_true(is.numeric(dat$x))
  expect_true(is.numeric(dat$y))
  expect_length(dat$x, 10)
  expect_length(dat$y, 12)
})

test_that("delta parameter shifts the mean of group 2 as expected", {
  set.seed(99)
  dat <- simulate_two_sample(n1 = 200, n2 = 200, dist = "normal", delta = 1)

  expect_gt(mean(dat$y) - mean(dat$x), 0.7)
})

test_that("simulate_two_sample produces expected shapes across distributions", {
  set.seed(5)
  dat_n <- simulate_two_sample(300, 300, dist = "normal")
  dat_e <- simulate_two_sample(300, 300, dist = "exponential")
  dat_t <- simulate_two_sample(300, 300, dist = "t")

  # Exponential is right-skewed: mean > median
  expect_gt(mean(dat_e$x), median(dat_e$x))
  # t with df=3 is heavier-tailed than normal: higher kurtosis
  expect_gt(kurtosis(dat_t$x), kurtosis(dat_n$x))
})

test_that("exponential samples are right-skewed", {
  set.seed(55)
  dat <- simulate_two_sample(300, 300, dist = "exponential")

  expect_gt(mean(dat$x), median(dat$x))
})

test_that("simulate_two_sample stops on unsupported distributions", {
  expect_error(simulate_two_sample(10, 10, dist = "gamma"))
})

# ===========================================================================
# mc_compare_tests
# ===========================================================================

test_that("mc_compare_tests returns valid rejection rates", {
  set.seed(1)
  res <- mc_compare_tests(nsim = 50, n1 = 10, n2 = 10)

  expect_true(is.list(res))
  expect_true("rejection_rates" %in% names(res))
  expect_true(all(res$rejection_rates >= 0))
  expect_true(all(res$rejection_rates <= 1))
})

test_that("mc_compare_tests rejection rates contain both test names", {
  set.seed(2)
  res <- mc_compare_tests(nsim = 50, n1 = 10, n2 = 10)

  expect_true("t_test" %in% names(res$rejection_rates))
  expect_true("permutation_test" %in% names(res$rejection_rates))
})

test_that("permutation test Type I error is near nominal under the null", {
  set.seed(2024)
  res <- mc_compare_tests(nsim = 200, n1 = 20, n2 = 20, delta = 0)

  perm_rate <- res$rejection_rates["permutation_test"]

  # Allow +/-0.05 around nominal alpha = 0.05
  expect_lt(abs(perm_rate - 0.05), 0.05)
})

test_that("mc_compare_tests shows higher rejection with larger effect size", {
  set.seed(2025)
  res_small <- mc_compare_tests(nsim = 100, n1 = 20, n2 = 20, delta = 0.3)
  res_large <- mc_compare_tests(nsim = 100, n1 = 20, n2 = 20, delta = 1)

  expect_gt(
    res_large$rejection_rates["permutation_test"],
    res_small$rejection_rates["permutation_test"]
  )
})

test_that("mc_compare_tests B argument is passed through correctly", {
  set.seed(3)
  res <- mc_compare_tests(nsim = 20, n1 = 10, n2 = 10, B = 99)

  expect_true(all(res$rejection_rates >= 0 & res$rejection_rates <= 1))
})

test_that("mc_compare_tests works with exponential distribution", {
  set.seed(101)
  res <- mc_compare_tests(nsim = 50, n1 = 15, n2 = 15,
                          dist = "exponential", delta = 0)

  expect_true(all(res$rejection_rates >= 0 & res$rejection_rates <= 1))
})

test_that("mc_compare_tests works with t distribution", {
  set.seed(202)
  res <- mc_compare_tests(nsim = 50, n1 = 15, n2 = 15,
                          dist = "t", delta = 0)

  expect_true(all(res$rejection_rates >= 0 & res$rejection_rates <= 1))
})

test_that("mc_compare_tests stat argument is accepted without error", {
  set.seed(303)
  res <- mc_compare_tests(nsim = 20, n1 = 10, n2 = 10, stat = "median_diff", B = 100)

  expect_true(all(res$rejection_rates >= 0 & res$rejection_rates <= 1))
})
