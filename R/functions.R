# Permutation test and Monte Carlo simulation functions
# Used for comparing permutation tests and t-tests via simulation

#' Two-Sample Permutation Test
#'
#' Performs a two-sample permutation test using a choice of test statistic:
#' difference in means, t-statistic, or difference in medians.
#'
#' @param x Numeric vector of observations from group 1
#' @param y Numeric vector of observations from group 2
#' @param B Number of permutations
#' @param stat Test statistic to use. One of \code{"mean_diff"} (default),
#'   \code{"t_stat"}, or \code{"median_diff"}.
#' @param seed Optional integer seed for reproducibility. If \code{NULL}
#'   (default), no seed is set.
#'
#' @return A list containing:
#' \describe{
#'   \item{obs_stat}{The observed test statistic}
#'   \item{perm_stats}{Numeric vector of permutation test statistics}
#'   \item{p_value}{Two-sided p-value (computed as (count + 1) / (B + 1)
#'     to avoid p = 0)}
#'   \item{stat}{The test statistic used}
#' }
#'
#' @examples
#' x <- rnorm(20)
#' y <- rnorm(20, mean = 0.5)
#' perm_test(x, y, seed = 1)
#' perm_test(x, y, stat = "t_stat", seed = 1)
#' perm_test(x, y, stat = "median_diff", seed = 1)
#'
#' @export
perm_test <- function(x, y, B = 1000,
                      stat = c("mean_diff", "t_stat", "median_diff"),
                      seed = NULL) {
  
  stat <- match.arg(stat)
  
  if (!is.null(seed)) set.seed(seed)
  
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Inputs must be numeric")
  }
  
  if (length(x) < 2 || length(y) < 2) {
    stop("Each group must have at least two observations")
  }
  
  if (B <= 0) {
    stop("B must be positive")
  }
  
  compute_stat <- function(a, b) {
    switch(stat,
           mean_diff   = mean(a) - mean(b),
           t_stat = {
             nx <- length(a)
             ny <- length(b)
             sp <- sqrt(((nx - 1) * var(a) + (ny - 1) * var(b)) / (nx + ny - 2))
             (mean(a) - mean(b)) / (sp * sqrt(1/nx + 1/ny))
           },
           median_diff = median(a) - median(b)
    )
  }
  
  obs_stat <- compute_stat(x, y)
  
  combined <- c(x, y)
  n_x <- length(x)
  
  perm_stats <- numeric(B)
  
  for (b in seq_len(B)) {
    permuted  <- sample(combined)
    perm_x    <- permuted[1:n_x]
    perm_y    <- permuted[-(1:n_x)]
    perm_stats[b] <- compute_stat(perm_x, perm_y)
  }
  
  p_value <- (sum(abs(perm_stats) >= abs(obs_stat)) + 1) / (B + 1)
  
  list(
    obs_stat   = obs_stat,
    perm_stats = perm_stats,
    p_value    = p_value,
    stat       = stat
  )
}

#' Simulate Two-Sample Data
#'
#' Generates two independent samples under different
#' distributional assumptions.
#'
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#' @param dist Distribution type. One of \code{"normal"} (default),
#'   \code{"exponential"}, or \code{"t"} (t with 3 df).
#' @param delta Mean shift applied to group 2
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{Numeric vector for group 1}
#'   \item{y}{Numeric vector for group 2}
#' }
#'
#' @examples
#' simulate_two_sample(20, 20, dist = "normal")
#' simulate_two_sample(20, 20, dist = "exponential", delta = 1)
#'
#' @export
simulate_two_sample <- function(n1, n2, dist = "normal", delta = 0) {
  
  if (n1 <= 0 || n2 <= 0) {
    stop("Sample sizes must be positive")
  }
  
  if (dist == "normal") {
    x <- rnorm(n1)
    y <- rnorm(n2, mean = delta)
  } else if (dist == "exponential") {
    x <- rexp(n1)
    y <- rexp(n2) + delta
  } else if (dist == "t") {
    x <- rt(n1, df = 3)
    y <- rt(n2, df = 3) + delta
  } else {
    stop("Unsupported distribution. Choose one of: 'normal', 'exponential', 't'")
  }
  
  list(x = x, y = y)
}

#' Monte Carlo Comparison of t-test and Permutation Test
#'
#' Uses Monte Carlo simulation to estimate rejection rates
#' for a two-sample t-test and a permutation test.
#'
#' @param nsim Number of simulations
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#' @param dist Distribution type passed to \code{\link{simulate_two_sample}}.
#'   One of \code{"normal"} (default), \code{"exponential"}, or \code{"t"}.
#' @param delta Mean difference between groups
#' @param alpha Significance level
#' @param B Number of permutations passed to \code{\link{perm_test}}
#' @param stat Test statistic passed to \code{\link{perm_test}}. One of
#'   \code{"mean_diff"} (default), \code{"t_stat"}, or \code{"median_diff"}.
#' @param seed Optional integer seed for reproducibility. If \code{NULL}
#'   (default), no seed is set.
#'
#' @return A list containing:
#' \describe{
#'   \item{rejection_rates}{Named numeric vector with rejection rates for
#'     \code{t_test} and \code{permutation_test}}
#' }
#'
#' @examples
#' \dontrun{
#' mc_compare_tests(nsim = 500, n1 = 20, n2 = 20, delta = 1, seed = 42)
#' }
#'
#' @export
mc_compare_tests <- function(nsim, n1, n2,
                             dist  = "normal",
                             delta = 0,
                             alpha = 0.05,
                             B     = 1000,
                             stat  = "mean_diff",
                             seed  = NULL) {
  
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1")
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  reject_t    <- logical(nsim)
  reject_perm <- logical(nsim)
  
  for (i in seq_len(nsim)) {
    dat <- simulate_two_sample(n1, n2, dist, delta)
    x   <- dat$x
    y   <- dat$y
    
    reject_t[i]    <- t.test(x, y, var.equal = TRUE)$p.value < alpha
    reject_perm[i] <- perm_test(x, y, B = B, stat = stat)$p_value < alpha
  }
  
  list(
    rejection_rates = c(
      t_test           = mean(reject_t),
      permutation_test = mean(reject_perm)
    )
  )
}