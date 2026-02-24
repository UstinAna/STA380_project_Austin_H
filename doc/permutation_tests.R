## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
set.seed(123)

refs <- '@book{efron1994introduction,
  title     = {An Introduction to the Bootstrap},
  author    = {Efron, Bradley and Tibshirani, Robert J.},
  year      = {1994},
  publisher = {Chapman and Hall/CRC},
  doi       = {10.1201/9780429246593}
}

@book{good2005permutation,
  title     = {Permutation, Parametric and Bootstrap Tests of Hypotheses},
  author    = {Good, Phillip},
  year      = {2005},
  publisher = {Springer},
  doi       = {10.1007/b138696}
}

@article{fisher1935design,
  title   = {The Design of Experiments},
  author  = {Fisher, R. A.},
  year    = {1935},
  journal = {Oliver and Boyd},
  url     = {https://ia600809.us.archive.org/15/items/in.ernet.dli.2015.502684/2015.502684.The-Design_text.pdf}
}

@manual{r2023,
  title        = {R: A Language and Environment for Statistical Computing},
  author       = {{R Core Team}},
  organization = {R Foundation for Statistical Computing},
  address      = {Vienna, Austria},
  year         = {2023},
  url          = {https://www.R-project.org/}
}'
writeLines(refs, "references.bib")

## ----load-functions-----------------------------------------------------------
source("../R/functions.R")

## ----simulate-normal----------------------------------------------------------
sim_data <- simulate_two_sample(
  n1    = 30,
  n2    = 30,
  dist  = "normal",
  delta = 0
)

x <- sim_data$x
y <- sim_data$y

## ----hist-normal--------------------------------------------------------------
hist(x, probability = TRUE, col = "lightgray",
     main = "Histogram of Sample X (Normal)",
     xlab = "Value")
lines(density(x), lwd = 2)

## ----perm-test----------------------------------------------------------------
perm_res <- perm_test(x, y, B = 1000, seed = 1)
perm_res$p_value

## ----perm-plot----------------------------------------------------------------
hist(perm_res$perm_stats, breaks = 30, col = "lightgray",
     main = "Permutation Distribution of Mean Difference",
     xlab = "Mean Difference")
abline(v =  perm_res$obs_stat, col = "red", lwd = 2, lty = 1)
abline(v = -perm_res$obs_stat, col = "red", lwd = 2, lty = 2)
legend("topright",
       legend = c("Observed statistic", "Mirror (two-sided)"),
       col    = "red", lty = c(1, 2), lwd = 2)

## ----mc-compare---------------------------------------------------------------
mc_res <- mc_compare_tests(
  nsim  = 500,
  n1    = 30,
  n2    = 30,
  dist  = "normal",
  delta = 0,
  alpha = 0.05,
  seed  = 42
)

mc_res

## ----mc-se--------------------------------------------------------------------
perm_rate <- mc_res$rejection_rates["permutation_test"]
t_rate    <- mc_res$rejection_rates["t_test"]

mc_se_perm <- sqrt(perm_rate * (1 - perm_rate) / 500)
mc_se_t    <- sqrt(t_rate    * (1 - t_rate)    / 500)

c(permutation_mc_se = round(mc_se_perm, 4),
  t_test_mc_se      = round(mc_se_t,    4))

## ----skewed-data--------------------------------------------------------------
exp_data <- simulate_two_sample(
  n1    = 30,
  n2    = 30,
  dist  = "exponential",
  delta = 0
)

hist(exp_data$x, probability = TRUE, col = "lightgray",
     main = "Histogram of Exponential Data",
     xlab = "Value")
lines(density(exp_data$x), lwd = 2)

## ----mc-exp-null--------------------------------------------------------------
mc_exp <- mc_compare_tests(
  nsim  = 500,
  n1    = 30,
  n2    = 30,
  dist  = "exponential",
  delta = 0,
  alpha = 0.05,
  seed  = 42
)

mc_exp

## ----mc-exp-power-------------------------------------------------------------
mc_power <- mc_compare_tests(
  nsim  = 500,
  n1    = 30,
  n2    = 30,
  dist  = "exponential",
  delta = 0.8,
  alpha = 0.05,
  seed  = 42
)

mc_power

## ----summary-table------------------------------------------------------------
summary_table <- rbind(
  Normal_Null = mc_res$rejection_rates,
  Exp_Null    = mc_exp$rejection_rates,
  Exp_Power   = mc_power$rejection_rates
)

round(summary_table, 3)

