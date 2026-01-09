library(binom)

# Flexible function: either calculate n for width, or width for n
collision_ci <- function(p, width = NULL, n = NULL, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  
  # Helper: Wilson interval
  wilson_int <- function(k, n) {
    p_hat <- k/n
    A <- p_hat + z^2 / (2*n)
    B <- z * sqrt((p_hat*(1-p_hat) + z^2/(4*n)) / n)
    denom <- 1 + z^2/n
    lower <- (A - B) / denom
    upper <- (A + B) / denom
    return(c(lower, upper))
  }
  
  # Case 1: user specifies p and target width
  if (!is.null(width) & is.null(n)) {
    # brute-force search for minimum n
    for (n_try in 2:100000) {
      k <- round(p * n_try)
      ci <- wilson_int(k, n_try)
      if ((ci[2] - ci[1]) <= width) {
        return(list(
          n = n_try,
          width = ci[2] - ci[1],
          ci = ci,
          message = sprintf("Collision probability = %.1f%% [CI: %.1f%% – %.1f%%], n = %d",
                            100*p, 100*ci[1], 100*ci[2], n_try)
        ))
      }
    }
  }
  
  # Case 2: user specifies n and p
  if (!is.null(n) & is.null(width)) {
    k <- round(p * n)
    ci <- wilson_int(k, n)
    return(list(
      n = n,
      width = ci[2] - ci[1],
      ci = ci,
      message = sprintf("Collision probability = %.1f%% [CI: %.1f%% – %.1f%%], n = %d, width = %.3f",
                        100*p, 100*ci[1], 100*ci[2], n, ci[2] - ci[1])
    ))
  }
  
  stop("Specify either (p, width) OR (p, n). Not both.")
}

collision_ci(p = 0.05, width = 0.1)
# Based on a collision threshold of 400g occurring at blade speeds of 2 + m/s incurring injury and possible mortality
# We believe collision (400g) will be rare in this system
# When collision is rare, sample size can be reduced and achieve a 10% bounds
# When collision is more frequent, sample sizes are driven up until exceeding 50%
# Conservatively plan for 5% collision
# Based on a 5% collision risk, with a desired confidence interval width of 10%, we require ~82 data sets.
# We have 17 sensors, so do 5 full deployments. 85

collision_ci(p = 0.05, n = 100)






# wilson_95_ci <- function(n_deaths, n_total) {
#   conf <- binom.confint(n_deaths, n_total, methods = "wilson")
#   
#   list(
#     survival = round((1 - conf$mean) * 100, 2),
#     lower_ci = round((1 - conf$upper) * 100, 2),
#     upper_ci = round((1 - conf$lower) * 100, 2)
#   )
# }
# 
# wilson_95_ci(7, 150)
# 
# 
# conf <- binom.confint(7, 150, methods = "wilson")
# 
# strike_rate <- round(conf$mean * 100, 2)
# lower_ci <- round(conf$lower * 100, 2)
# upper_ci <- round(conf$upper * 100, 2)
# 
# 
# conf
# strike_rate
# lower_ci
# upper_ci
