
# alleen trendberekening
integrated_random_walk <- function(y) {
  library(dlm)
  
  build_model <- function(par) {
    dlmModPoly(
      order = 2,
      dV = exp(par[1]),
      dW = c(0, exp(par[2]))
    )
  }
  
  fit <- dlmMLE(y,
                parm = log(c(var(y), 0.01)),
                build = build_model)
  
  model <- build_model(fit$par)
  
  filt <- dlmFilter(y, model)
  smooth <- dlmSmooth(filt)
  
  trend <- dropFirst(smooth$s[,1])
  
  return(trend)
}


# ook onzekerheid en stijging/daling classificatie
# 
trendspotter_irw <- function(y, k = 4) {
  library(dlm)
  
  # --- checks ---
  y <- as.numeric(y)
  if (any(!is.finite(y))) stop("y contains NA/Inf")
  
  v0 <- max(var(y), 1e-6)
  
  # --- model ---
  build_model <- function(par) {
    dlmModPoly(
      order = 2,
      dV = exp(par[1]),
      dW = c(0, exp(par[2]))
    )
  }
  
  fit <- try(
    dlmMLE(
      y,
      parm = log(c(v0, v0/10)),
      build = build_model
    ),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) {
    model <- dlmModPoly(order = 2, dV = v0, dW = c(0, v0/10))
  } else {
    model <- build_model(fit$par)
  }
  
  filt <- dlmFilter(y, model)
  smooth <- dlmSmooth(filt)
  
  # --- trend ---
  mu <- dropFirst(smooth$s[,1])
  
  # --- variance ---
  cov_array <- dlmSvd2var(smooth$U.S, smooth$D.S)
  
  var_mu <- sapply(cov_array, function(m) m[1,1])
  var_mu <- var_mu[-1]   # <-- BELANGRIJK ✅
  
  sd_mu <- sqrt(pmax(var_mu, 1e-12))
  
  
  # --- 95% CI ---
  lower <- mu - 1.96 * sd_mu
  upper <- mu + 1.96 * sd_mu
  
  # --- trend detection ---
  n <- length(mu)
  prob_up <- rep(NA, n)
  
  for (t in (k+1):n) {
    mean_diff <- mu[t] - mu[t-k]
    var_diff <- var_mu[t] + var_mu[t-k]
    sd_diff <- sqrt(max(var_diff, 1e-8))
    prob_up[t] <- pnorm(mean_diff / sd_diff)
  }
  
  classify <- function(p) {
    if (is.na(p)) return(NA)
    if (p > 0.95) return("strong increase")
    if (p > 0.8)  return("increase")
    if (p > 0.2)  return("stable")
    if (p > 0.05) return("decrease")
    return("strong decrease")
  }
  
  trend_class <- sapply(prob_up, classify)
  
  return(list(
    data = data.frame(
      time = seq_along(y),
      y = y,
      trend = mu,
      lower95 = lower,
      upper95 = upper,
      prob_increase = prob_up,
      class = trend_class
    ),
    smooth = smooth   # <-- BELANGRIJK ✅
  ))
  
}

compute_slope_from_state <- function(smooth) {
  
  # slope zit in state 2
  slope <- dropFirst(smooth$s[,2])
  
  cov_array <- dlmSvd2var(smooth$U.S, smooth$D.S)
  cov_array <- cov_array[-1]
  
  var_slope <- sapply(cov_array, function(m) m[2,2])
  sd_slope <- sqrt(pmax(var_slope, 1e-12))
  
  lower <- slope - 1.96 * sd_slope
  upper <- slope + 1.96 * sd_slope
  
  data.frame(
    slope = slope,
    lower95 = lower,
    upper95 = upper
  )
}
