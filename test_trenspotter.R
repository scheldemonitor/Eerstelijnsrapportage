
source("r/IRW_functions.R")
require(dlm)
?dlm

set.seed(1)
y <- cumsum(cumsum(rnorm(100))) + rnorm(100, sd = 15)

res <- integrated_random_walk(y)

plot(y, type = "p", col = "black")
lines(res, col = "blue", lwd = 2)



# trendspotter met CI

res <- trendspotter_irw(y, k = 5)

df_out <- res$data
smooth_obj <- res$smooth


plot(df_out$time, df_out$y, type="l", col="grey",
     xlab="Year", ylab="Sea level")

lines(df_out$time, df_out$trend, col="blue", lwd=2)

# CI band
lines(df_out$time, df_out$lower95, col="blue", lty=2)
lines(df_out$time, df_out$upper95, col="blue", lty=2)


# first derivative

slope_res <- compute_slope_from_state(smooth_obj)

df_slope <- data.frame(
  year = df_out$time[],
  slope = slope_res$slope,
  lower95 = slope_res$lower95,
  upper95 = slope_res$upper95
)

require(ggplot2)
ggplot(df_slope, aes(year, slope)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = 0)




res5  <- trendspotter_irw(y, k = 5)
res30 <- trendspotter_irw(y, k = 30)

plot(res5$data$prob_increase, type="l", col="blue")
lines(res30$data$prob_increase, col="red")
legend("bottomright", legend=c("k=5", "k=30"),
       col=c("blue","red"), lty=1)
