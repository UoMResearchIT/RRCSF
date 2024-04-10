# Runs `iter` dummy tasks of `sleep` seconds, sequentially

sleep <- 0.1
iter <- 24

slow_fcn <- function(x) {
  Sys.sleep(sleep)
  x^2
}

y <- c()
for (i in 1:iter) {
  y[i] <- slow_fcn(i)
}