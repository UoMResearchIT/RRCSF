# Runs `iter` dummy tasks of `sleep` seconds, in parallel

library(doFuture)
library(foreach)

iter <- 24
sleep <- 0.1
cores <- availableCores() # use all available cores

cat(paste("Running", iter, "iterations of", sleep, "seconds on", cores, "cores\n"))

slow_fcn <- function(x) {
  Sys.sleep(sleep)
  x^2
}

# Register parallel backend
plan(multisession, workers = cores)

y <- foreach(x = 1:iter, .combine = c) %dofuture% {
  slow_fcn(x)
}