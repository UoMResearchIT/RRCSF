library(doFuture)
library(foreach)
library(progressr)

slow_table <- function(i, n) {
  Sys.sleep(0.1)
  data.frame(itr = i, idx = 1:n, x = runif(n))
}

parallel_tables <- function(iter, n) {

  plan(multisession, workers = 2)

  progress <- progressor(along = 1:iter)
  foreach(i = 1:iter,
          .combine = rbind,
          .options.future = list(seed = TRUE)) %dofuture% {

    progress(sprintf("i=%g", i))
    slow_table(i, n)
  }
}
df <- parallel_tables(100, 3)