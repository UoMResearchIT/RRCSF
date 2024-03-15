library(Rmpi)
library(snow)

library(doFuture)
library(foreach)

n_cores <- availableCores()
workers <- mpi.universe.size() - 1

cat("availableCores reports", n_cores, "cores\n")
cat("mpi.universe.size reports", workers + 1, "workers\n")

cl <- snow::makeMPIcluster(count = workers, type = "MPI")

registerDoFuture()
plan(cluster, workers = cl)

slow_sqrt <- function(x) {
  Sys.sleep(1.0)  ## 1 second emulated slowness
  sqrt(x)
}

X <- 1:48
z <- foreach(x = X, .combine = c) %dopar% { slow_sqrt(x) }

# IMPORTANT: if you don't use these two, the job will hang and never finish
# (the same happens if you try to use snow::stopCluster)
mpi.close.Rslaves()
mpi.exit()