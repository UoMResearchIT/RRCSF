# Runs a number of dummy tasks in parallel
# Arguments can be passed from the command line, e.g.:
#
#   R CMD BATCH --args "--sleep 1 --cores 8" parallel_tester.R
#   Rscript parallel_tester.R --iter 32 --cores 4 > tester.log
#
# Arguments:
#   --iter: Number of tasks, defaults to 24
#   --sleep: Sleep time (seconds), defaults to 0.1
#   --cores: Defaults to future::availableCores()
#            If `cores` is 1, the tasks will be run sequentially.

library(argparser, quietly = TRUE)
library(doFuture)
library(foreach)
library(progressr)

parser <- arg_parser("Run a series of dummy tasks in parallel")
parser <- add_argument(parser, "--iter", default = 24, type = "integer",
                       help = "Number of tasks")
parser <- add_argument(parser, "--sleep", default = 0.1, type = "numeric",
                       help = "Sleep time")
parser <- add_argument(parser, "--cores", default = NA, type = "integer",
                       help = "Defaults to future::availableCores()")
args <- parse_args(parser)

slots <- availableCores()
workers <- min(args$cores, slots, na.rm = TRUE)

cat(paste("Running", args$iter, "iterations of", args$sleep, "s\n"))

slow_fcn <- function(x) {
  Sys.sleep(args$sleep)
  x^2
}

if (workers > 1) {
  cat("Registering parallel backend\n")
  system.time(
    plan(multisession, workers = workers)
  )
  cat(paste("Running tasks on", workers, "/", slots, "cores\n"))

  parallel_fcn <- function(its) {

    progress <- progressor(along = 1:its)

    foreach(x = 1:its, .combine = c) %dofuture% {
      progress(sprintf("x=%g", x))
      slow_fcn(x)
    }
  }
} else {
  cat("Running tasks sequentially\n")

  parallel_fcn <- function(its) {

    progress <- progressor(along = 1:its)

    y <- vector(mode = "numeric", length = its)
    for (x in seq_len(its)) {
      progress(sprintf("x=%g", x))
      y[x] <- slow_fcn(x)
    }
  }
}

system.time(
  parallel_fcn(args$iter)
)
