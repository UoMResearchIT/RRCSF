#!/usr/bin/env Rscript

library(argparser, quietly = TRUE)

parser <- arg_parser("Run a series of dummy tasks in parallel")
parser <- add_argument(parser, "--iter", default = 24, type = "integer",
                       help = "Number of tasks")
parser <- add_argument(parser, "--sleep", default = 0.1, type = "numeric",
                       help = "Sleep time")
parser <- add_argument(parser, "--cores", default = NA, type = "integer",
                       help = "Defaults to future::availableCores()")
args <- parse_args(parser)

library(doParallel)
library(doFuture)
library(foreach)

slots <- availableCores()
workers <- min(args$cores, slots, na.rm = TRUE)

cat(paste("Running", args$iter, "tasks of", args$sleep, "s\n"))

slow_fcn <- function(x) {
  Sys.sleep(args$sleep)
  x^2
}
x <- 1:args$iter

cat("Registering parallel backend\n")
system.time(
  plan(multisession, workers = workers)
)

cat(paste("Running tasks on", workers, "of", slots, "cores\n"))
system.time(
  y <- foreach(xi = x, .combine = c) %dofuture% slow_fcn(xi)
)