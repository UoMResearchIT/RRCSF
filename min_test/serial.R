#!/usr/bin/env Rscript

library(argparser, quietly = TRUE)

parser <- arg_parser("Run a series of tasks sequentially")
parser <- add_argument(parser, "--iter", default = 24, type = "integer",
                       help = "Number of tasks")
parser <- add_argument(parser, "--sleep", default = 0.1, type = "numeric",
                       help = "Sleep time")
args <- parse_args(parser)

slow_fcn <- function(x) {
  Sys.sleep(args$sleep)
  x^2
}
x <- 1:args$iter

cat(paste("Running", args$iter, "tasks of", args$sleep, "s (sequentially)\n"))

y <- c()
system.time(
  for (i in seq_along(x)) {
    y[i] <- slow_fcn(x[i])
  }
)