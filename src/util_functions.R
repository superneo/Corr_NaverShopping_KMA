# util_functions.R
#
# Purpose: Define utility functions for the project
#
# Author: KeunYoung Park
# License: MIT License
#
# Date: 2020-08
#
# Version: 1.0
# Version history:
#   1.0  initial version
#
# ==============================================================================

library(stringr)

get_latest_file <- function(path) {
  tmpshot <- fileSnapshot(path)

  return(rownames(tmpshot$info[which.max(tmpshot$info$mtime),]))
}

mov_avg <- function(seq, days) {
  delta <- 10

  if (length(seq) < days) {
    return(rep(NA, days - 1))
  }

  res <- sapply(days:length(seq), function(i) {
    rng <- (i - days + 1):i
    weights <- exp(-abs(i - rng)/delta)
    return(sum(seq[rng] * weights) / sum(weights))
  })

  return(c(rep(NA, days - 1), res))
}
