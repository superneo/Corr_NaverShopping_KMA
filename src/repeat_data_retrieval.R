# repeat_data_retrieval.R
#
# Purpose: Retrieve any dataset via repeated API calls (template for later)
#
# Author: KeunYoung Park
# License: MIT License
#
# Date: 2021-01
#
# Version: 1.0
# Version history:
#   1.0  initial version
#
# ==============================================================================

library(later)

retrieve_some_API_dataset <- function() {
  cat("[info] Do your API invocation here!!!\n")
}

repeat_call = function(interval = 60) {
  retrieve_some_API_dataset()
  later(repeat_call, interval)
}

repeat_call()
