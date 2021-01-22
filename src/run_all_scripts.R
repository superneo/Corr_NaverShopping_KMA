# run_all_scripts.R
#
# Purpose: Run all scripts in their separate environment
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

library(here)

env_kma <- new.env(parent = parent.frame())
env_rmd <- new.env(parent = parent.frame())

sys.source(paste0(here(), "/src/retrieve_KMA_API_data.R"), envir = env_kma)
sys.source(paste0(here(), "/src/make_results.R"), envir = env_rmd)
