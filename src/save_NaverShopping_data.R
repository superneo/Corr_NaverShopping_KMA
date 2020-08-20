#!/usr/bin/env Rscript

# save_NaverShopping_data.R
#
# Purpose: Save Naver Shopping Insight API data
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

library(here)

Naver_Cat_ID <- "50000804"  # blouse/shirts
url <- "https://openapi.naver.com/v1/datalab/shopping/category/device"
doc <- list(startDate = NULL, endDate = NULL, timeUnit = "date",
            category = Naver_Cat_ID)

cat("Test if it works")
