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

library(httr)
library(here)
library(lubridate)

Naver_Cat_ID <- "50000804"  # blouse/shirts
url <- "https://openapi.naver.com/v1/datalab/shopping/category/device"
doc <- list(startDate = NULL, endDate = NULL, timeUnit = "date",
            category = Naver_Cat_ID)
period_begin <- as.Date("2017-08-01")
period_end <- as.Date("2020-08-01")

args = commandArgs(trailingOnly=TRUE)

# check if 2 arguments are given
if (length(args) < 2) {
  stop(paste0("[Usage] system(paste(\"src/save_NaverShopping_data.R\", ",
              "Naver-Client-Id, Naver-Client-Secret))", "\n"),
       call. = FALSE)
}

naver_client_id <- args[1]
naver_client_secret <- args[2]

period_start <- period_begin
while (period_start < period_end) {
  period_finish <- period_start
  month(period_finish) <- month(period_finish) + 1
  period_finish <- period_finish - 1
  doc[["startDate"]] <- as.character(period_start)
  doc[["endDate"]] <- as.character(period_finish)
  res <- POST(url, body = doc, encode = "json",
              add_headers("X-Naver-Client-Id" = naver_client_id,
                          "X-Naver-Client-Secret" = naver_client_secret,
                          "Content-Type" = "application/json"))
  res <- content(res)  # automatically parses JSON
  cat("[", as.character(period_start), "] doc\n")  # for debugging
  print(doc)  # for debugging
  cat("[", as.character(period_start), "] res\n")  # for debugging
  print(res)  # for debugging
  month(period_start) <- month(period_start) + 1
  break  # for debugging
}
