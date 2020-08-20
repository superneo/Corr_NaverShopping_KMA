# get_kma_obs.R
#
# Purpose: Retrieve the KMA observations
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

source(paste0(here(), "/src/util_functions.R"))

KMA_Regions <- c("Seoul", "Busan")
KMA_Region_Path <- list("Seoul" = "data/KMA_Seoul",
                        "Busan" = "data/KMA_Busan")
KMA_Read <- list("Seoul" = FALSE, "Busan" = FALSE)
KMA_DF <- list("Seoul" = NULL, "Busan" = NULL)
KMA_YM_1st <- list("Seoul" = NULL, "Busan" = NULL)
KMA_YM_Last <- list("Seoul" = NULL, "Busan" = NULL)

# raw/derived KMA temperature features
kma_feat_names <- c(
  "avg_temp",
  "avg_temp_d1", "avg_temp_d2", "avg_temp_d3",
  "avg_temp_w1", "avg_temp_w2", "avg_temp_w3",
  "avg_temp_m1", "avg_temp_m2", "avg_temp_m3",
  "avg_temp_y1", "avg_temp_y2", "avg_temp_y3",
  "avg_temp_diff_d1", "avg_temp_diff_d2", "avg_temp_diff_d3",
  "avg_temp_diff_w1", "avg_temp_diff_w2", "avg_temp_diff_w3",
  "avg_temp_diff_m1", "avg_temp_diff_m2", "avg_temp_diff_m3",
  "avg_temp_diff_y1", "avg_temp_diff_y2", "avg_temp_diff_y3",
  "max_temp",
  "max_temp_d1", "max_temp_d2", "max_temp_d3",
  "max_temp_w1", "max_temp_w2", "max_temp_w3",
  "max_temp_m1", "max_temp_m2", "max_temp_m3",
  "max_temp_y1", "max_temp_y2", "max_temp_y3",
  "max_temp_diff_d1", "max_temp_diff_d2", "max_temp_diff_d3",
  "max_temp_diff_w1", "max_temp_diff_w2", "max_temp_diff_w3",
  "max_temp_diff_m1", "max_temp_diff_m2", "max_temp_diff_m3",
  "max_temp_diff_y1", "max_temp_diff_y2", "max_temp_diff_y3",
  "min_temp",
  "min_temp_d1", "min_temp_d2", "min_temp_d3",
  "min_temp_w1", "min_temp_w2", "min_temp_w3",
  "min_temp_m1", "min_temp_m2", "min_temp_m3",
  "min_temp_y1", "min_temp_y2", "min_temp_y3",
  "min_temp_diff_d1", "min_temp_diff_d2", "min_temp_diff_d3",
  "min_temp_diff_w1", "min_temp_diff_w2", "min_temp_diff_w3",
  "min_temp_diff_m1", "min_temp_diff_m2", "min_temp_diff_m3",
  "min_temp_diff_y1", "min_temp_diff_y2", "min_temp_diff_y3",
  "avg_temp_ma_7",
  "avg_temp_ma_7_d1", "avg_temp_ma_7_d2", "avg_temp_ma_7_d3",
  "avg_temp_ma_7_w1", "avg_temp_ma_7_w2", "avg_temp_ma_7_w3",
  "avg_temp_ma_7_m1", "avg_temp_ma_7_m2", "avg_temp_ma_7_m3",
  "avg_temp_ma_7_y1", "avg_temp_ma_7_y2", "avg_temp_ma_7_y3",
  "avg_temp_ma_7_diff_d1", "avg_temp_ma_7_diff_d2", "avg_temp_ma_7_diff_d3",
  "avg_temp_ma_7_diff_w1", "avg_temp_ma_7_diff_w2", "avg_temp_ma_7_diff_w3",
  "avg_temp_ma_7_diff_m1", "avg_temp_ma_7_diff_m2", "avg_temp_ma_7_diff_m3",
  "avg_temp_ma_7_diff_y1", "avg_temp_ma_7_diff_y2", "avg_temp_ma_7_diff_y3",
  "max_temp_ma_7",
  "max_temp_ma_7_d1", "max_temp_ma_7_d2", "max_temp_ma_7_d3",
  "max_temp_ma_7_w1", "max_temp_ma_7_w2", "max_temp_ma_7_w3",
  "max_temp_ma_7_m1", "max_temp_ma_7_m2", "max_temp_ma_7_m3",
  "max_temp_ma_7_y1", "max_temp_ma_7_y2", "max_temp_ma_7_y3",
  "max_temp_ma_7_diff_d1", "max_temp_ma_7_diff_d2", "max_temp_ma_7_diff_d3",
  "max_temp_ma_7_diff_w1", "max_temp_ma_7_diff_w2", "max_temp_ma_7_diff_w3",
  "max_temp_ma_7_diff_m1", "max_temp_ma_7_diff_m2", "max_temp_ma_7_diff_m3",
  "max_temp_ma_7_diff_y1", "max_temp_ma_7_diff_y2", "max_temp_ma_7_diff_y3",
  "min_temp_ma_7",
  "min_temp_ma_7_d1", "min_temp_ma_7_d2", "min_temp_ma_7_d3",
  "min_temp_ma_7_w1", "min_temp_ma_7_w2", "min_temp_ma_7_w3",
  "min_temp_ma_7_m1", "min_temp_ma_7_m2", "min_temp_ma_7_m3",
  "min_temp_ma_7_y1", "min_temp_ma_7_y2", "min_temp_ma_7_y3",
  "min_temp_ma_7_diff_d1", "min_temp_ma_7_diff_d2", "min_temp_ma_7_diff_d3",
  "min_temp_ma_7_diff_w1", "min_temp_ma_7_diff_w2", "min_temp_ma_7_diff_w3",
  "min_temp_ma_7_diff_m1", "min_temp_ma_7_diff_m2", "min_temp_ma_7_diff_m3",
  "min_temp_ma_7_diff_y1", "min_temp_ma_7_diff_y2", "min_temp_ma_7_diff_y3",
  "avg_temp_ma_30",
  "avg_temp_ma_30_d1", "avg_temp_ma_30_d2", "avg_temp_ma_30_d3",
  "avg_temp_ma_30_w1", "avg_temp_ma_30_w2", "avg_temp_ma_30_w3",
  "avg_temp_ma_30_m1", "avg_temp_ma_30_m2", "avg_temp_ma_30_m3",
  "avg_temp_ma_30_y1", "avg_temp_ma_30_y2", "avg_temp_ma_30_y3",
  "avg_temp_ma_30_diff_d1", "avg_temp_ma_30_diff_d2", "avg_temp_ma_30_diff_d3",
  "avg_temp_ma_30_diff_w1", "avg_temp_ma_30_diff_w2", "avg_temp_ma_30_diff_w3",
  "avg_temp_ma_30_diff_m1", "avg_temp_ma_30_diff_m2", "avg_temp_ma_30_diff_m3",
  "avg_temp_ma_30_diff_y1", "avg_temp_ma_30_diff_y2", "avg_temp_ma_30_diff_y3",
  "max_temp_ma_30",
  "max_temp_ma_30_d1", "max_temp_ma_30_d2", "max_temp_ma_30_d3",
  "max_temp_ma_30_w1", "max_temp_ma_30_w2", "max_temp_ma_30_w3",
  "max_temp_ma_30_m1", "max_temp_ma_30_m2", "max_temp_ma_30_m3",
  "max_temp_ma_30_y1", "max_temp_ma_30_y2", "max_temp_ma_30_y3",
  "max_temp_ma_30_diff_d1", "max_temp_ma_30_diff_d2", "max_temp_ma_30_diff_d3",
  "max_temp_ma_30_diff_w1", "max_temp_ma_30_diff_w2", "max_temp_ma_30_diff_w3",
  "max_temp_ma_30_diff_m1", "max_temp_ma_30_diff_m2", "max_temp_ma_30_diff_m3",
  "max_temp_ma_30_diff_y1", "max_temp_ma_30_diff_y2", "max_temp_ma_30_diff_y3",
  "min_temp_ma_30",
  "min_temp_ma_30_d1", "min_temp_ma_30_d2", "min_temp_ma_30_d3",
  "min_temp_ma_30_w1", "min_temp_ma_30_w2", "min_temp_ma_30_w3",
  "min_temp_ma_30_m1", "min_temp_ma_30_m2", "min_temp_ma_30_m3",
  "min_temp_ma_30_y1", "min_temp_ma_30_y2", "min_temp_ma_30_y3",
  "min_temp_ma_30_diff_d1", "min_temp_ma_30_diff_d2", "min_temp_ma_30_diff_d3",
  "min_temp_ma_30_diff_w1", "min_temp_ma_30_diff_w2", "min_temp_ma_30_diff_w3",
  "min_temp_ma_30_diff_m1", "min_temp_ma_30_diff_m2", "min_temp_ma_30_diff_m3",
  "min_temp_ma_30_diff_y1", "min_temp_ma_30_diff_y2", "min_temp_ma_30_diff_y3",
  "range",
  "range_d1", "range_d2", "range_d3",
  "range_w1", "range_w2", "range_w3",
  "range_m1", "range_m2", "range_m3",
  "range_y1", "range_y2", "range_y3",
  "range_diff_d1", "range_diff_d2", "range_diff_d3",
  "range_diff_w1", "range_diff_w2", "range_diff_w3",
  "range_diff_m1", "range_diff_m2", "range_diff_m3",
  "range_diff_y1", "range_diff_y2", "range_diff_y3",
  "range_sq",
  "range_sq_d1", "range_sq_d2", "range_sq_d3",
  "range_sq_w1", "range_sq_w2", "range_sq_w3",
  "range_sq_m1", "range_sq_m2", "range_sq_m3",
  "range_sq_y1", "range_sq_y2", "range_sq_y3",
  "range_sq_diff_d1", "range_sq_diff_d2", "range_sq_diff_d3",
  "range_sq_diff_w1", "range_sq_diff_w2", "range_sq_diff_w3",
  "range_sq_diff_m1", "range_sq_diff_m2", "range_sq_diff_m3",
  "range_sq_diff_y1", "range_sq_diff_y2", "range_sq_diff_y3",
  "log_ratio",
  "log_ratio_d1", "log_ratio_d2", "log_ratio_d3",
  "log_ratio_w1", "log_ratio_w2", "log_ratio_w3",
  "log_ratio_m1", "log_ratio_m2", "log_ratio_m3",
  "log_ratio_y1", "log_ratio_y2", "log_ratio_y3",
  "log_ratio_diff_d1", "log_ratio_diff_d2", "log_ratio_diff_d3",
  "log_ratio_diff_w1", "log_ratio_diff_w2", "log_ratio_diff_w3",
  "log_ratio_diff_m1", "log_ratio_diff_m2", "log_ratio_diff_m3",
  "log_ratio_diff_y1", "log_ratio_diff_y2", "log_ratio_diff_y3")

get_clean_raw_KMA <- function(region) {
  if (!(region %in% KMA_Regions)) {
    cat("[ERROR] (get_clean_raw_KMA) invalid region passed!!!")
    return(NULL)
  }

  if (KMA_Read[[region]] & !is.null(KMA_DF[[region]])) {
    return(KMA_DF[[region]])
  }

  kma_read <- KMA_Read
  kma_df <- KMA_DF
  kma_ym_1st <- KMA_YM_1st
  kma_ym_last <- KMA_YM_Last

  path_to_data <- paste0(here(), "/", KMA_Region_Path[[region]])
  df <-read.csv(paste0(path_to_data, "/", get_latest_file(path_to_data)),
                fileEncoding = "EUC-KR")
  cols <- read.delim(paste0(here(), "/data/KMA_header.txt"), header = FALSE,
                     sep = "\n")[[1]]
  colnames(df) <- cols
  # df$date <- str_replace_all(df$date, "-", "/")
  df$year <- sapply(strsplit(df$date, split="-", fixed = T),
                    function(x) { x[1] })
  df$month <- sapply(strsplit(df$date, split="-", fixed = T),
                     function(x) { x[2] })
  df$ym <- apply(df[ , c("year", "month")], 1, paste, collapse = "-")
  df[which(is.na(df$daily_precip_mm)), "daily_precip_mm"] <- 0.0
  df <- df[complete.cases(df), ]
  rownames(df) <- NULL  # correct row names

  kma_read[[region]] <- TRUE
  kma_df[[region]] <- df
  kma_ym_1st[[region]] <- min(df$ym)
  kma_ym_last[[region]] <- max(df$ym)

  assign("KMA_Read", kma_read, envir = .GlobalEnv)
  assign("KMA_DF", kma_df, envir = .GlobalEnv)
  assign("KMA_YM_1st", kma_ym_1st, envir = .GlobalEnv)
  assign("KMA_YM_Last", kma_ym_last, envir = .GlobalEnv)

  df <- NULL
  kma_read <- NULL
  kma_df <- NULL
  kma_ym_1st <- NULL
  kma_ym_last <- NULL

  return(KMA_DF[[region]])
}

get_KMA_subset <- function(region, year_start, month_start, year_end,
                           month_end) {
  if (!(region %in% KMA_Regions)) {
    cat("[ERROR] (get_KMA_subset) invalid region passed!!!")
    return(NULL)
  }

  if (!KMA_Read[[region]] | is.null(KMA_DF[[region]])) {
    invisible(capture.output(get_clean_raw_KMA(region)))
  }

  ym_start <- paste(c(as.character(year_start),
                      sprintf("%02d", as.integer(month_start))), collapse = "-")
  ym_end <- paste(c(as.character(year_end),
                    sprintf("%02d", as.integer(month_end))), collapse = "-")

  if (ym_start > ym_end) {
    cat(paste0(
      "[ERROR] (get_KMA_subset) invalid period boundaries passed!!!\n"))
    return(NULL)
  }

  if (ym_start < KMA_YM_1st[[region]] | ym_end > KMA_YM_Last[[region]]) {
    cat(paste0(
      "[ERROR] (get_KMA_subset) invalid period boundaries passed!!!\n"))
    return(NULL)
  }

  subframe <- KMA_DF[[region]][which((ym_start <= KMA_DF[[region]]$ym) &
                                       (KMA_DF[[region]]$ym <= ym_end)), ]
  rownames(subframe) <- NULL

  # reset_KMA_data()

  return(subframe)
}

add_base_derived_features <- function(region) {
  if (!(region %in% KMA_Regions)) {
    cat("[ERROR] (add_base_derived_features) invalid region passed!!!")
    return(NULL)
  }

  if (!KMA_Read[[region]] | is.null(KMA_DF[[region]])) {
    invisible(capture.output(get_clean_raw_KMA(region)))
  }

  kma_df <- KMA_DF

  kma_df[[region]]$range <-
    kma_df[[region]]$max_temp - kma_df[[region]]$min_temp
  kma_df[[region]]$range_sq <- (kma_df[[region]]$range)^2
  kma_df[[region]]$log_ratio <-
    log((kma_df[[region]]$max_temp - kma_df[[region]]$avg_temp)/
          (kma_df[[region]]$avg_temp - kma_df[[region]]$min_temp))
  kma_df[[region]]$avg_temp_ma_7 <- mov_avg(kma_df[[region]]$avg_temp, 7)
  kma_df[[region]]$max_temp_ma_7 <- mov_avg(kma_df[[region]]$max_temp, 7)
  kma_df[[region]]$min_temp_ma_7 <- mov_avg(kma_df[[region]]$min_temp, 7)
  kma_df[[region]]$avg_temp_ma_30 <- mov_avg(kma_df[[region]]$avg_temp, 30)
  kma_df[[region]]$max_temp_ma_30 <- mov_avg(kma_df[[region]]$max_temp, 30)
  kma_df[[region]]$min_temp_ma_30 <- mov_avg(kma_df[[region]]$min_temp, 30)

  assign("KMA_DF", kma_df, envir = .GlobalEnv)
  kma_df <- NULL

  return(KMA_DF[[region]])
}

derive_kma_feature <- function(region, feat_name) {
  if (!(region %in% KMA_Regions)) {
    cat("[ERROR] (derive_kma_feature) invalid region passed!!!")
    return(NULL)
  }

  if (!KMA_Read[[region]] | is.null(KMA_DF[[region]])) {
    invisible(capture.output(get_clean_raw_KMA(region)))
    add_base_derived_features(region)
  }

  if (feat_name %in% colnames(KMA_DF[[region]])) {
    return(KMA_DF[[region]][, feat_name])
  }

  is_past <- str_detect(feat_name, "_[dwmy][\\d]+$")
  if (!is_past) {
    cat("[ERROR] (derive_kma_feature) invalid feature name passed!!!")
    return(NULL)
  }

  period_days <- list("d" = 1, "w" = 7, "m" = 30, "y" = 365)
  is_diff <- str_detect(feat_name, "_diff_")
  if (is_diff) {
    offsets <- str_locate(feat_name, "_diff_")
    org_feat <- substr(feat_name, 1, offsets[1, 1] - 1)
    period <- substr(feat_name, offsets[1, 2] + 1, offsets[1, 2] + 1)
    step <- as.integer(substr(feat_name, offsets[1, 2] + 2, nchar(feat_name)))
    delta <- period_days[[period]] * step
    feat_vec <- KMA_DF[[region]][, org_feat] - c(rep(NA, delta),
      KMA_DF[[region]][1:(nrow(KMA_DF[[region]]) - delta), org_feat])
  } else {
    offsets <- str_locate(feat_name, "_[dwmy][\\d]+$")
    org_feat <- substr(feat_name, 1, offsets[1, 1] - 1)
    period <- substr(feat_name, offsets[1, 1] + 1, offsets[1, 1] + 1)
    step <- as.integer(substr(feat_name, offsets[1, 1] + 2, nchar(feat_name)))
    delta <- period_days[[period]] * step
    feat_vec <- c(rep(NA, delta),
      KMA_DF[[region]][1:(nrow(KMA_DF[[region]]) - delta), org_feat])
  }

  return(feat_vec)
}

reset_KMA_data <- function() {
  assign("KMA_Read", list("Seoul" = FALSE, "Busan" = FALSE), envir = .GlobalEnv)
  assign("KMA_DF", list("Seoul" = NULL, "Busan" = NULL), envir = .GlobalEnv)
  assign("KMA_YM_1st", list("Seoul" = NULL, "Busan" = NULL),
         envir = .GlobalEnv)
  assign("KMA_YM_Last", list("Seoul" = NULL, "Busan" = NULL),
         envir = .GlobalEnv)
}
