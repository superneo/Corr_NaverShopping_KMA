# retrieve_KMA_API_data.R
#
# Purpose: Save the KMA ASOS hourly observation dataset via API
#          (obs. of different temporal resolutions may be supported later)
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

cat("[retrieve_KMA_API_data] start\n")

library(here)
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)
library(utils)

# predefined optional parameters
opt_params <- list(
  numOfRows = 24,
  dataType = "JSON",
  dataCd = "ASOS",
  dateCd = "HR")

if (!file.exists(paste0(here(), '/config/KMA_params.json'))) {
  stop(paste("[ERROR] (retrieve_KMA_API_data.R)",
             "KMA_params.json doesn't exist!!!"))
}

kma_params <- fromJSON(paste0(here(), "/config/KMA_params.json"))

if (kma_params$startDt >= kma_params$endDt) {
  stop(paste("[ERROR] (retrieve_KMA_API_data.R)",
             "KMA_params.json has invalid startDt or endDt."))
}

KMA_API_KEY <- URLdecode(kma_params$ServiceKey)

host_name <- "http://apis.data.go.kr"
atomic_endpoint <- "1360000/AsosHourlyInfoService/getWthrDataList"

start_hour <- "00"
end_day <- format(Sys.time() - days(1), "%Y%m%d")
end_hour <- "23"
config_end_date <- paste0(kma_params$endDt, end_hour)
if (config_end_date < paste0(end_day, end_hour)) {
  end_day <- kma_params$endDt
  end_hour <- end_hour
}
start_time <- as.POSIXct(
  paste0(kma_params$startDt, " ", start_hour, "0000"),
  format = "%Y%m%d %H%M%OS")
end_time <- as.POSIXct(
  paste0(end_day, " ", end_hour, "0000"), format = "%Y%m%d %H%M%OS")
num_hours <- as.integer(as.numeric(end_time - start_time, units = "hours") + 1)
num_pages <- as.integer((num_hours + opt_params$numOfRows - 1) /
                              opt_params$numOfRows)

stn_ids <- kma_params$stnIds

# initialize queries & data_paths
queries <- list()
data_paths <- list()
for (stn_id in stn_ids) {
  cat(paste0("[station ID] ", stn_id, "\n"))
  queries[[stn_id]] <- list(numOfRows = as.character(opt_params$numOfRows),
                            dataCd = "ASOS", dateCd = "HR", stnIds = stn_id,
                            startDt = kma_params$startDt, startHh = start_hour,
                            endDt = end_day, endHh = end_hour,
                            dataType = "JSON", serviceKey = KMA_API_KEY)
  cat(paste0("query:\n", queries[[stn_id]], "\n"))
  data_paths[[stn_id]] <- paste0(here(), "/data/KMA_ASOS_hourly_", stn_id, "/")
  cat(paste0("data_path:\n", data_paths[[stn_id]], "\n"))
}

# retrieve all requested KMA ASOS hourly observation data using API
for (stn_id in stn_ids) {
  for (cur_page in seq_len(num_pages)) {
    cat(paste0("[stn_id] ", stn_id, "\t[cur_page] ", as.character(cur_page),
               "\n"))
    queries[[stn_id]][["pageNo"]] <- as.character(cur_page)
    if (!dir.exists(data_paths[[stn_id]])) {
      dir.create(data_paths[[stn_id]])
    }

    page_start_time <- start_time + hours((cur_page - 1) * opt_params$numOfRows)
    page_end_time <- page_start_time + hours(opt_params$numOfRows - 1)
    if (page_end_time > end_time) {
      page_end_time <- end_time
    }
    file_name <- paste0(format(page_start_time, "%Y%m%d_%H"),
                        "-",
                        format(page_end_time, "%Y%m%d_%H"),
                        ".json")
    cat(paste0("[file name] ", file_name, "\n"))
    url <- paste(host_name, atomic_endpoint, sep = "/")
    res <- GET(url, query = queries[[stn_id]])
    res <- content(res)  # automatically parses JSON
    writeLines(prettify(toJSON(res, auto_unbox = TRUE), indent = 4),
               paste0(data_paths[[stn_id]], file_name),
               useBytes = TRUE)  # utf-8
  }
}

cat("[retrieve_KMA_API_data] finished\n")
