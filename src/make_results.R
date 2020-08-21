# make_results.R
#
# Purpose: Render R markdowns into the given output path
#
# Author: KeunYoung Park/KLab/IBM Korea
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
library(rmarkdown)

results_path = paste0(here(), "/results")

do_correlate_kma_nsi = TRUE

correlate_kma_nsi <- function() {
  render(input = paste0(here(), "/src/Correlate_KMA_NSI.Rmd"),
         output_file = "Correlate_KMA_NSI",
         output_dir = results_path,
         params = list(start_date = "2017-08-01",
                       end_date = "2020-07-31"))
}

if (do_correlate_kma_nsi) {
  correlate_kma_nsi()
}
