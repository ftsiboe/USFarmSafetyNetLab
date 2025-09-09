rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

rm(list = ls(all = TRUE))

source("data-raw/scripts/environment_setup.R")

# Verify auth first (nice sanity check)
if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# 1) Delete the release if it exists (ignore 404s)
tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/USFarmSafetyNetLab", tag = "reps")
}, error = function(e) NULL)

# 2) Create the release (once)
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "reps",
  name = "Replications",
  body = "Various items aggregated from replications"
)

# 3) Upload the assets
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/reps"), full.names = TRUE, recursive = TRUE),
  repo  = "ftsiboe/USFarmSafetyNetLab",
  tag   = "reps",
  overwrite = TRUE
)

# piggyback::pb_upload(
#   list.files(paste0(dir_data_release,"/ice"), full.names = TRUE, recursive = TRUE),
#   repo = "ftsiboe/USFarmSafetyNetLab", tag  = "ice",overwrite = TRUE)