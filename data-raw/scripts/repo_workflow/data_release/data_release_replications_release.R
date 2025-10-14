rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

rm(list = ls(all = TRUE))

source("data-raw/scripts/repo_workflow/environment_setup.R")

# Upload 
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/reps"), full.names = TRUE, recursive = TRUE),
  repo  = "ftsiboe/USFarmSafetyNetLab",
  tag   = "reps",
  overwrite = TRUE
)

