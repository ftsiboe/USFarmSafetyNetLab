
source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/spatial_features"), full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "spatial_features",overwrite = TRUE)

