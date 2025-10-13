source("data-raw/scripts/environment_setup.R")

# Send spatial features to Github
 tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/USFarmSafetyNetLab", tag = "spatial_features")
}, error = function(e){NULL})

piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag = "spatial_features",
  name = "Spatial features",
  body = "A collection of frequently used spatial features")

piggyback::pb_new_release( repo = "ftsiboe/USFarmSafetyNetLab", tag  = "spatial_features")

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/spatial_features"), full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "spatial_features",overwrite = TRUE)



