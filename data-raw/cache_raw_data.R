rm(list = ls(all = TRUE))

# Clean dn deply HISTORICAL SUMMARY OF BUSINESS and CAUSE OF LOSS DATA as a data release
devtools::document()

if(Sys.info()['sysname'] %in% "Windows"){
  farmpolicylab <- paste0(gsub("OneDrive","Dropbox",Sys.getenv("OneDriveConsumer")),"/farmpolicylab/database/")
}

saveRDS(standardize_fcip_column_names(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaSumOfBussiness/Output/SOBCOV.rds"))),
        file=paste0("data-raw/data_release/historical_summary_of_business_by_state_county_crop_coverage.rds"))

saveRDS(standardize_fcip_column_names(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaSumOfBussiness/Output/SOBSCC.rds"))),
        file=paste0("data-raw/data_release/historical_summary_of_business_by_state_county_crop.rds"))

saveRDS(standardize_fcip_column_names(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaCauseOfLoss/Output/COLIND.rds"))),
        file=paste0("data-raw/data_release/historical_cause_of_loss_indemnities_with_month.rds"))

saveRDS(standardize_fcip_column_names(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaCauseOfLoss/Output/COLINDMNT.rds"))),
        file=paste0("data-raw/data_release/historical_cause_of_loss_indemnities_only.rds"))

saveRDS(standardize_fcip_column_names(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaCauseOfLoss/Output/COLINDPRM.rds"))),
        file=paste0("data-raw/data_release/historical_cause_of_loss_premimums_and_indemnities.rds"))

# Create a new GitHub release via piggyback for the calibrated data
piggyback::pb_new_release(
  repo = "ftsiboe/US-FarmSafetyNet-Lab",
  tag  = "v0.1.0",
  name = "Initial release",
  body = ""
)

# Upload the first two .rds files to the new release
piggyback::pb_upload(
  list.files(paste0("data-raw/data_release"), "\\.rds$", full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/US-FarmSafetyNet-Lab",
  tag  = "v0.1.0",
  name = "Initial release",
  overwrite = TRUE
)

# Clean up by removing the release tag (if needed)
piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "v0.1.0")

