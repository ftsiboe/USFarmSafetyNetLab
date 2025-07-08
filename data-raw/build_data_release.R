#-------------------------------------------------------------------------------
# Preliminaries                                                              ####
rm(list = ls(all = TRUE))
# usethis::edit_r_environ()
# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")

# Clean dn deply HISTORICAL SUMMARY OF BUSINESS and CAUSE OF LOSS DATA as a data release
devtools::document()

# Directory to store cached calibrations
dir_data_release <- "data-raw/data_release/"

# Create the cache directory if it does not already exist
if (!dir.exists(dir_data_release)) {
  dir.create(dir_data_release, recursive = TRUE)
}

#-------------------------------------------------------------------------------
# Prepare Databases                                                          ####
# Prepare FCIP Data for Release
prep_fcip_data(dir_dest = dir_data_release)

# Prepare and Save USDA NASS Data for Release
prep_nass_data(dir_dest = paste0(dir_data_release,"/nass/"), 
               dir_source = "./data-raw/fastscratch/nass/")

#-------------------------------------------------------------------------------
# Send Summary of Business to Github                                         ####

tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "sob")
}, error = function(e){NULL})
piggyback::pb_release_create(
  repo = "ftsiboe/US-FarmSafetyNet-Lab", 
  tag = "sob",
  name = "Summary of Business",
  body = paste("Summary of Business data breaks out FCIP participation at variaous levels:",
               paste0("**sobtpu** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their contract choice (i), the insurance pool they selected (j), ",
                      "and the crop year (t). Contract choices combine insurance plan (e.g., APH, RP), ",
                      "coverage level, and unit structure (e.g., Optional [OU], Enterprise [EU]). ",
                      "Pools are the most granular rate‐setting level and are distinguished by county, commodity, ",
                      "crop type, and practice (e.g., irrigated, organic).", collapse = ""),
               paste0("**sobcov** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their coverage level, county, commodity, and  commodity year.", collapse = ""),
               paste0("**sobscc** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their county, commodity, and  commodity year.", collapse = ""),sep = "\n\n"))
piggyback::pb_new_release( repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "sob")
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/sob"), full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "sob",overwrite = TRUE)

#-------------------------------------------------------------------------------
# Send Cause of Loss to Github                                               ####
tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "col")
}, error = function(e){NULL})
piggyback::pb_release_create(
  repo = "ftsiboe/US-FarmSafetyNet-Lab", 
  tag  = "col",
  name = "Cause of Loss",
  body = "Cause of Loss breaks out FCIP participation by peril")
piggyback::pb_new_release( repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "col")
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "col",overwrite = TRUE)

#-------------------------------------------------------------------------------
# Send Actuarial Data Master to Github                                       ####
tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "adm")
}, error = function(e){NULL})

piggyback::pb_release_create(
  repo = "ftsiboe/US-FarmSafetyNet-Lab", 
  tag = "adm",
  name = "Actuarial Data Master",
  body = "Various items aggregated from the FCIP's Actuarial Data Master")
piggyback::pb_new_release( repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "adm")
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/adm"), full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "adm",overwrite = TRUE)

#-------------------------------------------------------------------------------

# Livestock Gross Margin – Summary of Business Data 
# download_rma_web_data_files(2003:as.numeric(format(Sys.Date(),"%Y")), "lgm")

# Livestock Risk Protection – Summary of Business Data
# download_rma_web_data_files(2003:as.numeric(format(Sys.Date(),"%Y")), "lrp")

