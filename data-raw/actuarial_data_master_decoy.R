# adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/"
rm(list = ls(all = TRUE)); gc()
devtools::document()
library(future.apply)
build_adm_test_zip(
  year = 2020,
  dir_fastscratch = "./data-raw/fastscratch/adm_decoy",
  dir_release = "./data-raw/data_release/adm_decoy")


# Set up parallel plan
# plan(sequential)
# plan(list(tweak(multisession, workers = availableCores())));gc()

lapply(
  2011:2026, 
  build_adm_test_zip,
  dir_fastscratch = "./data-raw/fastscratch/adm_decoy",
  dir_release = "./data-raw/data_release/adm_decoy")

# plan(sequential)
# gc()


tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "adm_decoy")
}, error = function(e){NULL})

piggyback::pb_release_create(
  repo = "ftsiboe/US-FarmSafetyNet-Lab", 
  tag  = "adm_decoy",
  name = "Actuarial Data Master - Decoy",
  body = "Reproducible slice of the USDA RMA Actuarial Data Master, purpose-built for fast, reliable testthat unit-testing in downstream repos")

piggyback::pb_new_release( repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "adm_decoy")

piggyback::pb_upload(
  list.files("./data-raw/data_release/adm_decoy", full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "adm_decoy",overwrite = TRUE)

# usethis::edit_r_environ()
