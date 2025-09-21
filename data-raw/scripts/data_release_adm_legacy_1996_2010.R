
source("data-raw/scripts/environment_setup.R")

devtools::document()

# unlink(list.files(paste0(dir_data_release,"/adm_legacy"), full.names = TRUE, recursive = TRUE))

Keep.List<-c("Keep.List",ls())

legacy_list <- list.files(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Output/"), full.names = TRUE, recursive = TRUE)
legacy_list <- legacy_list[grepl(paste0(1996:2010,collapse ="|"),legacy_list)]
legacy_list <- legacy_list[grepl("base_rate|coverage_level_differential|insurance_dates|area_index|area_rate",legacy_list)]

legacy_price_list <- list.files(paste0(farmpolicylab,"rmaFCIPdata/rmaPrices/Archive/addendum/"), full.names = TRUE, recursive = TRUE)

lapply(
  c(legacy_list,legacy_price_list),
  function(y){
    df <- readRDS(y)
    df <- standardize_fcip_column_names(df)
    saveRDS(as.data.table(df),file=file.path(dir_data_release,"adm_legacy",basename(y)))
    invisible()
  })


# # Verify auth first (nice sanity check)
# if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)
# 
# # 1) Delete the release if it exists (ignore 404s)
# tryCatch({
#   piggyback::pb_release_delete(repo = "ftsiboe/USFarmSafetyNetLab", tag = "adm_legacy")
# }, error = function(e) NULL)
# 
# # 2) Create the release (once)
# piggyback::pb_release_create(
#   repo = "ftsiboe/USFarmSafetyNetLab",
#   tag  = "adm_legacy",
#   name = "Actuarial Data Master - Legacy",
#   body = "The FCIP's Actuarial Data Master from 1996 to 2010"
# )

# 3) Upload the assets
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/adm_legacy"), full.names = TRUE, recursive = TRUE),
  repo  = "ftsiboe/USFarmSafetyNetLab",
  tag   = "adm_legacy",
  overwrite = TRUE
)
