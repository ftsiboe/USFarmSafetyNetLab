
library(rfsa);library(data.table);library(dplyr)
source("data-raw/scripts/repo_workflow/environment_setup.R")

# unlink(list.files(paste0(dir_data_release,"/fsa"), full.names = TRUE, recursive = TRUE))

devtools::document()

Keep.List<-c("Keep.List",ls())

# crop linker
rm(list= ls()[!(ls() %in% c(Keep.List))])
source("data-raw/scripts/repo_workflow/data_release/data_release_fsa_crop_linker.R")
linker[, data_source := "Internal innovation"]
saveRDS(linker,file=paste0(dir_data_release,"/fsa/fsa_crop_linker.rds"))

# Upload the assets
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/fsa_acreage"), full.names = TRUE, recursive = TRUE, pattern = "fsaCropAcreageData"),
  repo  = "ftsiboe/USFarmSafetyNetLab",
  tag   = "fsa_extracts",
  overwrite = TRUE
)

