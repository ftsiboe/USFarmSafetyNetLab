
rm(list = ls(all = TRUE))

source("data-raw/scripts/environment_setup.R")

devtools::document()

# RMA-SOB
source(paste0(dir_data_release,"/scripts/data_release_sob.R"))

# RMA-COL
source(paste0(dir_data_release,"/scripts/data_release_col.R"))

# RMA-ADM
source(paste0(dir_data_release,"/scripts/data_release_adm.R"))

# RMA-ICE
source(paste0(dir_data_release,"/scripts/data_release_ice.R"))

# NASS Data
source(paste0(dir_data_release,"/scripts/data_release_nass.R"))


