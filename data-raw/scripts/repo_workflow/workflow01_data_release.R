
rm(list = ls(all = TRUE))

source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

# RMA-SOB
source("data-raw/scripts/repo_workflow/data_release/data_release_sob.R")

# RMA-COL
source("data-raw/scripts/repo_workflow/data_release/data_release_col.R")

# RMA-ADM
source("data-raw/scripts/repo_workflow/data_release/data_release_adm.R")

# RMA-ICE
source("data-raw/scripts/repo_workflow/data_release/data_release_ice.R")

# NASS Data
source("data-raw/scripts/repo_workflow/data_release/data_release_nass.R")


