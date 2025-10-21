
rm(list = ls(all = TRUE))

source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

# Verify auth first (nice sanity check)/create all release once
# source("data-raw/scripts/repo_workflow/data_release/create_all_release_once.R")

# RMA-SOB
source("data-raw/scripts/repo_workflow/data_release/data_release_sob.R")

# RMA-COL
source("data-raw/scripts/repo_workflow/data_release/data_release_col.R")

# RMA-ADM
source("data-raw/scripts/repo_workflow/data_release/data_release_adm.R")
source("data-raw/scripts/repo_workflow/data_release/data_release_adm_legacy_1996_2010.R")

# RMA-ICE
source("data-raw/scripts/repo_workflow/data_release/data_release_ice.R")

# NASS Data
source("data-raw/scripts/repo_workflow/data_release/data_release_nass.R")

# spatial features
source("data-raw/scripts/repo_workflow/data_release/data_release_spatial_features.R")

# Replications
source("data-raw/scripts/repo_workflow/data_release/data_release_replications_release.R")

# # Replications
# source("data-raw/scripts/repo_workflow/data_release/data_release_fsa.R")



