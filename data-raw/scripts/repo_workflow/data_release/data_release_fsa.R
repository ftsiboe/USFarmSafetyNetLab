
library(rfsa);library(data.table);library(dplyr)
source("data-raw/scripts/environment_setup.R")

# unlink(list.files(paste0(dir_data_release,"/fsa"), full.names = TRUE, recursive = TRUE))

devtools::document()

Keep.List<-c("Keep.List",ls())

# Download database
source("data-raw/scripts/data_release_fsa_download_fsa_acreage_data.R")

# crop linker
rm(list= ls()[!(ls() %in% c(Keep.List))])
source("data-raw/scripts/data_release_fsa_crop_linker.R")
linker[, data_source := "Internal innovation"]
saveRDS(linker,file=paste0(dir_data_release,"/fsa/fsa_crop_linker.rds"))

