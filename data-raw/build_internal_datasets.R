#---------------------------------------------------------
# Preliminaries                                        ####
# load package
rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table)
library(dplyr)
source("R/constants.R")
source("R/build_internal_datasets.R")
#source("R/prep_fcip_data.R")
source("R/helper_nass.R")
source("R/get_ice_data.R")
source("R/copied_from_rmaADM.R")
current_year <- as.numeric(format(Sys.Date(),"%Y"))-2

dir_fastscratch <- "./data-raw/fastscratch"

# Create target directory if needed
if (!dir.exists(dir_fastscratch)) {
  dir.create(dir_fastscratch, recursive = TRUE)
}
if(!dir.exists("./data-raw/internal_datasets")) {
  dir.create("./data-raw/internal_datasets", recursive = TRUE)
}

unlink(list.files("./data-raw/internal_datasets",full.names = T))

if(Sys.info()['sysname'] %in% "Windows"){
  farmpolicylab <- paste0(gsub("OneDrive","Dropbox",Sys.getenv("OneDriveConsumer")),"/farmpolicylab/database/")
}else{
  farmpolicylab <- paste0("~/Database/USA/USDA/")
}
Keep.List<-c("Keep.List",ls())
#---------------------------------------------------------
# Contiguous county                                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
contiguous_county <- rmaADM:::clean_data(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/2025/2025_A01230_ContiguousCounty_YTD.rds")))
contiguous_county <- as.data.table(contiguous_county)
contiguous_county[, data_source := "USDA-RMA, Actuarial Data Master - A0123"]
saveRDS(contiguous_county,file="./data-raw/internal_datasets/contiguous_county.rds");rm(contiguous_county);gc()
#---------------------------------------------------------
# Build the helper data sets                           ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
build_internal_datasets(dir_source = "./data-raw/internal_datasets", size_threshold = 1)
#---------------------------------------------------------
