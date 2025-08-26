
rm(list=ls(all=TRUE));gc();library(rfsa);library(data.table);library(dplyr)
source("data-raw/work_environment_setup.R")

# unlink(list.files(paste0(dir_data_release,"/fsa"), full.names = TRUE, recursive = TRUE))

devtools::document()

Keep.List<-c("Keep.List",ls())

# fsa Crop Acreage             
rm(list= ls()[!(ls() %in% c(Keep.List))])




















