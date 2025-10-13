
rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

build_internal_datasets(
  source_files = c(
    list.files(paste0(dir_data_release,"/fsa") ,full.names = T,pattern = ".rds$"),
    list.files(paste0(dir_data_release,"/ice") ,full.names = T,pattern = ".rds$"),
    list.files(paste0(dir_data_release,"/nass"),full.names = T,pattern = ".rds$"),
    list.files(paste0(dir_data_release,"/fsa") ,full.names = T,pattern = ".rds$"),
    list.files(paste0(dir_data_release,"/adm") ,full.names = T,pattern = ".rds$"))
  , size_threshold = 2 )

