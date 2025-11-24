#----------------------------------------------------
# Initialize environment                          ####
rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

source("data-raw/scripts/repo_workflow/environment_setup.R")

# unlink(list.files(paste0(dir_data_release,"/nass"), full.names = TRUE, recursive = TRUE))

devtools::document()

# source("R/helper_nass.R")


census_file  <- list.files(paste0(dir_fastscratch,"/nass"),pattern = "qs.census",full.names = T)[5]

data <-  as.data.frame(data.table::fread(census_file))
data <- data[data$DOMAIN_DESC %in% c("TOTAL"),]
data <- data[data$AGG_LEVEL_DESC %in% c("COUNTY"),]
data <- data[data$DOMAINCAT_DESC %in% c("NOT SPECIFIED"),]
SHORT_DESC_list <- unique(data$SHORT_DESC)

SHORT_DESC_list[grepl("FERT",SHORT_DESC_list)]
SHORT_DESC_list[grepl("PRODUCTION, MEASURED IN",SHORT_DESC_list)]

"CORN, SILAGE - PRODUCTION, MEASURED IN TONS"
"CORN, SILAGE - ACRES HARVESTED"

"CORN, GRAIN - PRODUCTION, MEASURED IN BU" 
"CORN, GRAIN - ACRES HARVESTED"
"CORN - SALES, MEASURED IN $"
















