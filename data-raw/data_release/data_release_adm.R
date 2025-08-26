source("data-raw/work_environment_setup.R")

devtools::document()

# unlink(list.files(paste0(dir_data_release,"/adm"), full.names = TRUE, recursive = TRUE))

Keep.List<-c("Keep.List",ls())

# recodes type
rm(list= ls()[!(ls() %in% c(Keep.List))])
data <- harmonize_crop_type_codes()
data[, data_source := "Generated internally, using harmonize_crop_type_codes()"]
saveRDS(data,file=paste0(dir_data_release,"/adm/fcip_recodes_type.rds"))

# recodes practice
rm(list= ls()[!(ls() %in% c(Keep.List))])
source(paste0(dir_data_release,"/data_release_adm_recodes_practice.R"))
data[, data_source := "USDA-RMA, Actuarial Data Master - A00510 supplemented data from legacy ADM files"]
saveRDS(data,file=paste0(dir_data_release,"/adm/fcip_recodes_practice.rds"))

# recodes palns
rm(list= ls()[!(ls() %in% c(Keep.List))])
source(paste0(dir_data_release,"/data_release_adm_recodes_palns.R"))
data[, data_source := "USDA-RMA, Actuarial Data Master - A00460 supplemented data from legacy ADM files"]
saveRDS(data,file=paste0(dir_data_release,"/adm/fcip_recodes_insurance_plan.rds"))

# recodes commodity
rm(list= ls()[!(ls() %in% c(Keep.List))])
source(paste0(dir_data_release,"/data_release_adm_recodes_commodity.R"))
adm[, data_source := "USDA-RMA, Actuarial Data Master - A00400 and A00420 supplemented data from legacy ADM files"]
saveRDS(adm,file=paste0(dir_data_release,"/adm/fcip_recodes_commodity_groupings.rds"))

# commodity price
rm(list= ls()[!(ls() %in% c(Keep.List))])
source(paste0(dir_data_release,"/data_release_adm_commodity_price.R"))
price[, data_source := "USDA-RMA, Actuarial Data Master - A00810 supplemented data from legacy ADM files"]
saveRDS(price,file=paste0(dir_data_release,"/adm/fcip_commodity_price.rds"))

# base rate
rm(list= ls()[!(ls() %in% c(Keep.List))])
source(paste0(dir_data_release,"/data_release_adm_base_rate.R"))
baserate[, data_source := "USDA-RMA, Actuarial Data Master supplemented data from legacy ADM files"]
saveRDS(baserate,file=paste0(dir_data_release,"/adm/fcip_aph_base_rate.rds"))

# Contiguous county 
rm(list= ls()[!(ls() %in% c(Keep.List))])
contiguous_county <- rmaADM:::clean_data(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/2025/2025_A01230_ContiguousCounty_YTD.rds")))
contiguous_county <- as.data.table(contiguous_county)
contiguous_county[, data_source := "USDA-RMA, Actuarial Data Master - A0123"]
saveRDS(contiguous_county,file=paste0(dir_data_release,"/adm/fcip_contiguous_county.rds"))


# # Send Actuarial Data Master to Github    
# tryCatch({
#   piggyback::pb_release_delete(repo = "ftsiboe/USFarmSafetyNetLab", tag = "adm")
# }, error = function(e){NULL})
# 
# piggyback::pb_release_create(
#   repo = "ftsiboe/USFarmSafetyNetLab",
#   tag = "adm",
#   name = "Actuarial Data Master",
#   body = "Various items aggregated from the FCIP's Actuarial Data Master")
# 
# piggyback::pb_new_release( repo = "ftsiboe/USFarmSafetyNetLab", tag  = "adm")

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/adm"), full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "adm",overwrite = TRUE)


