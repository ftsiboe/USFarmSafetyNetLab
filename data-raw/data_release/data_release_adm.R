source("data-raw/work_environment_setup.R")

devtools::document()

# recodes palns
source(paste0(dir_data_release,"/data_release_adm_recodes_palns.R"))

# base rate
baserate <-data.table::rbindlist(
  lapply(
    2011:as.numeric(format(Sys.Date(),"%Y")),
    function(y){
      baserate <- get_adm_data(year = y, dataset = "baserate")
      baserate$tau_adm <- baserate$reference_rate + baserate$fixed_rate
      baserate <- doBy::summaryBy(tau_adm~commodity_year + state_code + county_code + commodity_code,data=baserate,FUN=mean,na.rm=T,keep.names = T)
      return(baserate)
    }), fill = TRUE)

baserate_legacy <- data.table::rbindlist(
  lapply(
    2001:2010,
    function(y){
      adm <- readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Output/base_rate/base_rate_",y,".rds"))
      adm$tau_adm <- adm$Rr + adm$Rf
      adm <- doBy::summaryBy(tau_adm~crop_yr + state_cd + county_cd + crop_cd,data=adm,FUN=mean,na.rm=T,keep.names = T)
      return(adm)
    }), fill = TRUE)
baserate_legacy <- standardize_fcip_column_names(baserate_legacy)

baserate <- rbind(baserate_legacy,baserate)
baserate <- baserate[complete.cases(baserate)]
saveRDS(baserate,file=paste0(dir_data_release,"/adm/fcip_demand_instruments_from_adm.rds"))

# price <- readRDS(file.path(farmpolicylab,
#                            "rmaFCIPdata", "rmaPrices",
#                            "Output", "ersFcipPrices.rds"))
# price$projecetd_price <- price$Price_Prj
# price$harvest_price <- price$Price_Hrvst
# price <- doBy::summaryBy(list(c("projecetd_price", "harvest_price"),c("state_cd","county_cd","typ_cd","crop_yr","crop_cd")),
#                          data=price,FUN=mean,na.rm=T,keep.names = T)
# price <- standardize_fcip_column_names(price)
# 
# saveRDS(price,file=paste0(dir_dest,"/adm/fcip_commodity_prices.rds"))

# Send Actuarial Data Master to Github    
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


