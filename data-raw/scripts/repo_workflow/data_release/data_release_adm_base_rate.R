
source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

baserate <-data.table::rbindlist(
  lapply(
    2011:as.numeric(format(Sys.Date(),"%Y")),
    function(y){
      baserate <- get_adm_data(year = y, dataset = "baserate", force=TRUE)
      baserate$tau_adm <- baserate$reference_rate + baserate$fixed_rate
      baserate <- doBy::summaryBy(tau_adm~commodity_year + state_code + county_code + commodity_code,data=baserate,FUN=mean,na.rm=T,keep.names = T)
      
      baserate[, c(intersect(FCIP_INSURANCE_POOL, names(baserate))) := lapply(
        .SD, function(x) as.numeric(as.character(x))
      ), .SDcols = intersect(FCIP_INSURANCE_POOL, names(baserate))]
      
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
baserate_legacy <- as.data.table(baserate_legacy)
baserate_legacy[, c(intersect(FCIP_INSURANCE_POOL, names(baserate_legacy))) := lapply(
  .SD, function(x) as.numeric(as.character(x))
), .SDcols = intersect(FCIP_INSURANCE_POOL, names(baserate_legacy))]

baserate <- as.data.table(rbind(baserate_legacy,baserate))
baserate <- baserate[complete.cases(baserate)]
