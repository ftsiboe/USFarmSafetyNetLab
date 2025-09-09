
source("data-raw/scripts/environment_setup.R")

devtools::document()

price_discovery <- data.table::rbindlist(
  lapply(
    list.files(paste0(farmpolicylab,"rmaFCIPdata/rmaPrices/Output/discovery/"),full.names = T),
    function(i){
      return(readRDS(i))
    }), fill = TRUE)[
      , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
      by = c("crop_yr", "crop_cd", "state_cd", "typ_cd"),
      .SDcols = c("Proj_mean","Harv_mean")]

setnames(price_discovery,
         old = c("crop_yr", "crop_cd", "state_cd", "typ_cd","Proj_mean","Harv_mean"),
         new = c("commodity_year", "commodity_code", "state_code", "type_code",
                 "projected_price_discovery","harvest_price_discovery"))

price_legacy <- data.table::rbindlist(
  lapply(
    1999:2010,
    function(y){
      return(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaPrices/Output/rmaADMprices/rmaADMprices_",y,".rds")))
    }), fill = TRUE)[
  , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
  by = c("crop_yr", "crop_cd", "state_cd", "county_cd", "typ_cd", "pract_cd"),
  .SDcols = c("Price_Prj","Price_Hrvst")]

setnames(price_legacy,
         old = c("crop_yr", "crop_cd", "state_cd", "county_cd", "typ_cd", "pract_cd","Price_Prj","Price_Hrvst"),
         new = c("commodity_year", "commodity_code", "state_code", "county_code", "type_code", "practice_code",
                 "projected_price","harvest_price"))

price <-data.table::rbindlist(
  lapply(
    2011:as.numeric(format(Sys.Date(),"%Y")),
    function(y){
      get_adm_data(year = y, dataset="A00810_Price")[
        , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
        by = c("commodity_year",FCIP_INSURANCE_POOL),
        .SDcols = c("price_volatility_factor","projected_price","harvest_price","catastrophic_price")]
    }), fill = TRUE)


price <- data.table::rbindlist(list(price,price_legacy,price_discovery), fill = TRUE)

price <- price[
      , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
      by = c("commodity_year",FCIP_INSURANCE_POOL),
      .SDcols = c("price_volatility_factor","projected_price","harvest_price","catastrophic_price",
                  "projected_price_discovery","harvest_price_discovery")]

price[harvest_price %in% c(NA,Inf,-Inf,0,NaN), harvest_price := harvest_price_discovery]
price[projected_price %in% c(NA,Inf,-Inf,0,NaN), projected_price := projected_price_discovery]

price <- price[
  ! (projected_price %in% c(NA,Inf,-Inf,0,NaN) &
       harvest_price %in% c(NA,Inf,-Inf,0,NaN) &
       catastrophic_price %in% c(NA,Inf,-Inf,0,NaN))]

price <- as.data.table(price)



