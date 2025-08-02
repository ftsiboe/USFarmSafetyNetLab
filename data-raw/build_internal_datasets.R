#---------------------------------------------------------
# Preliminaries                                        ####
# load package
rm(list=ls(all=TRUE));gc()
devtools::document()
devtools::load_all()

current_year <- as.numeric(format(Sys.Date(),"%Y"))-2

dir_fastscratch <- "./data-raw/fastscratch"

# Create target directory if needed
if (!dir.exists(dir_fastscratch)) {
  dir.create(dir_fastscratch, recursive = TRUE)
}
if (!dir.exists("./data-raw/internal_datasets")) {
  dir.create("./data-raw/internal_datasets", recursive = TRUE)
}

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
# Harmonize and summarize crop type                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
type_recodes <- harmonize_crop_type_codes()
saveRDS(type_recodes,file="./data-raw/internal_datasets/rma_type_recodes.rds");gc()
#---------------------------------------------------------
# INDEX FOR PRICE RECEIVED, 2011                       ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- process_nass_dataset(
  dir_source  = paste0(dir_fastscratch,"/nass"),
  large_dataset = "economics",
  nassqs_params = list(short_desc = "COMMODITY TOTALS - INDEX FOR PRICE RECEIVED, 2011",
                       freq_desc = "ANNUAL"))
df <- df[freq_desc %in% "ANNUAL",
         .(index_for_price_recived = mean(value, na.rm = TRUE)),
         by = c("commodity_year")]
df[, data_source := "USDA NASS Quick Stats"]
saveRDS(df,file="./data-raw/internal_datasets/index_for_price_recived.rds");rm(df);gc()
#---------------------------------------------------------
# Get Marketing Year Average Price                     ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <-  get_marketing_year_avg_price(
  dir_source = paste0(dir_fastscratch,"/nass"),
  agg_level_desc = c("NATIONAL","STATE"),
  short_desc = c(
    "OATS - PRICE RECEIVED, MEASURED IN $ / BU",
    "RYE - PRICE RECEIVED, MEASURED IN $ / BU",
    "TOBACCO - PRICE RECEIVED, MEASURED IN $ / LB",
    "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
    "FLAXSEED - PRICE RECEIVED, MEASURED IN $ / BU",
    "BARLEY - PRICE RECEIVED, MEASURED IN $ / BU",
    "BEANS, DRY EDIBLE, INCL CHICKPEAS - PRICE RECEIVED, MEASURED IN $ / CWT",
    "HAY - PRICE RECEIVED, MEASURED IN $ / TON",
    "WHEAT - PRICE RECEIVED, MEASURED IN $ / BU",
    "COTTON - PRICE RECEIVED, MEASURED IN $ / LB",
    "SORGHUM, GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
    "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
    "SUGARBEETS - PRICE RECEIVED, MEASURED IN $ / TON",
    "PEAS, DRY EDIBLE - PRICE RECEIVED, MEASURED IN $ / CWT",
    "SUNFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT",
    "RICE - PRICE RECEIVED, MEASURED IN $ / CWT",
    "PEANUTS - PRICE RECEIVED, MEASURED IN $ / LB",
    "CANOLA - PRICE RECEIVED, MEASURED IN $ / CWT",
    "MAPLE SYRUP - PRICE RECEIVED, MEASURED IN $ / GALLON",
    "RICE, LONG GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
    "MILLET, PROSO - PRICE RECEIVED, MEASURED IN $ / BU",
    "SUGARCANE - PRICE RECEIVED, MEASURED IN $ / TON",
    "SAFFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT"))

df[, data_source := "USDA NASS Quick Stats"]
saveRDS(df,file="./data-raw/internal_datasets/marketing_year_avg_price.rds");rm(df);gc()

#---------------------------------------------------------
# State rental rates                                   ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_state_rental_rates()
df[, data_source := "Output from get_state_rental_rates() function"]
saveRDS(df,file = "./data-raw/internal_datasets/state_rental_rates.rds")
#---------------------------------------------------------
# Program indicator                                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "IceProgramIndicator")
df <- unique(df[, c("reinsurance_year","program_indicator_code","program_indicator_description"), with = FALSE])
df <- df[complete.cases(df)]
table(df$program_indicator_code,df$reinsurance_year)
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file="./data-raw/internal_datasets/ice_program_indicator_code.rds");rm(df);gc()
#---------------------------------------------------------
# Administrative Fee Waiver Code                       ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "IceAdministrativeFeeWaiver")
df <- unique(df[, c("reinsurance_year","administrative_fee_waiver_code","administrative_fee_waiver_description"), with = FALSE])
df <- df[complete.cases(df)]
table(df$administrative_fee_waiver_code,df$reinsurance_year)
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file="./data-raw/internal_datasets/ice_administrative_fee_waiver_code.rds");rm(df);gc()
#---------------------------------------------------------
# Policy History Request Code                          ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "IceHistoryRequest")
df <- unique(df[, c("reinsurance_year","producer_policy_history_request_code","policy_producer_history_request_code_description"), with = FALSE])
df <- df[complete.cases(df)]
table(df$producer_policy_history_request_code,df$reinsurance_year)
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file="./data-raw/internal_datasets/ice_policy_history_request_code.rds");rm(df);gc()
#---------------------------------------------------------
# Yield Type Code                                      ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "D00042_IceYieldType")
df <- unique(df[, c("reinsurance_year","yield_type_code","valid_pty_non_summary_flag","yield_type_description"), with = FALSE])
df <- df[complete.cases(df)]
table(df$yield_type_code,df$reinsurance_year)
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file="./data-raw/internal_datasets/ice_yield_type_code.rds");rm(df);gc()
#---------------------------------------------------------
# Build the helper data sets                           ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
build_internal_datasets(dir_source = "./data-raw/internal_datasets", size_threshold = 1)
#---------------------------------------------------------
