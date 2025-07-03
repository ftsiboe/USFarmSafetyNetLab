# ------------------------------------------------------------------------------
# Prepared By Francis Tsiboe (ftsiboe@hotmail.com) 
# Citation requirement;
# (1) Tsiboe, F., & Turner, D. (2023). Econometric identification of crop insurance participation. 
# Agricultural and Resource Economics Review, 52(3), 476-497.  https://doi.org/10.1017/age.2023.13
# 
# (2) Tsiboe, F., & Turner, D. (2023). The crop insurance demand response to premium subsidies: 
#     Evidence from US Agriculture. Food Policy, 119, 102505. https://doi.org/10.1016/j.foodpol.2023.102505
# ------------------------------------------------------------------------------
# Purpose:
#   This R script automates the retrieval, processing, and aggregation of
#   USDA Risk Management Agency (RMA) crop insurance summary-of-business data
#   from multiple sources (app API and historical releases). It computes key
#   metrics (insured area, liability, indemnity, and loss cost ratio), and
#   replicates the key instrumental variables for crop insurance demand as
#   discussed in Tsiboe & Turner (2023) by combining: 
#   (1) estimated base rates by mimicking contemporary RMA methods allied to historic data, 
#   (2) Averages of actual actuarial data master rates, and 
#   (3) the national subsidy-rate instrument from Yu et al. (2018). 
#   The resulting instrument dataset (excluding the current year) is saved as an RDS
#   for downstream econometric identification of crop insurance participation.
# ------------------------------------------------------------------------------

rm(list=ls(all=TRUE)); gc(); library(data.table); library(magrittr)
devtools::document()
devtools::load_all()

dir_USFarmSafetyNetLab <- getwd()

# Change working directory based on OS:
setwd(
  ifelse(
    Sys.info()['sysname'] == "Windows",
    "C:/GitHub/US-FarmSafetyNet-Lab/replications/crop_insurance_instruments",
    paste0("/homes/", Sys.info()['user'], 
           "/GitHub/US-FarmSafetyNet-Lab/replications/crop_insurance_instruments/")
  )
)

base_url <- "https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download"
version  <- "v0.1.0"

current_year <- as.numeric(format(Sys.Date(),"%Y")) - 2

# Directory to store cached calibrations
dir_data_release <- paste0(dir_USFarmSafetyNetLab,"/data-raw/data_release")

# Create the cache directory if it does not already exist
if (!dir.exists(dir_data_release)) {
  dir.create(dir_data_release, recursive = TRUE)
}

# Download and process summary of business from RMA's app
sobapp <- as.data.table(rfcip::get_sob_data(year = 1990:current_year, group_by = c("county","crop")))
sobapp[, state_abbreviation := state_abbrv]
sobapp <- sobapp[, .(
  insured_area     = sum(ifelse(grepl("Acre", quantity_type), quantity, 0), na.rm = TRUE),
  liability_amount = sum(liabilities, na.rm = TRUE),
  indemnity_amount = sum(indemnity, na.rm = TRUE)), 
  by = c("commodity_year","state_code","state_abbreviation",
         "county_code","county_name","commodity_code","commodity_name")]

# Download and process Historical summary of business by state, county, and crop
sobscc <- tempfile(fileext = ".rds")
download.file(
  paste(base_url, version,
        "historical_summary_of_business_by_state_county_crop.rds",
        sep = "/"),
  sobscc, mode = "wb", quiet = TRUE)
sobscc <- as.data.table(readRDS(sobscc))
sobscc <- sobscc[, .(
  insured_area     = sum(net_reporting_level_amount, na.rm = TRUE),
  liability_amount = sum(liability_amount, na.rm = TRUE),
  indemnity_amount = sum(indemnity_amount, na.rm = TRUE)), 
  by = c("commodity_year","state_code","state_abbreviation",
  "county_code","county_name","commodity_code","commodity_name")]

# Combine datasets
sob <- rbind(
  sobapp[commodity_year >= 1990],
  sobscc[commodity_year <= 1989])
rm(sobapp, sobscc); gc()

# Aggregate over the new dataset without specific crop codes
sob_all_crops <- sob[, .(
  insured_area     = sum(insured_area, na.rm = TRUE),
  liability_amount = sum(liability_amount, na.rm = TRUE),
  indemnity_amount = sum(indemnity_amount, na.rm = TRUE),
  commodity_code   = 0,
  commodity_name   = "All crops"), 
  by = c("commodity_year","state_code","state_abbreviation","county_code","county_name")]

# Append datasets, excluding the generic crop code from the combined dataset
sob <- rbind(
  sob_all_crops,
  sob[!county_code %in% 9999])
rm(sob_all_crops); gc()

# Calculate Loss Cost Ratio (LCR) for risk assessment
sob[, lcr := indemnity_amount / liability_amount]

sob[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(sob))) := lapply(
  .SD, function(x) as.numeric(as.character(x))), 
  .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(sob))]

# Estimate FCIP Instrumental Variables (Unloaded Rates)
instruments <- as.data.frame(
  data.table::rbindlist(
    lapply(
      c((min(sob[["commodity_year"]]) + 22):max(sob[["commodity_year"]])),
      estimate_fcip_instruments,
      statplan = sob
    ), fill = TRUE))

# merge Instrument (i.e., target rate) aggregated directly from RMA’s actuarial data master 
adm <- tempfile(fileext = ".rds")
download.file(
  paste(base_url, version,
        "instrumental_variables_fcip_demand_from_actuarial_data_master.rds",
        sep = "/"),
  adm, mode = "wb", quiet = TRUE)
adm <- as.data.table(readRDS(adm))

instruments <- merge( instruments,adm, by= intersect(names(instruments), names(adm)), all  = TRUE)
instruments <- as.data.table(instruments)
# formulate and merge national subsidy rate instrument as described by (Yu et al., 2018)
instrument_yu2018 <- get_yu2018_instrument()

instruments <- merge(instruments,instrument_yu2018,
  by  = intersect(names(instruments), names(instrument_yu2018)),
  all = TRUE)
instruments <- as.data.table(instruments)
# tau_final: Same as tau_adm with missing data filled in with tau_sob (as is). 
instruments[, tau_final := tau_adm] 
instruments[is.na(tau_adm) | !is.finite(tau_adm) | tau_adm == 0, tau_final := tau_sob]

instruments <- merge(
 unique( sob[
   , .SD, .SDcols = 
     c("commodity_year","state_abbreviation","state_code","county_name","county_code",
        "commodity_name","commodity_code")]),instruments,
                     by  = c("commodity_year","state_code","county_code","commodity_code"),
                     all = TRUE)


instruments <- instruments[
  , .SD, .SDcols = 
    c( "commodity_year","state_abbreviation","state_code","county_name","county_code",
      "commodity_name","commodity_code","tau_sob","tau_adm","tau_final",
      "subsidy_rate_65","subsidy_rate_75")]

instruments <- instruments[!is.na(tau_final) & is.finite(tau_final) & tau_final != 0]

# Save the processed data to an RDS file for use
saveRDS(instruments, paste0(dir_data_release, "/estimated_instrumental_variables_fcip_demand.rds"))

