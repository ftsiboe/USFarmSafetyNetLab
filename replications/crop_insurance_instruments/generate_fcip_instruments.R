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
#   from multiple sources. It computes key metrics (insured area, liability, indemnity,
#   and loss cost ratio), and replicates the key instrumental variables for 
#   crop insurance demand as discussed in Tsiboe & Turner (2023) by combining: 
#   (1) estimated base rates by mimicking contemporary RMA methods allied to historic data, 
#   (2) Averages of actual actuarial data master rates, and 
#   (3) the national subsidy-rate instrument from Yu et al. (2018). 
#   The resulting instrument dataset (excluding the current year) is saved as an RDS
#   for downstream econometric identification of crop insurance participation.
# ------------------------------------------------------------------------------

rm(list=ls(all=TRUE)); gc(); library(data.table); library(magrittr)
devtools::document()
devtools::load_all()

current_year <- as.numeric(format(Sys.Date(),"%Y")) - 2

# Download and process Historical summary of business
sobcov_full <- data.table::rbindlist(
  lapply(
    1989:current_year,
    function(year){
      tryCatch({
        sobcov <- tempfile(fileext = ".rds")
        download.file(
          paste0("https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/sob/sobcov_",year,".rds"),
          sobcov, mode = "wb", quiet = TRUE)
        sobcov <- readRDS(sobcov)
        # sobcov <- readRDS(paste0("./data-raw/data_release/sobcov_",year,".rds"))
        return(sobcov)
      }, error = function(e){return(NULL)})
    }), fill = TRUE)
dt <- as.data.table(sobcov_full)
sobcov <- sobcov_full[, .(
  insured_area     = sum(net_reported_quantity, na.rm = TRUE),
  liability_amount = sum(liability_amount, na.rm = TRUE),
  indemnity_amount = sum(indemnity_amount, na.rm = TRUE)), 
  by = c("commodity_year","state_code","state_abbreviation",
  "county_code","county_name","commodity_code","commodity_name")]

sobscc <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/sob/sobscc_1948_1988.rds",
  sobscc, mode = "wb", quiet = TRUE)
  sobscc <- readRDS(sobscc)
# sobscc <- readRDS(paste0("./data-raw/data_release/sobscc_1948_1988.rds"))
sobscc <- sobscc[, .(
  insured_area     = sum(net_reported_quantity, na.rm = TRUE),
  liability_amount = sum(liability_amount, na.rm = TRUE),
  indemnity_amount = sum(indemnity_amount, na.rm = TRUE)), 
  by = c("commodity_year","state_code","state_abbreviation",
         "county_code","county_name","commodity_code","commodity_name")]

sobscc[, .(
  insured_area     = sum(insured_area, na.rm = TRUE),
  liability_amount = sum(liability_amount, na.rm = TRUE),
  indemnity_amount = sum(indemnity_amount, na.rm = TRUE)), 
  by = c("commodity_year")]

sobcov[, .(
  insured_area     = sum(insured_area, na.rm = TRUE),
  liability_amount = sum(liability_amount, na.rm = TRUE),
  indemnity_amount = sum(indemnity_amount, na.rm = TRUE)), 
  by = c("commodity_year")]

# Combine datasets
sob <- rbind(
  sobcov[commodity_year >= 1989],
  sobscc[commodity_year <= 1988])
rm(sobcov, sobscc); gc()

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
  "https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/adm/fcip_demand_instruments_from_adm.rds",
  adm, mode = "wb", quiet = TRUE)
adm <- readRDS(adm)
# adm <- readRDS(paste0("./data-raw/data_release/fcip_demand_instruments_from_adm.rds"))
adm <- as.data.table(adm)

instruments <- merge( instruments,adm, by= intersect(names(instruments), names(adm)), all  = TRUE)
instruments <- as.data.table(instruments)

# formulate and merge national subsidy rate instrument as described by (Yu et al., 2018)
instrument_yu2018 <- get_yu2018_instrument(sobcov_full)

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

instruments[, data_source := "Key instrumental variables for crop insurance demand as discussed in Tsiboe & Turner (2023)"]
saveRDS(instruments, "./data-raw/data_release/fcip_demand_instruments.rds")

