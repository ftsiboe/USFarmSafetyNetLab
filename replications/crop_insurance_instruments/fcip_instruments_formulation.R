
# Prepared By Francis Tsiboe (ftsiboe@hotmail.com)  
# Citation requirement;
# 1. Tsiboe,F. and Turner, D., 2023. Econometric identification of crop insurance participation. Agricultural and Resource Economics Review, 52(3):476-497. https://doi.org/10.1017/age.2023.13
# 2. Tsiboe,F. and Turner, D., 2023. The crop insurance demand response to premium subsidies: Evidence from US Agriculture. Food Policy, 119. https://doi.org/10.1016/j.foodpol.2023.102505

rm(list=ls(all=TRUE));gc();library(data.table);library(magrittr)
devtools::document()
devtools::load_all()

base_url <- "https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download"
version  <- "v0.1.0"

current_year <- as.numeric(format(Sys.Date(),"%Y"))-2

# Directory to store cached calibrations
dir_data_release <- "data-raw/data_release"

# Create the cache directory if it does not already exist
if (!dir.exists(dir_data_release)) {
  dir.create(dir_data_release, recursive = TRUE)
}

# Download and process summary of business from RMA's app
sobapp <- as.data.table(rfcip::get_sob_data(year = 1990:current_year, group_by = c("county","crop")))
sobapp[,state_abbreviation := state_abbrv]
sobapp <- sobapp[, .(insured_area = sum(ifelse(grepl("Acre", quantity_type), quantity, 0), na.rm = TRUE),
                     liability_amount = sum(liabilities, na.rm = TRUE),
                     indemnity_amount = sum(indemnity, na.rm = TRUE)),
                 by = c("commodity_year","state_code","state_abbreviation","county_code",
                        "county_name","commodity_code","commodity_name")]

# Download and process Historical summary of business by state, county, crop, and coverage
sobcov <- tempfile(fileext = ".rds")
download.file(
  paste(base_url, version,
         "historical_summary_of_business_by_state_county_crop_coverage.rds", sep = "/"),
  sobcov, mode = "wb",quiet=TRUE)
sobcov <- as.data.table(readRDS(sobcov))
sobcov <- sobcov[, .(insured_area = sum(ifelse(grepl("ACRE", reporting_level_type), net_reporting_level_amount, 0), na.rm = TRUE),
                     liability_amount = sum(liability_amount, na.rm = TRUE),
                     indemnity_amount = sum(indemnity_amount, na.rm = TRUE)),
                 by = c("commodity_year","state_code","state_abbreviation","county_code",
                        "county_name","commodity_code","commodity_name")]

# Download and process Historical summary of business by state, county, and crop
sobscc <- tempfile(fileext = ".rds")
download.file(
  paste(base_url, version,
        "historical_summary_of_business_by_state_county_crop.rds", sep = "/"),
  sobscc, mode = "wb",quiet=TRUE)
sobscc <- as.data.table(readRDS(sobscc))
sobscc <- sobscc[, .(insured_area = sum(net_reporting_level_amount, na.rm = TRUE),
                     liability_amount = sum(liability_amount, na.rm = TRUE),
                     indemnity_amount = sum(indemnity_amount, na.rm = TRUE)),
                 by = c("commodity_year","state_code","state_abbreviation","county_code",
                        "county_name","commodity_code","commodity_name")]

# Combine datasets
sob <- rbind(sobapp[commodity_year >= 1990],sobscc[1989:1991], sobcov[commodity_year <= 1988])
rm(sobapp, sobscc, sobcov);gc()

# Aggregate over the new dataset without specific crop codes
sob_all_crops <- sob[, .(insured_area = sum(insured_area, na.rm = TRUE),
                         liability_amount = sum(liability_amount, na.rm = TRUE),
                         indemnity_amount = sum(indemnity_amount, na.rm = TRUE),
                         commodity_code = 0, commodity_name="All crops"),
                   by = c("commodity_year","state_code","state_abbreviation","county_code","county_name")]

# Append datasets, excluding the generic crop code from the combined dataset
sob <- rbind(sob_all_crops, sob[!county_code %in% 9999])
rm(sob_all_crops);gc()

# Calculate Loss Cost Ratio (LCR) for risk assessment
sob[, lcr := indemnity_amount/liability_amount]

# saveRDS(sob,file="data-raw/sob.rds")
# sob <- readRDS("data-raw/sob.rds")

sob <- as.data.table(sob)
sob[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(sob))) := lapply(
  .SD, function(x) as.numeric(as.character(x))
), .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(sob))]

# Estimate FCIP Instrumental Variables (Unloaded Rates)
# adm <- estimate_fcip_instruments(year = 2011, statplan=sob)
instruments <- as.data.frame(
  data.table::rbindlist(
    lapply(
      (min(sob[["commodity_year"]])+22):max(sob[["commodity_year"]])[1:2],
      estimate_fcip_instruments,
      statplan=sob), fill = TRUE))

# merge Instrument (i.e., target rate) aggregated directly from RMA’s actuarial data master 
adm <- tempfile(fileext = ".rds")
download.file(
  paste(base_url, version,
        "instrumental_variables_fcip_demand_from_actuarial_data_master.rds", sep = "/"),
  adm, mode = "wb",quiet=TRUE)
adm <- as.data.table(readRDS(adm))

instruments <- dplyr::full_join(instruments,adm, by=names(instruments)[names(instruments) %in% names(adm)])

# formulate and merge national subsidy rate instrument as described by (Yu et al., 2018)
instrument_yu2018 <- get_yu2018_instrument()

instruments <- dplyr::full_join(instruments,instrument_yu2018, by=names(instruments)[names(instruments) %in% names(instrument_yu2018)])












# tau_final: Same as tau_adm with missing data filled in with tau_sob (as is). 
instruments$tau_final <- ifelse(instruments$tau_adm %in% c(NA,Inf,-Inf,NaN,0),instruments$tau_sob,instruments$tau_adm)

instruments <- instruments[c("crop_yr","state_ab","state_cd","county","county_cd","crop","crop_cd",
                             "tau_sob","tau_adm","tau_final","subsidy_rate_65","subsidy_rate_75")]
instruments <- instruments[!instruments$tau_final %in% c(NA,Inf,-Inf,NaN,0),]

# Save the processed data to an RDS file for use
instruments <- instruments[!instruments$crop_yr %in% as.numeric(format(Sys.Date(),"%Y")),]
saveRDS(dir_data_release, "/instrumental_variables_fcip_demand.rds")

