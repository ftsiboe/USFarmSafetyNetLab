
rm(list = ls(all = TRUE));gc();library(rfsa)

# Re-generate documentation for any R package functions in this project
devtools::document()

# Create directories for storing output (if they don’t already exist)
dir_estimations <- "./data-raw/fastscratch/reps/fcip_demand_response/output/estimations/"

if (!dir.exists(dir_estimations)) {
  dir.create(dir_estimations, recursive = TRUE)
}

replications_release <- "./data-raw/data_release/reps"

if (!dir.exists(replications_release)) {
  dir.create(replications_release, recursive = TRUE)
}




get_fcip_demand_estimation_data <- function(
    study_years = 2001:(as.numeric(format(Sys.Date(), "%Y"))-1)){
  
  # study_years <- 2001:(as.numeric(format(Sys.Date(), "%Y"))-1)
  
  # (1) RMA’s summary of business files and contains insurance metrics aggregated by 
  #    county, crop, crop type (e.g., corn can be grain or silage), production practice 
  #    (e.g., irrigation, organic, etc..), insurance plan (e.g., Actual Production History [APH], 
  #    Yield Protection [YP], Crop Revenue Coverage [CRC], Revenue Protection [RP], etc..), 
  #    coverage level, and insurance unit (Optional unit [OU], Enterprise unit [EU], etc..); 
  #    RMA refers to this data source as “Summary of Business” by “Type, Practice, Unit Structure” or “SOBTPU” for short.  
  #    The SOBTPU serves as the foundational data source for the study. 
  
  df <- tempfile(fileext = ".rds")
  download.file(
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobtpu_all.rds",
    df, mode = "wb", quiet = TRUE)
  df <- readRDS(df)
  df <- df[commodity_year %in% study_years]
  df <- df[coverage_type_code %in% "A"]
  df <- df[insurance_plan_abbreviation %in% c("APH", "YP", "CRC", "RP","RA","RPHPE", "IP")]
  df <- df[reporting_level_type %in% c("Acres")]
  df[,coverage_level_percent := ifelse(coverage_level_percent>1,coverage_level_percent/100,coverage_level_percent)]
  df[,potential_liability_amount := liability_amount/coverage_level_percent]
  
  df <- df[
    , lapply(.SD, sum), 
    by = c("commodity_year",FCIP_INSURANCE_POOL,"insurance_plan_code","unit_structure_code"), 
    .SDcols = c("net_reporting_level_amount","potential_liability_amount","liability_amount",
                "total_premium_amount","subsidy_amount")]
  df[,aggregate_coverage_level := liability_amount/potential_liability_amount]
  df[,premium_per_liability := total_premium_amount/liability_amount]
  df[,subsidy_per_premium := subsidy_amount/total_premium_amount]
  
  # (2) Characteristics
  df <- df[fcip_recodes_commodity_groupings[
    , c("commodity_year","commodity_code","commodity_name","CROP","commodity_group"), with = FALSE],
    on = c("commodity_year","commodity_code"), nomatch = 0]
  
  df <- df[fcip_recodes_practice[
    , c("commodity_year","commodity_code","practice_code", "irrigation_recode","organic_recode"), with = FALSE],
    on = c("commodity_year","commodity_code","practice_code"), nomatch = 0]
  
  df <- df[fcip_recodes_insurance_plan[
    , c("commodity_year","insurance_plan_code","insurance_plan_name","outcome_protected"), with = FALSE],
    on = c("commodity_year","insurance_plan_code"), nomatch = 0]
  
  # (3) FCIP commodity price
  adm_price <- tempfile(fileext = ".rds")
  download.file(
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_commodity_price.rds",
    adm_price, mode = "wb", quiet = TRUE)
  adm_price <- readRDS(adm_price)
  adm_price[,price := ifelse(projected_price %in% c(NA,0,Inf,-Inf,NaN),harvest_price,projected_price)]
  adm_price <- adm_price[, lapply(.SD, mean), by = intersect(names(df),names(adm_price)), .SDcols = c("price")]
  df <- merge(df,adm_price,by = intersect(names(df),names(adm_price)),all.x = TRUE)
  rm(adm_price);gc()
  
  # (4) FCIP demand instruments
  fcip_instruments <- tempfile(fileext = ".rds")
  download.file(
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/reps/fcip_demand_instruments.rds",
    fcip_instruments, mode = "wb", quiet = TRUE)
  fcip_instruments <- readRDS(fcip_instruments)
  fcip_instruments[,tau := tau_adm]
  fcip_instruments[tau %in% c(NA,Inf,-Inf,NaN,0),tau := tau_sob]
  
  df <- df[fcip_instruments[, c("commodity_year","state_code","county_code","commodity_code","tau","subsidy_rate_65","subsidy_rate_75"), with = FALSE], 
           on = c("commodity_year","state_code","county_code","commodity_code"), nomatch = 0]
  rm(fcip_instruments);gc()
  
  # (5) per-acre cost of crop production was approximated with state-level rental rates retrieved from NASS Quick Stats.
  df <- df[nass_state_rental_rates[, lapply(.SD, mean), by = intersect(names(df),names(nass_state_rental_rates)), .SDcols = c("rent")],
           on = intersect(names(df),names(nass_state_rental_rates)), nomatch = 0]
  
  # (6) Price index  
  df <- df[nass_index_for_price_recived[, lapply(.SD, mean), by = intersect(names(df),names(nass_index_for_price_recived)), .SDcols = c("index_for_price_recived")],
           on = intersect(names(df),names(nass_index_for_price_recived)), nomatch = 0]
  
  # (7) Acreage data from Farm Service Agency (FSA) with missing data filled in with planted, harvested, and baring acres 
  #     from USDA National Agricultural Statistics Service (NASS) Quick Stats, in that order
  data(fsaCropAcreage)
  fsaCropAcreage <- as.data.table(fsaCropAcreage)[
    , .(fsa_planted_acres = sum(planted_acres,    na.rm = TRUE)),
    by = .(crop_yr, state_cd, county_cd, rma_crop_code)]
  
  setnames(fsaCropAcreage,
           old = c("crop_yr", "state_cd", "county_cd", "rma_crop_code"),
           new = c("commodity_year", "state_code", "county_code", "commodity_code"))
  
  df <- merge(df,fsaCropAcreage,
              by = c("commodity_year", "state_code", "county_code", "commodity_code"),
              all.x = TRUE)
  rm(fsaCropAcreage);gc()
  
  
  nass_acreage_data <- tempfile(fileext = ".rds")
  download.file(
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/nass_extracts/nass_production_data.rds",
    nass_acreage_data, mode = "wb", quiet = TRUE)
  
  nass_acreage_data <- readRDS(nass_acreage_data)[
    agg_level_desc %in% "COUNTY" & statisticcat_desc %in% c("nassSurvey_AREA_HARVESTED","nassSurvey_AREA_PLANTED","nassSurvey_AREA_BEARING" )
    , .(value = sum(value, na.rm = TRUE)),
    by = .(commodity_year, state_code, county_code, commodity_name,statisticcat_desc)]
  
  nass_acreage_data <- nass_acreage_data |> tidyr::spread(statisticcat_desc, value)
  nass_acreage_data <- as.data.table(nass_acreage_data)
  
  df <- merge(df,nass_acreage_data,
              by = c("commodity_year", "state_code", "county_code", "commodity_name"),
              all.x = TRUE)
  rm(nass_acreage_data);gc()
  
  df[,county_acreage := fsa_planted_acres]
  df[county_acreage %in% c(NA,Inf,-Inf,NaN,0),county_acreage := nassSurvey_AREA_PLANTED]
  df[county_acreage %in% c(NA,Inf,-Inf,NaN,0),county_acreage := nassSurvey_AREA_BEARING]
  df[county_acreage %in% c(NA,Inf,-Inf,NaN,0),county_acreage := nassSurvey_AREA_HARVESTED]
  
  df <- df[, c("nassSurvey_AREA_HARVESTED","nassSurvey_AREA_PLANTED","nassSurvey_AREA_BEARING","fsa_planted_acres") := NULL]
  
  # Fill in price data
  df[price  %in% c(NA,0,Inf,-Inf,NaN), price := NA]
  df[price  %in% c(NA,0,Inf,-Inf,NaN), price := mean(price,na.rm=T), by = c("commodity_year","state_code","commodity_name","type_code","practice_code")]
  df[price  %in% c(NA,0,Inf,-Inf,NaN), price := mean(price,na.rm=T), by = c("commodity_year","state_code","commodity_name","type_code")]
  
  # Remove observations with missing or zero values for key variables
  df <- df[!log(aggregate_coverage_level) %in% c(0,NA,Inf,-Inf,NaN)]
  df <- df[!log(net_reporting_level_amount) %in% c(0,NA,Inf,-Inf,NaN)]
  df <- df[!log(subsidy_per_premium) %in% c(0,NA,Inf,-Inf,NaN)]
  df <- df[!log(premium_per_liability) %in% c(0,NA,Inf,-Inf,NaN)]
  df <- df[!log(tau) %in% c(0,NA,Inf,-Inf,NaN)]
  df <- df[!log(subsidy_rate_65) %in% c(0,NA,Inf,-Inf,NaN)]
  df <- df[!log(subsidy_rate_75) %in% c(0,NA,Inf,-Inf,NaN)]
  #df <- df[!log(county_acreage) %in% c(0,NA,Inf,-Inf,NaN)] # we are losing a lot of obs here

  # Keep only crops with at least 30 observations per year for at least 10 years
  croplist <- df[, .(n = length(net_reporting_level_amount)),by = .(commodity_year, commodity_name)][
    n>=30,.(n = length(n)),by = .(commodity_name)][n>=10]
  df <- df[commodity_name %in% croplist$commodity_name]
  rm(croplist);gc()
  
  # Create mundlak variables and pool variable
  mundlak <-  c("state_code","county_code","commodity_code","type_code","practice_code","insurance_plan_code","unit_structure_code")
  mundlak <- names(df)[names(df) %in% mundlak]
  mundlak <- df[, .(singleton = length(commodity_year) %in% 1),by = c(mundlak)]
  mundlak[,pool := .I]
  df <- df[mundlak,on = intersect(names(df),names(mundlak)), nomatch = 0]
  rm(mundlak);gc()
  
  df <- na.omit(df, cols = c("pool","commodity_year","net_reporting_level_amount","aggregate_coverage_level",
                             "subsidy_per_premium","premium_per_liability",
                             "price","tau","subsidy_rate_65","subsidy_rate_75"))
  
  # Create subsidy bins
  step <- 0.02
  lo   <- 0.40
  hi   <- 0.80
  df[, subsidy_bins := {
    x <- subsidy_per_premium
    # clamp to [lo, hi], NAs stay NA
    x <- pmin(pmax(x, lo), hi)
    # bin downward in step=0.02; tiny eps handles floating error on boundaries
    b <- lo + floor((x - lo + 1e-12) / step) * step
    # format as "SUB040", "SUB078", "SUB080"; NAs remain NA
    ifelse(is.na(b), NA_character_, sprintf("SUB%03d", round(b * 100)))
  }]
  
  df[, period_farmbill := fcase(
    commodity_year < 1980, 0L,
    commodity_year >= 1980 & commodity_year < 1994, 1L,
    commodity_year >= 1994 & commodity_year < 1996, 2L,
    commodity_year >= 1996 & commodity_year < 2000, 3L,
    commodity_year >= 2000 & commodity_year < 2008, 4L,
    commodity_year >= 2008 & commodity_year < 2014, 5L,
    commodity_year >= 2014 & commodity_year < 2018, 6L,
    commodity_year >= 2018, 7L,
    default = NA_integer_
  )]
  
  labs <- c("Pre farm bill","1980 farm bill","1994 farm bill","1996 farm bill",
            "2000 farm bill","2008 farm bill","2014 farm bill","2018 farm bill")
  
  df[, `:=`(
    period_farmbill = labs[as.integer(period_farmbill) + 1L],        
    period_combo = fcase(commodity_year >= 2012, "After",commodity_year <  2012, "Before",default = NA_character_)
  )]
  
  states <- as.data.table(usmap::fips_info(unique(df$state_code)))
  states[,fips := as.numeric(as.character(fips))]
  setnames(states,
           old = c("abbr", "fips", "full"),
           new = c("state_abbreviation", "state_code", "state_name"))
  df <- merge(df,states,by = c("state_code"),all.x = TRUE)
  rm(states);gc()
  
  return(df)
}








