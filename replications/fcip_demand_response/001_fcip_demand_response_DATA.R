
rm(list = ls(all = TRUE));gc();library(rfsa)

# Re-generate documentation for any R package functions in this project
devtools::document()

# Create directories for storing output (if they don’t already exist)
dir_estimations <- "./data-raw/fastscratch/replications/fcip_demand_response/output/estimations/"

if (!dir.exists(dir_estimations)) {
  dir.create(dir_estimations, recursive = TRUE)
}

replications_release <- "./data-raw/data_release/replications"

if (!dir.exists(replications_release)) {
  dir.create(replications_release, recursive = TRUE)
}

# (1) RMA’s summary of business files and contains insurance metrics aggregated by 
#    county, crop, crop type (e.g., corn can be grain or silage), production practice 
#    (e.g., irrigation, organic, etc..), insurance plan (e.g., Actual Production History [APH], 
#    Yield Protection [YP], Crop Revenue Coverage [CRC], Revenue Protection [RP], etc..), 
#    coverage level, and insurance unit (Optional unit [OU], Enterprise unit [EU], etc..); 
#    RMA refers to this data source as “Summary of Business” by “Type, Practice, Unit Structure” or “SOBTPU” for short.  
#    The SOBTPU serves as the foundational data source for the study. 

sobtpu <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobtpu_all.rds",
  sobtpu, mode = "wb", quiet = TRUE)
sobtpu <- readRDS(sobtpu)
sobtpu <- sobtpu[commodity_year %in% 2001:2022]
sobtpu <- sobtpu[coverage_type_code %in% "A"]
sobtpu <- sobtpu[insurance_plan_abbreviation %in% c("APH", "YP", "CRC", "RP","RA","RPHPE", "IP")]
sobtpu <- sobtpu[reporting_level_type %in% c("Acres")]
sobtpu[,coverage_level_percent := ifelse(coverage_level_percent>1,coverage_level_percent/100,coverage_level_percent)]
sobtpu[,potential_liability_amount := liability_amount/coverage_level_percent]

sobtpu <- sobtpu[
  , lapply(.SD, sum), 
  by = c("commodity_year",FCIP_INSURANCE_POOL,"commodity_name","insurance_plan_code","unit_structure_code"), 
  .SDcols = c("net_reporting_level_amount","potential_liability_amount","liability_amount",
              "total_premium_amount","subsidy_amount")]
sobtpu[,aggregate_coverage_level := liability_amount/potential_liability_amount]
sobtpu[,premium_per_liability := total_premium_amount/liability_amount]
sobtpu[,subsidy_per_premium := subsidy_amount/total_premium_amount]
sobtpu <- sobtpu[!aggregate_coverage_level %in% c(0,NA,Inf,-Inf,NaN)]
sobtpu <- sobtpu[!net_reporting_level_amount %in% c(0,NA,Inf,-Inf,NaN)]
sobtpu <- sobtpu[!subsidy_per_premium %in% c(0,NA,Inf,-Inf,NaN)]
sobtpu <- sobtpu[!premium_per_liability %in% c(0,NA,Inf,-Inf,NaN)]

# (2) Actuarial information (i.e., design parameters for the FCIP that govern the premiums 
#     farmers face when making crop insurance decisions), projected, and harvest price from RMA’s Actuarial Data Master (ADM)
adm_rates <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_aph_base_rate.rds",
  adm_rates, mode = "wb", quiet = TRUE)
adm_rates <- readRDS(adm_rates)

adm_price <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_commodity_price.rds",
  adm_price, mode = "wb", quiet = TRUE)
adm_price <- readRDS(adm_price)

# (3) per-acre cost of crop production was approximated with state-level rental rates retrieved from NASS Quick Stats.
# state_rental_rates
state_rental_rates <- nass_state_rental_rates

# (4) Price index  
index_for_price_recived <- nass_index_for_price_recived

# (5) Acreage data from Farm Service Agency (FSA) with missing data filled in with planted, harvested, and baring acres 
#     from USDA National Agricultural Statistics Service (NASS) Quick Stats, in that order
nass_production_data <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/nass_extracts/nass_production_data.rds",
  nass_production_data, mode = "wb", quiet = TRUE)
nass_production_data <- readRDS(nass_production_data)

data(fsaCropAcreage)
fsa_acreage_data <- as.data.frame(fsaCropAcreage)





acreage_nass  <- readRDS("C:/GitHub/US-FarmSafetyNet-Lab/data-raw/data_release/miscellaneous/acreage_data_nass.rds");gc()
acreage_fsa   <- readRDS("C:/GitHub/US-FarmSafetyNet-Lab/data-raw/data_release/miscellaneous/acreage_data_fsa.rds");gc()
acreage_nass$county$commodity_name <- toupper(acreage_nass$county$commodity_name)

area <- as.data.frame(dplyr::full_join(acreage_fsa$county,acreage_nass$county,
                                       by=c( "commodity_name","state_code","county_code","commodity_year")))
area$A <- ifelse(area$fsa_COUNTY_plant %in% c(NA,NaN,Inf,-Inf,0),area$plant_COUNTY,area$fsa_COUNTY_plant)
area$A <- ifelse(area$A %in% c(NA,NaN,Inf,-Inf,0),area$harvet_COUNTY,area$A)
area$A <- ifelse(area$A %in% c(NA,NaN,Inf,-Inf,0),area$bearing_COUNTY,area$A)
area <- doBy::summaryBy(list(c("A"),c("commodity_name","state_code","county_code","commodity_year")),
                        data=area,FUN=mean,keep.names = T,na.rm=T)






# (5) Merge the data sources
df <- sobtpu[adm_rates[, c("commodity_year","state_code","county_code","commodity_code","tau_adm","tau_sob","subsidy_rate_65","subsidy_rate_75"), with = FALSE], 
             on = c("commodity_year","state_code","county_code","commodity_code"), nomatch = 0][
               adm_price,, on = c("state_code","county_code","type_code","commodity_year","commodity_code"), nomatch = 0][
                 state_rental_rates[, c("state_code","commodity_year","rent"), with = FALSE],
                 , on = c("state_code","commodity_year"), nomatch = 0] 















