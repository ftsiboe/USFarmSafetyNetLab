
# Prepared By Francis Tsiboe (ftsiboe@hotmail.com)  
# Citation requirement;
# 1. Tsiboe,F. and Turner, D., 2023. Econometric identification of crop insurance participation. Agricultural and Resource Economics Review, 52(3):476-497. https://doi.org/10.1017/age.2023.13
# 2. Tsiboe,F. and Turner, D., 2023. The crop insurance demand response to premium subsidies: Evidence from US Agriculture. Food Policy, 119. https://doi.org/10.1016/j.foodpol.2023.102505

rm(list=ls(all=TRUE));gc();library(data.table);library(magrittr)

devtools::load_all()

base_url <- "https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download"
version  <- "v0.1.0"

current_year <- as.numeric(format(Sys.Date(),"%Y"))-2

# # Download and process summary of business from RMA's app
# sobapp <- as.data.table(rfcip::get_sob_data(year = 1990:current_year, group_by = c("county","crop")))
# sobapp[,state_abbreviation := state_abbrv]
# sobapp <- sobapp[, .(insured_area = sum(ifelse(grepl("Acre", quantity_type), quantity, 0), na.rm = TRUE),
#                      liability_amount = sum(liabilities, na.rm = TRUE),
#                      indemnity_amount = sum(indemnity, na.rm = TRUE)),
#                  by = c("commodity_year","state_code","state_abbreviation","county_code",
#                         "county_name","commodity_code","commodity_name")]
# 
# # Download and process Historical summary of business by state, county, crop, and coverage
# sobcov <- tempfile(fileext = ".rds")
# download.file(
#   paste(base_url, version, 
#          "historical_summary_of_business_by_state_county_crop_coverage.rds", sep = "/"),
#   sobcov, mode = "wb",quiet=TRUE)
# sobcov <- as.data.table(readRDS(sobcov))
# sobcov <- sobcov[, .(insured_area = sum(ifelse(grepl("ACRE", reporting_level_type), net_reporting_level_amount, 0), na.rm = TRUE),
#                      liability_amount = sum(liability_amount, na.rm = TRUE),
#                      indemnity_amount = sum(indemnity_amount, na.rm = TRUE)),
#                  by = c("commodity_year","state_code","state_abbreviation","county_code",
#                         "county_name","commodity_code","commodity_name")]
# 
# # Download and process Historical summary of business by state, county, and crop
# sobscc <- tempfile(fileext = ".rds")
# download.file(
#   paste(base_url, version, 
#         "historical_summary_of_business_by_state_county_crop.rds", sep = "/"),
#   sobscc, mode = "wb",quiet=TRUE)
# sobscc <- as.data.table(readRDS(sobscc))
# sobscc <- sobscc[, .(insured_area = sum(net_reporting_level_amount, na.rm = TRUE),
#                      liability_amount = sum(liability_amount, na.rm = TRUE),
#                      indemnity_amount = sum(indemnity_amount, na.rm = TRUE)),
#                  by = c("commodity_year","state_code","state_abbreviation","county_code",
#                         "county_name","commodity_code","commodity_name")]
# 
# # Combine datasets
# sob <- rbind(sobapp[commodity_year >= 1990],sobscc[1989:1991], sobcov[commodity_year <= 1988])
# rm(sobapp, sobscc, sobcov);gc()
# 
# # Aggregate over the new dataset without specific crop codes
# sob_all_crops <- sob[, .(insured_area = sum(insured_area, na.rm = TRUE),
#                          liability_amount = sum(liability_amount, na.rm = TRUE),
#                          indemnity_amount = sum(indemnity_amount, na.rm = TRUE),
#                          commodity_code = 0, commodity_name="All crops"),
#                    by = c("commodity_year","state_code","state_abbreviation","county_code","county_name")]
# 
# # Append datasets, excluding the generic crop code from the combined dataset
# sob <- rbind(sob_all_crops, sob[!county_code %in% 9999])
# rm(sob_all_crops);gc()
# 
# # Calculate Loss Cost Ratio (LCR) for risk assessment
# sob[, lcr := indemnity_amount/liability_amount]
# 
# saveRDS(sob,file="data-raw/sob.rds")

sob <- readRDS("data-raw/sob.rds")

# Read contiguous county data for spatial analysis
contiguous_county <- tempfile(fileext = ".rds")
download.file(
  paste(base_url, version, 
        "contiguous_county.rds", sep = "/"),
  contiguous_county, mode = "wb",quiet=TRUE)
contiguous_county <- as.data.table(readRDS(contiguous_county))


estimate_fcip_unloaded_rate <- function(
    data   = data,
    contiguous_county = contiguous_county,
    year   = 2011,
    crop   = NULL,
    state  = NULL,
    county = NULL,
    fips   = NULL){
  
  
  
  # The calculations in this loop are based on procedures found on 
  # page 65-70 of 2009 FCIC Rate Methodology Handbook APH
  # https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf
  
  data <- as.data.table(data)
  
  group_data <- data[contiguous_county, on = .(state_code, county_code), nomatch = 0
  ][, .(state_code = contiguous_state_code, county_code = contiguous_county_code)]
  
  target_data <- data[state_code %in% state & county_code %in% county][, on = .(state_cd, county_cd), nomatch = 0]
  
  
  
  group_data  <- unique(rbind(group_data[statplan  , on = .(state_cd, county_cd), nomatch = 0],target_data))
  
  # County Group LCR and Variance(includes target):
  group_data <- group_data[, .(
    c_alpha = mean(net_acre,na.rm=T),c_a = var(lcr,na.rm=T),
    c_u = mean(lcr,na.rm=T)), by = .(crop_cd)]
  
  # Target County LCR & Variance
  target_data <- target_data[, .(
    c_v = var(lcr,na.rm=T), c_x = mean(lcr,na.rm=T),
    c_net_acre = sum(net_acre,na.rm=T)), by = .(state_cd,county_cd,crop_cd)]
  
  data <- target_data[group_data, on = .(crop_cd), nomatch = 0]
  data[, c_P := c_net_acre/c_alpha]
  data[, c_K := c_v/c_a]
  data[, c_Z := c_P/(c_P+c_K)]
  data[, tau := c_Z*c_x + (1-c_Z)*c_u] # County Unloaded Rate (same as target rate).
  return(as.data.frame(data)[c("state_cd","county_cd","crop_cd","tau")])
}






estimate_fcip_instruments <- function(year,sob){
  tryCatch({
    # year <- 1970
    # Extract relevant years of data for each county
    statplan <- sob[commodity_year %in% (year-2):(year-21)] 
    
    # List of unique state and county combinations
    task_list <- unique(statplan[, .(state_code, county_code)])
    
    
    

    
    
    
    tryCatch({
    
    }, error = function(e){return(NULL)})
    
    # Process data for each county, calculating unloaded rate (ULR)
    ADM <- data.table::rbindlist(
      lapply(
        1:nrow(worklist),
        ), fill = TRUE)
    
    # Fill in missing values using contiguous counties' mean
    setDT(ADM)
    contiguous <- readRDS("ContiguousCounty.rds")
    setDT(contiguous)
    contiguous[, state_cd := Contiguous.State.Code]
    contiguous[, county_cd := Contiguous.County.Code]
    contiguous_adm <- unique(contiguous, by = c("State.Code", "County.Code"))
    
    contiguous_adm <- data.table::rbindlist(
      lapply(
        1:nrow(contiguous_adm),
        function(ss){
          tryCatch({
            # ss <- 1
            data <- contiguous_adm[ss][contiguous, on = .(State.Code, County.Code), nomatch = 0][
              ADM, on = .(state_cd, county_cd), nomatch = 0]
            
            data <- data[, .(tau_c = mean(tau, na.rm = TRUE)),by = .(State.Code, County.Code, crop_cd)]
            
            setnames(data, old = c("State.Code", "County.Code"), new = c("state_cd", "county_cd"))
            return(data)
          }, error = function(e){return(NULL)})
        }), fill = TRUE)
    
    ADM <- ADM[contiguous_adm, on = intersect(names(ADM), names(contiguous_adm)), nomatch = 0]
    ADM[, tau_sob := fifelse(tau %in% c(NA, Inf, -Inf, NaN) | tau == 0, tau_c, tau)]
    rm(contiguous_adm);gc()
    ADM <- as.data.frame(ADM)
    ADM <- ADM[names(ADM)[!names(ADM) %in% c("tau_c","tau")]]
    ADM <- ADM[!ADM$tau %in% c(NA, Inf, -Inf, NaN,0),]
    ADM <- dplyr::inner_join(unique(as.data.frame(soball[crop_yr %in% year])[c("crop_yr","state_cd","state_ab","county_cd","county","crop_cd","crop")]),
                             ADM, by=names(ADM)[names(ADM) %in% c("crop_yr","state_cd","state_ab","county_cd","county","crop_cd","crop")])
    gc()
    return(ADM)
  }, error = function(e){return(NULL)})
}















# Loop through years to process data and calculate target rate (tau).
instruments <- as.data.frame(
  data.table::rbindlist(
    lapply(
      (min(soball$crop_yr)+22):max(soball$crop_yr),
      ), fill = TRUE))

# merge Instrument (i.e., target rate) aggregated directly from RMA’s actuarial data master 
adm <- readRDS("fcip_instruments_from_adm.rds")
instruments <- dplyr::full_join(instruments,adm, by=names(instruments)[names(instruments) %in% names(adm)])

# formulate and merge national subsidy rate instrument as described by (Yu et al., 2018)
yu2018 <- as.data.frame(
  data.table::rbindlist(
    lapply(
      list.files(paste0(dir_datasets,"/sob/SOBCOV"),recursive = T,full.names = T,pattern = "SOBCOV_"),
      function(file){
        return(readRDS(file))
      }), fill = TRUE))
yu2018 <- yu2018[yu2018$delivery_sys %in% c("RBUP","FBUP"),]
yu2018 <- yu2018[yu2018$ins_plan_cd %in% c(1:3,90,44,25,42),]
yu2018$cov_lvl  <- paste0("subsidy_rate_",(round((yu2018$cov_lvl/0.05))*0.05)*100)
yu2018 <- yu2018[yu2018$cov_lvl %in% c("subsidy_rate_65","subsidy_rate_75"),]
yu2018 <- doBy::summaryBy(subsidy+total_prem~crop_yr+cov_lvl,data=yu2018,FUN=sum,na.rm=T,keep.names = T)
yu2018$subsidy <- yu2018$subsidy/yu2018$total_prem
yu2018 <- yu2018[c("crop_yr","cov_lvl","subsidy")] %>% tidyr::spread(cov_lvl, subsidy)
instruments <- dplyr::full_join(instruments,yu2018, by=names(instruments)[names(instruments) %in% names(yu2018)])

# tau_final: Same as tau_adm with missing data filled in with tau_sob (as is). 
instruments$tau_final <- ifelse(instruments$tau_adm %in% c(NA,Inf,-Inf,NaN,0),instruments$tau_sob,instruments$tau_adm)

instruments <- instruments[c("crop_yr","state_ab","state_cd","county","county_cd","crop","crop_cd",
                             "tau_sob","tau_adm","tau_final","subsidy_rate_65","subsidy_rate_75")]
instruments <- instruments[!instruments$tau_final %in% c(NA,Inf,-Inf,NaN,0),]

# Save the processed data to an RDS file for use
instruments <- instruments[!instruments$crop_yr %in% as.numeric(format(Sys.Date(),"%Y")),]
saveRDS(instruments, "final_fcip_instruments.rds")

