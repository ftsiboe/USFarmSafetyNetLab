#' Prepare and Save USDA NASS Data for Release
#'
#' @description
#' Downloads and processes multiple USDA NASS datasets-including census, economics, crops,
#' production, marketing-year average prices, and state rental rates-and saves each as
#' an RDS file in a specified directory.
#'
#' @param dir_dest Character. Target directory for saving processed NASS data files
#'   (default: `"data-raw/data_release/nass/"`).
#' @param dir_source Character. Directory where raw NASS Quick Stats CSVs are stored
#'   or will be downloaded (default: `"./data-raw/fastscratch/nass/"`).
#'
#' @details
#' This function first ensures that `dir_dest` exists, creating it if necessary. It then
#' downloads the raw NASS Quick Stats datasets for the specified census years, economics,
#' and crops into `dir_source` via `downloaded_nass_large_datasets()`. Next, it retrieves
#' the annual Index for Price Received from the economics dataset with
#' `process_nass_dataset()`, normalizes it to the current year, and saves the result as
#' `nass_survey_index_for_price_received.rds`. It then extracts production and area data at the
#' national, state, and county levels using `get_nass_production_data()`, tags the data
#' source, and writes `nass_survey_production_data.rds`. For commodity price averages, it calls
#' `get_marketing_year_avg_price()` for a predefined list of items, labels the source,
#' and saves `nass_survey_marketing_year_avg_price.rds`. Finally, it obtains annual state cropland
#' cash rent and asset-value data with `get_state_rental_rates()`, notes the bias-correction
#' and interpolation methods used, and saves `nass_survey_state_rental_rates.rds`. The function returns
#' a character vector of all filenames in `dir_dest`.
#'
#' @seealso
#' * \code{\link{downloaded_nass_large_datasets}} - download raw NASS Quick Stats CSVs  
#' * \code{\link{process_nass_dataset}} - fetch and reshape raw NASS data  
#' * \code{\link{get_nass_production_data}} - compute production and area metrics  
#' * \code{\link{get_marketing_year_avg_price}} - calculate marketing-year average prices  
#' * \code{\link{get_state_rental_rates}} - assemble state rental rate data  
#'
#' @return Character vector of filenames created in `dir_dest`.
#' @export
prep_nass_data <- function(
    dir_dest = "data-raw/data_release/nass/",
    dir_source = "./data-raw/fastscratch/nass/") {
  
  if (!dir.exists(dir_dest)) {
    dir.create(dir_dest, recursive = TRUE)
  }
  
  if (!dir.exists(dir_source)) {
    dir.create(dir_source, recursive = TRUE)
  }
  
  # Download database
  downloaded_nass_large_datasets(
    large_datasets = c(
      paste0("census", c(2022, 2017, 2012, 2007, 2002)),"economics","crops"),
    dir_dest = dir_source)
  
  # Index for price received
  df <- process_nass_dataset(
    dir_source    = dir_source,
    large_dataset  = "economics",
    nassqs_params  = list(
      short_desc = "COMMODITY TOTALS - INDEX FOR PRICE RECEIVED, 2011",
      freq_desc  = "ANNUAL"))
  
  df <- df[freq_desc %in% "ANNUAL",.(index_for_price_recived = mean(value, na.rm = TRUE)),by = c("commodity_year")]
  
  # df[, index_for_price_recived := index_for_price_recived/df[commodity_year %in% 2023][["index_for_price_recived"]]] 
  
  df[, data_source := "USDA NASS Quick Stats"]
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_index_for_price_received.rds"));rm(df); gc()
  
  # Production data
  df <- get_nass_production_data(dir_source = dir_source,source_desc = "SURVEY",agg_level_desc = c("NATIONAL", "STATE", "COUNTY"))
  df[, data_source := "USDA NASS Quick Stats"]
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_production_data.rds"));rm(df); gc()
  
  # Get Marketing Year Average Price
  df <- get_marketing_year_avg_price(
    dir_source    = dir_source,
    agg_level_desc = c("NATIONAL", "STATE"),
    short_desc     = c(
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
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_marketing_year_avg_price.rds"));rm(df); gc()
  
  # State rental rates
  df <- get_state_rental_rates(dir_source=dir_source)
  df[, data_source :=
       paste0(
         "USDA NASS Quick Stats (annual state cropland cash rent & asset value); ",
         "missing values filled by panel-regression bias correction and 5-nearest-neighbor spatial interpolation"
       )]
  
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_state_rental_rates.rds"));rm(df); gc()
  
  # Census Data
  get_nass_census_data(censuses = as.numeric(gsub("[^0-9]","",list.files(dir_source,pattern = "census"))),
                       dir_source = dir_source,
                       dir_dest = dir_dest)
  
  # Historical Track Record Crop Production
  df <- get_nass_historical_track_record_crop(dir_source = dir_source)
  df[, data_source :="USDA NASS Historical Track Record Crop Production"]
  saveRDS(df, file = paste0(dir_dest, "/nass_historical_track_record_crop.rds"));rm(df); gc()
  
  return(list.files(dir_dest))
}