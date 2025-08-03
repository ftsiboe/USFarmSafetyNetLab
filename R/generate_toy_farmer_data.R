#' Generate a sample farmer-level toy dataset for testing
#'
#' @param n Integer number of farmer records to generate. Default 100.
#' @param year Numeric value of commodity year. Default 2020.
#' @param state Numeric vector of state FIPS codes. Default 19.
#' @param counties Numeric vector of county FIPS codes. Default 127.
#' @param commodity Numeric vector of commodity codes. Default 41.
#' @param types Numeric vector of type codes. Default 16.
#' @param practices Numeric vector of practice codes. Default c(2, 3, 997).
#' @param unit_codes Character vector of unit structure codes. Default c("BU", "OU", "EU").
#' @param plan_codes Numeric vector of insurance plan codes. Default c(1:6, 16:17, 90).
#' @param coverage_codes Character vector of coverage type codes. Default c("A", "C").
#' @param cov_level_range Numeric vector of length 2 giving the min and max for coverage_level_percent. Default c(0.8, 0.8).
#' @param yield_mean Numeric mean for approved_yield and rate_yield. Default 100.
#' @param yield_sd Numeric standard deviation for approved_yield and rate_yield. Default 10.
#' @param price_mean Numeric mean for expected price. Default 2.5.
#' @param price_sd Numeric standard deviation for expected price. Default 1.5.
#' @param price_share Numeric
#' @param area_share Numeric
#' @param planted_area Numeric
#' @param production_history_length Integer vector of indices for actual_farm_yield columns. Default 0:10.
#' 
#' @return A \code{data.table} with one row per farmer and columns:
#' \itemize{
#'   \item \code{farmid}
#'   \item \code{commodity_year}
#'   \item \code{state_code}, \code{county_code}
#'   \item \code{commodity_code}, \code{type_code}, \code{practice_code}
#'   \item \code{unit_structure_code}, \code{insurance_plan_code}
#'   \item \code{coverage_type_code}, \code{coverage_level_percent}
#'   \item \code{approved_yield}, \code{rate_yield}
#'   \item \code{reported_acres}, \code{insured_share_percent}
#'   \item \code{price_election_percent}, \code{dmage_area_rate}, \code{market_year_price}
#'   \item \code{actual_farm_yield_0} to \code{actual_farm_yield_<max(production_history_length)>}
#' }
#'
#' @examples
#' farmdata <- generate_toy_farmer_data()
#' farmdata <- generate_toy_farmer_data(
#'   n = 20,
#'   cov_level_range = c(0.7, 0.9),
#'   production_history_length = 0:2
#' )
#' head(farmdata)
#'
#' @import data.table
#' @importFrom stats rnorm runif
#' @export
generate_toy_farmer_data <- function(
    n                        = 100,
    year                     = 2020,
    state                    = 18,
    counties                 = 7:23,
    commodity                = 41,
    types                    = c(26,16),
    practices                = c(2, 3, 997),
    unit_codes               = c("BU", "OU", "EU"),
    plan_codes               = c(1:6, 16:17, 90),
    coverage_codes           = c("A", "C"),
    cov_level_range          = c(0.5, 0.95),
    yield_mean               = 100,
    yield_sd                 = 10,
    price_mean               = 2.5,
    price_sd                 = 1.5,
    price_share              = NULL,
    area_share               = NULL,
    planted_area             = NULL,
    production_history_length = 0:10){
  # create base table
  dt <- data.table(
    farmid          = seq_len(n),
    commodity_year  = year,
    state_code      = state,
    commodity_code  = commodity
  )
  
  if(length(counties) %in% 1){
    dt[, county_code := counties]
  }else{
    dt[, county_code := sample(counties, n, replace = TRUE)]
  }
  
  if(length(types) %in% 1){
    dt[, type_code := types]
  }else{
    dt[, type_code := sample(types, n, replace = TRUE)]
  }
  
  if(length(practices) %in% 1){
    dt[, practice_code := practices]
  }else{
    dt[, practice_code := sample(practices, n, replace = TRUE)]
  }
  
  if(length(unit_codes) %in% 1){
    dt[, unit_structure_code := unit_codes]
  }else{
    dt[, unit_structure_code := sample(unit_codes, n, replace = TRUE)]
  }
  
  if(length(plan_codes) %in% 1){
    dt[, insurance_plan_code := plan_codes]
  }else{
    dt[, insurance_plan_code := sample(plan_codes, n, replace = TRUE)]
  }
  
  if(length(coverage_codes) %in% 1){
    dt[, coverage_type_code := coverage_codes]
  }else{
    dt[, coverage_type_code := sample(coverage_codes, n, replace = TRUE)]
  }
  
  if(length(cov_level_range) %in% 1){
    dt[, coverage_level_percent := cov_level_range]
  }else{
    dt[, coverage_level_percent := sample(cov_level_range, n, replace = TRUE)]
  }
  
  
  
  if(is.null(planted_area)){
    dt[, reported_acres := sample(50:1000, n, replace = TRUE),]
  }else{
    dt[, reported_acres := planted_area]
  }
  
  if(is.null(area_share)){
    dt[, insured_share_percent := sample(c(0.5, 1), n, replace = TRUE),]
  }else{
    dt[, insured_share_percent := area_share]
  }
  
  if(is.null(price_share)){
    dt[, price_election_percent := round(runif(n, 0.8, 1.2), 2)]
  }else{
    dt[, price_election_percent := price_share]
  }
  
  # add actual_farm_yield_i columns
  for (i in production_history_length) {
    col <- paste0("farm_yield_", i)
    dt[, (col) := as.integer(pmax(
      0,
      round(yield_mean * runif(.N, 0.5, 1.5) + rnorm(.N, 0, yield_sd))
    ))]
  }
  setnames(dt,old = c("farm_yield_0"),new = c("farm_yield"))
  
  dt[, rate_yield := round(rowMeans(.SD),digits = 0),.SDcols = patterns("^farm_yield_")]
  dt[, approved_yield := rate_yield*sample(seq(1,2,0.01), n, replace = TRUE)]
  dt[, dmage_area_rate :=  round(runif(n, 0, 1), 2)]
  dt[, market_price   :=  abs(round(price_mean * runif(n, 0.5, 1.5) + rnorm(n, 0, price_sd),2))]
  
  # cap/floor coverage_level_percent by plan code
  dt[insurance_plan_code %in% c(1:3, 90) & coverage_level_percent > 0.85,
     coverage_level_percent := 0.85]
  dt[insurance_plan_code %in% 4:6 & coverage_level_percent > 0.90,
     coverage_level_percent := 0.90]
  dt[insurance_plan_code %in% 4:6 & coverage_level_percent < 0.65,
     coverage_level_percent := 0.65]
  dt[insurance_plan_code %in% 16:17 & coverage_level_percent < 0.70,
     coverage_level_percent := 0.70]
  
  # adjust for coverage type C
  dt[coverage_type_code == "C" & insurance_plan_code %in% c(1, 90),
     coverage_level_percent := 0.50]
  dt[coverage_type_code == "C" & insurance_plan_code == 4,
     coverage_level_percent := 0.65]
  dt[coverage_type_code == "C" & !insurance_plan_code %in% c(1, 90, 4),
     coverage_type_code := "A"]
  
  # adjust unit structure and price election
  dt[!insurance_plan_code %in% c(1:3, 90), unit_structure_code := "OU"]
  dt[insurance_plan_code %in% 2:3, price_election_percent        := 1]
  
  dt[, (grep("^farm_yield_", names(dt), value = TRUE)) := NULL]
  
  dt[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(dt))) := lapply(
    .SD, function(x) as.numeric(as.character(x))
  ), .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(dt))]
  
  return(dt[])
}