#' Standardize FCIP Data Frame Column Names
#'
#' Rename a set of common Federal Crop Insurance Program (FCIP) data frame
#' columns to consistent, descriptive names. This ensures uniform naming
#' across different data sources and simplifies downstream processing.
#'
#' @param df A `data.frame` containing FCIP data with short column names such as `crop_yr`, `state_cd`, etc.
#' @return The input `df` with Standardized column names:
#' @examples
#' \dontrun{
#' # Suppose fcip_raw is loaded with original column names
#' fcip_clean <- standardize_fcip_column_names(fcip_raw)
#' # Now fcip_clean has standardized, descriptive column names
#' }
#' @export
standardize_fcip_column_names <- function(df) {
  names(df)[names(df) %in% "crop_yr"]               <- "commodity_year"
  names(df)[names(df) %in% "state_cd"]              <- "state_code"
  names(df)[names(df) %in% "state_ab"]              <- "state_abbreviation"
  names(df)[names(df) %in% "county_cd"]             <- "county_code"
  names(df)[names(df) %in% "county"]                <- "county_name"
  names(df)[names(df) %in% "crop_cd"]               <- "commodity_code"
  names(df)[names(df) %in% "crop"]                  <- "commodity_name"
  names(df)[names(df) %in% "ins_plan_cd"]           <- "insurance_plan_code"
  names(df)[names(df) %in% "ins_plan_ab"]           <- "insurance_plan_abbreviation"
  names(df)[names(df) %in% "cov_typ"]               <- "coverage_type_code"
  names(df)[names(df) %in% "cov_lvl"]               <- "coverage_level_percent"
  names(df)[names(df) %in% "delivery_sys"]          <- "delivery_id"
  names(df)[names(df) %in% "pol_sold_cnt"]          <- "policies_sold"
  names(df)[names(df) %in% "pol_prem_cnt"]          <- "policies_earning_prem"
  names(df)[names(df) %in% "pol_indem_cnt"]         <- "policies_indemnified"
  names(df)[names(df) %in% "unit_prem_cnt"]         <- "units_earning_prem"
  names(df)[names(df) %in% "unit_indem_cnt"]        <- "units_indemnified"
  names(df)[names(df) %in% "net_acre_typ"]          <- "reporting_level_type"
  names(df)[names(df) %in% "net_acre_qty"]          <- "net_reporting_level_amount"
  names(df)[names(df) %in% "edr_acre_qty"]          <- "endorsed_commodity_reporting_level_amount"
  names(df)[names(df) %in% "liability_amt"]         <- "liability_amount"
  names(df)[names(df) %in% "total_prem"]            <- "total_premium_amount"
  names(df)[names(df) %in% "subsidy"]               <- "subsidy_amount"
  names(df)[names(df) %in% "subsidy_prvt"]          <- "state_private"
  names(df)[names(df) %in% "subsidy_add"]           <- "addnl_subsidy"
  names(df)[names(df) %in% "subsidy_efa"]           <- "efa_prem_discount"
  names(df)[names(df) %in% "lr"]                    <- "loss_ratio"
  names(df)[names(df) %in% "indem_amt"]             <- "indemnity_amount"
  names(df)[names(df) %in% "stage_cd"]              <- "stage_code"
  names(df)[names(df) %in% "damage_cd"]             <- "col_code"
  names(df)[names(df) %in% "damage"]                <- "col_name"
  names(df)[names(df) %in% "month_cd"]              <- "month_of_loss_code"
  names(df)[names(df) %in% "month_ab"]              <- "month_of_loss_name"
  names(df)[names(df) %in% "det_acre_qty"]          <- "indemnified_quantity"
  names(df)[names(df) %in% "pnt_acre_qty"]          <- "net_planted_qty"

  df
}









