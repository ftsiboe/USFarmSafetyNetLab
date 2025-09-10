#' Clean and aggregate RMA Summary of Business (SOB-TPU) data
#'
#' @description
#' Retrieves RMA SOB-TPU records for requested years, combining **live** years
#' (last 5 years, fetched via [rfcip::get_sob_data()]) with **stable** years
#' (downloaded from a prebuilt `.rds` release), then filters, harmonizes
#' insurance plan codes, coverage levels, and unit structure codes, and returns
#' an analysis-ready `data.table` aggregated to common keys.
#'
#' @param years Integer vector of commodity years.
#' @param insurance_plan Optional integer vector of harmonized plan codes to keep after harmonization (1=YP, 2=RP, 3=RP-HPE). If `NULL`, keep all.
#' @param acres_only Logical; keep only acres-level records. Default `TRUE`.
#' @param addon_only Logical; exclude CAT (`coverage_type_code == "C"`). Default `TRUE`.
#' @param harmonize_insurance_plan_code Logical; recode plans to (1,2,3). Default `TRUE`.
#' @param harmonize_coverage_level_percent Logical; normalize coverage levels to decimal in 0.50 to 0.95 at 0.05 steps. Default `TRUE`.
#' @param harmonize_unit_structure_code Logical; recode unit structure to (OU, BU, and EU). Default `TRUE`.
#'
#' @return A `data.table` aggregated to the keys with columns:
#'   `insured_acres`, `endorsed_acres`, `liability_amount`,
#'   `total_premium_amount`, `subsidy_amount`, `indemnity_amount`.
#'
#' @import data.table
#' @importFrom rfcip get_sob_data
#' @importFrom utils download.file
#' @export
clean_rma_sobtpu <- function(
    years = as.numeric(format(Sys.Date(),"%Y"))-1,
    insurance_plan = NULL,
    acres_only = TRUE,
    addon_only = TRUE,
    harmonize_insurance_plan_code = TRUE,
    harmonize_coverage_level_percent = TRUE,
    harmonize_unit_structure_code = TRUE
){
  # Pull SOB-TPU data
  current_year <- as.numeric(format(Sys.Date(),"%Y"))
  live_years   <- years[years %in% (current_year-5):current_year]
  stable_years <- years[!years %in% live_years]

  if(length(live_years)>0){
    live_sobtpu <- get_sob_data(sob_version = "sobtpu", year = live_years,force = TRUE)
  }else{
    live_sobtpu <- NULL
  }

  if(length(stable_years)>0){
    stable_sobtpu <- tempfile(fileext = ".rds")
    download.file(
      "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobtpu_all.rds",
      stable_sobtpu, mode = "wb", quiet = TRUE)
    stable_sobtpu <- readRDS(stable_sobtpu)
    data.table::setDT(stable_sobtpu)
    stable_sobtpu <- stable_sobtpu[commodity_year %in% stable_years]
  }else{
    stable_sobtpu <- NULL
  }

  sob <- data.table::rbindlist(list(live_sobtpu,stable_sobtpu), fill = TRUE)

  # Ensure data.table and expected columns
  setDT(sob)
  needed <- c(
    "insurance_plan_code", "coverage_type_code", "reporting_level_type",
    "coverage_level_percent", "commodity_year", "state_code", "county_code",
    "commodity_code", "type_code", "practice_code", "unit_structure_code",
    "liability_amount", "total_premium_amount", "subsidy_amount", "indemnity_amount",
    "net_reporting_level_amount"  # adjust if your feed uses a different field name
  )
  missing_cols <- setdiff(needed, names(sob))
  if (length(missing_cols)) {
    stop(sprintf(
      "Missing required columns in SOB-TPU feed: %s. Check field naming (e.g., net_reporting_level_amount).",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Basic filters
  if (addon_only)  sob <- sob[coverage_type_code != "C"]
  if (acres_only)  sob <- sob[reporting_level_type == "Acres"]

  # Coerce types for robust recoding
  sob[, insurance_plan_code := as.integer(insurance_plan_code)]
  sob[, coverage_level_percent := as.numeric(coverage_level_percent)]

  # Harmonize plan codes (only if requested)
  # These three plans of insurance are similar but not identical. Some differences are:
  # * CRC bases the insurance guarantee on the higher of the base price or the harvest period price.
  # * IP and standard RA guarantees are determined using the base price, with no adjustment if the price increases.
  # * RA offers up-side price protection like that of CRC as an option but IP does not.
  # * IP limits unit formats to basic units, which include all interest in a crop in a county under identical ownership.
  # * RA is unique in offering coverage on whole farm units, integrating coverage from two to three crops.
  # check <- data[data$ins_plan_ab %in% c("YP","APH","IP","RP-HPE","RPHPE","CRC","RP","RA"),]
  if(isTRUE(harmonize_insurance_plan_code)){
    # APH[90] -> YP[1]
    sob[insurance_plan_code %in% c(1L, 90L), insurance_plan_code := 1L]
    # CRC[44] -> RP[2]
    sob[insurance_plan_code %in% c(44L, 2L), insurance_plan_code := 2L]
    # IP[42], RP-HPE[3], [25] -> RP-HPE[3]
    sob[insurance_plan_code %in% c(25L, 42L, 3L), insurance_plan_code := 3L]
  }

  # Now, if user requested a subset of (harmonized) plans, apply it here
  if (!is.null(insurance_plan)) {
    keep_plans <- as.integer(insurance_plan)
    sob <- sob[insurance_plan_code %in% keep_plans]
  }

  # Optionally harmonize unit structure codes
  # OU - Optional Unit;
  # UD - OU established by UDO; UA - OU established by WUA
  # BU - Basic Unit;
  # EU - Enterprise Unit;
  # EP - Enterprise Unit by Irrigated/Non-Irrigated;
  # EC - Enterprise Unit by FAC/NFAC;
  # WU - Whole-farm Unit
  if (isTRUE(harmonize_unit_structure_code)) {
    sob[unit_structure_code %in% c("UD", "UA", "OU", "", NA),unit_structure_code := "OU"]
    sob[unit_structure_code %in% c("EU", "EP", "EC", "WU"),unit_structure_code := "EU"]
    sob[unit_structure_code %in% "BU",unit_structure_code := "BU"]
  }

  # Optionally normalize coverage levels
  if (isTRUE(harmonize_coverage_level_percent)) {
    # Convert >1 to decimals
    sob[!is.na(coverage_level_percent) & coverage_level_percent > 1,coverage_level_percent := coverage_level_percent / 100]
    # Round to nearest 0.05 and clamp to [0.50, 0.95]
    sob[!is.na(coverage_level_percent),coverage_level_percent := pmin(0.95, pmax(0.50,round(coverage_level_percent / 0.05) * 0.05))]
    # fix floating artifacts to 2 decimals
    sob[!is.na(coverage_level_percent),coverage_level_percent := as.numeric(sprintf("%.2f", coverage_level_percent))]
  }

  # Aggregate (do NOT force to {1,2,3} unless you truly want that)
  sob <- sob[
    ,
    .(
      insured_acres        = sum(net_reporting_level_amount, na.rm = TRUE),
      endorsed_acres       = sum(endorsed_commodity_reporting_level_amount, na.rm = TRUE),
      liability_amount     = sum(liability_amount,           na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount,       na.rm = TRUE),
      subsidy_amount       = sum(subsidy_amount,             na.rm = TRUE),
      indemnity_amount     = sum(indemnity_amount,           na.rm = TRUE)
    ),
    by = .(
      commodity_year, state_code, county_code, commodity_code, type_code, practice_code,
      unit_structure_code, insurance_plan_code, coverage_type_code, coverage_level_percent
    )
  ]

  # Keep rows with positive liability amount
  sob <- sob[is.finite(liability_amount) & liability_amount > 0]

  sob[]
}
