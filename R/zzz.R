.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  calculate_mode <- function(x,na.rm = T){ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

}

# Note: data.table's `:=` creates these columns at runtime. We register them here
# so that R CMD check doesn’t flag “no visible binding for global variable”.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(unique(c(
    '.',"asd_code", "commodity_code", "commodity_desc", "commodity_name",
    "commodity_year", "county_ansi", "county_code", "data_source",
    "insurance_plan_code", "insurance_plan_recode", "liability_amount",
    "marketing_year_avg_price", "price_received", "state_code", "state_fips_code",
    "type_code", "type_name", "type_recode", "unit_structure_code",
    "unit_structure_recode", "util_practice_desc", "value",
    "CROP", "agg_level_desc", "area", "asd_cd",
    "c_K", "c_P", "c_Z", "c_a", "c_alpha", "c_net_acre",
    "c_u", "c_v", "c_x", "census_year", "class_desc",
    "contiguous_county", "contiguous_county_code",
    "contiguous_state_code", "coverage_level_percent",
    "crop_year", "delivery_type", "domain_desc",
    "domaincat_desc", "freq_desc", "indemnity_amount",
    "insured_area", "lcr", "loss_ratio",
    "prodn_practice_desc", "production", "sector_desc",
    "short_desc", "state", "state_alpha", "statisticcat_desc",
    "su", "su_class", "su_practice", "su_use",
    "subsidy_amount", "subsidy_rate", "tau", "tau_c",
    "tau_sob", "total_premium_amount", "unit_desc"
  )))
}