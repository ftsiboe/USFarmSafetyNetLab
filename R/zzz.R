.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

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
    "unit_structure_recode", "util_practice_desc", "value"
  )))
}