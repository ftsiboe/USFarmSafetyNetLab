.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)
  
  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # requireNamespace("rmaADM", quietly = TRUE)
  requireNamespace("rfcip", quietly = TRUE)
  
  # Note: data.table's `:=` creates these columns at runtime. We register them here
  # so that R CMD check does not flag no visible binding for global variable.
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        " filename area_km2 centroid commodity_year cx cy geometry is_small label state_abbv value value_cat
        
        approved_yield county_code coverage_level_percent coverage_type_code dmage_area_rate insurance_plan_code insured_share_percent
    market_price practice_code price_election_percent rate_yield reported_acres type_code unit_structure_code
    
    . cagr change disaggregate outcome pct_change rank_among_outcomes
    
    estimate_smooth
    
    cause_of_loss_description commodity_code
    coverage_level_percent_recode coverage_type_code_recode
    damage_name_recode data_source insurance_plan_recode liability_amount
    state_code type_name type_recode unit_structure_recode
    unit_structure_rename reinsurance_year
    
    CROP agg_level_desc area asd_cd asd_code census_year class_desc
    commodity_desc commodity_name county_ansi crop_year domain_desc
    domaincat_desc map marketing_year_avg_price price_received
    prodn_practice_desc production sector_desc short_desc state
    state_alpha state_fips_code statisticcat_desc su su_class su_practice
    su_use unit_desc util_practice_desc
    
      ",
        "\\s+"
      )[[1]]
    )
  }
}




