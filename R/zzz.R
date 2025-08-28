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
        "
    . CROP agg_level_desc area asd_cd asd_code c_K c_P c_Z c_a c_alpha
    c_net_acre c_u c_v c_x census_year class_desc commodity_code
    commodity_desc commodity_name commodity_year contiguous_county
    contiguous_county_code contiguous_state_code county_ansi county_code
    coverage_level_percent crop_year data_source delivery_type
    domain_desc domaincat_desc freq_desc indemnity_amount
    insurance_plan_code insurance_plan_recode insured_area lcr
    liability_amount loss_ratio marketing_year_avg_price price_received
    prodn_practice_desc production sector_desc short_desc state
    state_alpha state_code state_fips_code statisticcat_desc su su_class
    su_practice su_use subsidy_amount subsidy_rate tau tau_c tau_sob
    total_premium_amount type_code type_name type_recode unit_desc
    unit_structure_code unit_structure_recode util_practice_desc value
    approved_yield coverage_type_code dmage_area_rate
    insured_share_percent market_price practice_code
    price_election_percent rate_yield record_category_code
    reference_amount_code reported_acres
    ..final_cols ..keep_cols ALL county_fips estimate_smooth obs
    IQR cagr change coef disaggregate outcome pct_change
    rank_among_outcomes reinsurance_year sd unit_structure_rename filename time
    area_km2 cause_of_loss_description centroid
    coverage_level_percent_recode coverage_type_code_recode cx cy
    damage_name_recode geometry is_small label state_abbv value_cat fcip_contiguous_county
        county_acreage county_cd coverage_level_dominant
    coverage_level_percent_aggregate coverage_level_percent_avg
    coverage_level_percent_max coverage_level_percent_wavg crop_cd
    crop_yr data fcip_recodes_commodity_groupings
    fcip_recodes_insurance_plan fcip_recodes_practice fips fsaCropAcreage
    fsa_crop_linker fsa_planted_acres harvest_price na.omit
    nassSurvey_AREA_BEARING nassSurvey_AREA_HARVESTED
    nassSurvey_AREA_PLANTED nass_index_for_price_recived
    nass_state_rental_rates net_reporting_level_amount period_farmbill
    planted_acres pool potential_liability_amount premium_per_liability
    price projected_price reporting_level_type
    standardized_liability_amount state_cd subsidy_bins
    subsidy_per_premium subsidy_rate_65 subsidy_rate_75 tau_adm
    triger_level
      ",
        "\\s+"
      )[[1]]
    )
  }
}




