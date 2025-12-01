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
        "file_name max_size size_mb . CROP agg_level_desc area asd_cd asd_code cause_of_loss_description
    census_year class_desc commodity_code commodity_desc commodity_name
    commodity_year county_ansi county_code coverage_level_percent
    coverage_level_percent_recode coverage_type_code
    coverage_type_code_recode crop_year damage_name_recode data_source
    domain_desc domaincat_desc filename insurance_plan_code
    insurance_plan_recode liability_amount marketing_year_avg_price
    price_received prodn_practice_desc production reinsurance_year
    sector_desc short_desc state state_alpha state_code state_fips_code
    statisticcat_desc su su_class su_practice su_use type_code type_name
    type_recode unit_desc unit_structure_code unit_structure_recode
    unit_structure_rename util_practice_desc value map PPIPR agg_level census_directory comm index index_for_price_recived",
        "\\s+"
      )[[1]]
    )
  }
}




