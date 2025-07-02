# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")

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


#' Harmonize and summarize crop type codes from raw SOBTPU data
#'
#' @description
#' Download the raw “Summary of Business by Type, Practice, and Unit Structure” (SOBTPU)
#' data, clean and harmonize the crop type names for a fixed set of commodity codes,
#' determine a single dominant type per county, and save the resulting lookup table
#' of type renames.
#'
#' @details
#' This helper function performs the following steps:
#' 1. Downloads the raw SOBTPU data via `download_raw_data("sobtpu")` and reads it in.
#' 2. Filters out rows with missing, zero, infinite, or NaN `liability_amount`, and
#'    retains only the specified set of commodity codes (wheat, barley, rice, etc.).
#' 3. Recodes `type_name` by commodity:
#'    - **Wheat (11):** “DURUM”, “SPRING”, or “NO TYPE SPECIFIED”, dropping “FORAGE WHEAT FOR SEED”.
#'    - **Barley (91):** “SPRING” or “WINTER”.
#'    - **Rice (18):** “SHORT”, “LONG”, “MEDIUM”, or “NO TYPE SPECIFIED”, dropping certain japonica types.
#'    - **Other crops (41, 75, 94, 17, 81):** “GRAIN”, “SILAGE” (when `type_code == 26`), or “ALL”.
#' 4. Summarizes total `liability_amount` by `(commodity_code, state_code, county_code, type_name)`.
#' 5. Pivots to wide form, with one column per `type_name`, filling missing with zero.
#' 6. Determines the single “dominant” type for each county—i.e. the one with positive liability
#'    when all other types are zero.
#' 7. Joins this `type_recode` back onto the full dataset and applies final cleanup:
#'    - If `type_name` is not “NO TYPE SPECIFIED”, it overrides the county-level rename.
#'    - Fills any remaining blanks or `NA` in `type_recode` with “NO TYPE SPECIFIED”.
#' 8. Drops any residual invalid liability rows, selects and deduplicates the key columns,
#'    adds a `data_source` column, saves to `./data-raw/type_recodes.rds`, and returns the result.
#'    
#' @return
#' A `data.frame` with columns:
#' * `commodity_year`
#' * `state_code`
#' * `county_code`
#' * `commodity_code`
#' * `type_code`
#' * `type_recode`
#' * `data_source` (always “Summary of business by type, practice, and unit structure”)
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' # Build and retrieve the lookup of harmonized crop types
#' lookup_dt <- harmonize_crop_type_codes()
#' }
harmonize_crop_type_codes <- function(){
  
  # Download if sobtpu is missing OR more than 7 days old
  file_path <- "./data-raw/fastscratch/sobtpu.rds"
  if (
    !file.exists(file_path) ||
    difftime(Sys.time(), file.info(file_path)$mtime, units = "days") > 7
  ) {
    sobtpu <- rfcip::get_sob_data(
      sob_version = "sobtpu",
      year        = 1999:as.numeric(format(Sys.Date(), "%Y"))
    )
    data.table::setDT(sobtpu)
    saveRDS(sobtpu, file = file_path)
  }
  
  df <- readRDS("./data-raw/fastscratch/sobtpu.rds");setDT(df)
  
  # Define the codes we care about up‐front
  keep_codes <- c(11, 91, 18, 41, 75, 94, 17, 81)
  
  # 1) Filter out invalid liabilities and unwanted commodities
  df <- df[
    !is.na(liability_amount) & liability_amount != 0 & is.finite(liability_amount) &
      commodity_code %in% keep_codes
  ]
  
  # 2) Recode type_name by commodity_code
  #    use := by reference and fcase (data.table v1.14+)
  df[commodity_code == 11,  # Wheat
     type_name := fcase(
       grepl("DURUM",                           type_name), "DURUM",
       grepl("KHORASAN",                        type_name), "SPRING",
       grepl("NTS-HARVEST REVENUE OPTION",      type_name), "NO TYPE SPECIFIED",
       default = type_name
     )]
  # drop the unwanted wheat rows
  df <- df[!(commodity_code==11 & type_name=="FORAGE WHEAT FOR SEED")]
  
  df[commodity_code == 91,  # Barley
     type_name := fcase(
       grepl("SPRING", type_name), "SPRING",
       grepl("WINTER", type_name), "WINTER",
       default = type_name
     )]
  
  df[commodity_code == 18,  # Rice
     type_name := fcase(
       grepl("SHORT",      type_name), "SHORT",
       grepl("LONG",       type_name), "LONG",
       grepl("MEDIUM",     type_name), "MEDIUM",
       grepl("ALL OTHERS", type_name), "NO TYPE SPECIFIED",
       default = type_name
     )]
  # drop the unwanted rice varieties
  df <- df[!(commodity_code==18 & type_name %in% c("AKITAKOMACHI","HIMENO MOCHI","KOSHIHIKARI"))]
  
  # Other crops
  df[commodity_code == 41,                              type_name := "GRAIN"]
  df[commodity_code == 41 & type_code == 26,            type_name := "SILAGE"]
  df[commodity_code %in% c(75,94,17,81),                 type_name := "ALL"]
  
  # 3) Summarize liability by county+type
  county_type <- df[
    ,
    .(liability_amount = sum(liability_amount, na.rm=TRUE)),
    by=.(commodity_code, state_code, county_code, type_name)
  ]
  
  # 4) Pivot types into columns, filling missing with 0
  county_wide <- dcast(
    county_type,
    commodity_code + state_code + county_code ~ type_name,
    value.var = "liability_amount",
    fill = 0
  )
  
  # 5) Determine the one “dominant” type per county
  #    exclude “NO TYPE SPECIFIED” from consideration
  type_cols <- setdiff(
    names(county_wide),
    c("commodity_code","state_code","county_code","NO TYPE SPECIFIED")
  )
  
  county_wide[
    ,
    type_recode := {
      row <- unlist(.SD)
      positive <- which(row > 0)
      if (length(positive)==1L) names(row)[positive]
      else NA_character_
    },
    .SDcols = type_cols
  ]
  
  # 6) Join back onto the main df
  setkey(df, commodity_code, state_code, county_code)
  setkey(county_wide, commodity_code, state_code, county_code)
  
  df <- county_wide[df]  # left join: brings type_recode into df
  
  # 7) Final cleanup of type_recode
  df[
    ,
    type_recode := fifelse(
      type_name != "NO TYPE SPECIFIED",
      type_name,
      type_recode
    )
  ][
    ,
    type_recode := fifelse(
      is.na(type_recode) | type_recode %in% c("", " "),
      "NO TYPE SPECIFIED",
      type_recode
    )
  ]
  
  # 8) Drop any rows with invalid liability again (just in case),
  #    and select+dedupe the key columns
  df <- df[
    !is.na(liability_amount) & liability_amount != 0 & is.finite(liability_amount),
    .(commodity_year, state_code, county_code,
      commodity_code, type_code, type_recode)
  ]
  df <- unique(df)
  
  df[ , data_source := "USDA-RMA, Summary of business by type, practice, and unit structure"]
  
  return(df)
}


#' Harmonize election codes in a data table of insurance elections
#'
#' This function looks for two columns—`unit_structure_code` and
#' `insurance_plan_code`—and creates recoded versions grouping similar
#' codes into broader categories.
#'
#' @param df A `data.frame` containing, at minimum,
#'   one or both of the columns:
#'   - `unit_structure_code`
#'   - `insurance_plan_code`
#'
#' @return A `data.frame` with the same columns as `df` with new columns:
#'   - `unit_structure_recode` (if `unit_structure_code` was present)
#'   - `insurance_plan_recode` (if `insurance_plan_code` was present)
#' @export
harmonize_election_codes <- function(df){
  
  ## Recode unit structure codes into broader classes
  if ("unit_structure_code" %in% names(df)) {
    # OU – Optional Unit;
    # UD – OU established by UDO; and
    # UA – OU established by a WUA.
    # BU – Basic Unit;
    # EU – Enterprise Unit;
    # EP – Enterprise Unit by Irrigated and/or Non-Irrigated Practices;
    # EC – Enterprise Unit by FAC and/or NFAC Cropping Practices;
    # WU – Whole-farm Unit;
    df[, unit_structure_recode := ifelse(
      unit_structure_code %in% c("UD","UA","OU","", NA),
      "OU",
      unit_structure_code
    )]
    
    # Map all enterprise variants → "EU"
    df[, unit_structure_recode := ifelse(
      unit_structure_code %in% c("EU","EP","EC"),
      "EU",
      unit_structure_recode
    )]
  }
  
  ## Recode insurance plan codes to harmonize similar products
  if ("insurance_plan_code" %in% names(df)) {
    # Harmonize COMBO products
    # These three plans of insurance are similar but not identical. Some differences are:
    # • CRC bases the insurance guarantee on the higher of the base price or the harvest period price.
    # • IP and standard RA guarantees are determined using the base price, with no adjustment if the price increases.
    # • RA offers up-side price protection like that of CRC as an option but IP does not.
    # • IP limits unit formats to basic units, which include all interest in a crop in a county under identical ownership.
    # • RA is unique in offering coverage on whole farm units, integrating coverage from two to three crops.
    # check <- data[data$ins_plan_ab %in% c("YP","APH","IP","RP-HPE","RPHPE","CRC","RP","RA"),]
    
    # APH[90] → YP[1]
    df[, insurance_plan_recode := ifelse(
      insurance_plan_code %in% c(1, 90),
      1,
      insurance_plan_code
    )]
    
    # CRC[44] → RP[2]
    df[, insurance_plan_recode := ifelse(
      insurance_plan_code %in% c(44, 2),
      2,
      insurance_plan_recode
    )]
    
    # IP[42], RP-HPE[3], 25 → RP-HPE[3]
    df[, insurance_plan_recode := ifelse(
      insurance_plan_code %in% c(25, 42, 3),
      3,
      insurance_plan_recode
    )]
  }
  
  return(df)
}


#' Download and clean Insurance Control Elements tables
#'
#' @description
#' `get_ice_data()` retrieves all “YTD” ICE (Insurance Control Elements) text files
#' from the specified directory on the RMA public FTP site for one or more years,
#' downloads them to a temporary location, reads them as pipe-delimited data,
#' applies internal cleaning routines, and returns the combined dataset. Original
#' text files are discarded after reading.
#'
#' @param years
#'   Integer vector of calendar years to download (e.g. `2012:2020`). Defaults to `2012`.
#'
#' @param ice_url
#'   Character string giving the base URL of the ICE directory on the RMA FTP site.
#'   Must end with a slash. Defaults to
#'   `"https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/"`.
#'
#' @param selected_ice
#'   Character vector of keyword(s) or regular expressions to filter the filenames.
#'   Only ICE files whose names match at least one element of `selected_ice` will be
#'   downloaded. If `NULL`, all “YTD” files are processed.
#'
#' @return
#' A single `data.table` (invisibly coercible to `data.frame`) containing the cleaned
#' ICE data for all requested years. If no matching files are found or all downloads
#' fail, returns an empty `data.table`.
#'
#' @details
#' Internally, `get_ice_data()` uses
#' \code{\link[rmaADM]{locate_download_link}} to find all links ending in
#' “YTD.txt” for each year, then
#' \code{\link[utils]{download.file}} to fetch them, and
#' \code{\link[rmaADM]{clean_data}} to perform any standard cleanup before
#' combining with \code{\link[data.table]{rbindlist}}.
#'
#' @seealso
#' \code{\link[rmaADM]{locate_download_link}}, \code{\link[rmaADM]{clean_data}}
#'
#' @import dplyr
#' @importFrom stringr str_extract str_match_all
#' @importFrom data.table rbindlist
#' @importFrom utils download.file
#' @export
#'
#' @examples
#' \dontrun{
#' # Download & process ICE tables for 2018 and 2019,
#' # filtering for any file with “IceAOExpenseSubsidy” in its name
#' ice_df <- get_ice_data(
#'   years        = 2018:2019,
#'   selected_ice = "IceAOExpenseSubsidy"
#' )
#' }
get_ice_data <- function(
    years        = 2012,
    ice_url      = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
    selected_ice = NULL) {
  dt <- data.table::rbindlist(
    lapply(years, function(year) {
      tryCatch({
        ## locate and filter links ending in "YTD.txt"
        download_links <- rmaADM:::locate_download_link(
          year        = year,
          ice_url     = ice_url,
          data_source = "ice"
        ) %>% unlist(use.names = FALSE)
        download_links <- grep("YTD\\.txt$", download_links, value = TRUE)
        
        ## filter by user-supplied patterns, if any
        if (!is.null(selected_ice)) {
          pattern <- paste(selected_ice, collapse = "|")
          download_links <- grep(pattern, download_links, value = TRUE)
        }
        
        dt  <-  data.table::rbindlist(
          lapply(download_links, function(download_link) {
            tryCatch({
              ## download, read, clean
              tmp <- tempfile(fileext = ".txt")
              utils::download.file(download_link, destfile = tmp, mode = "wb")
              dt  <- readr::read_delim(tmp, delim = "|",
                                       col_names = TRUE, show_col_types = FALSE)
              dt  <- rmaADM:::clean_data(dt)
              dt
            }, error = function(e) {NULL})
          }),fill = TRUE)
      }, error = function(e) {NULL})
    }),fill = TRUE)
  gc()
  return(dt)
}
