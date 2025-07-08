# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")

#' Download and Process RMA Web Data Files
#'
#' Fetches one or more years of USDA RMA “Summary of Business” data
#' (state–county–crop & livestock participation series), unzips,
#' reads the pipe-delimited text, applies the correct column names,
#' and saves each year as an \code{.rds} under \code{dest}.
#'
#' @param years Integer vector of crop or livestock years to fetch.
#' @param file_name Character; one of
#'   \code{"sobcov"}, \code{"sobtpu"}, \code{"lgm"}, \code{"lrp"}, or \code{"colsom"}.
#'   Determines both the subdirectory and the column schema.
#' @param dest Character path where the final \code{.rds} files will go.
#'   Defaults to \code{"./data-raw/data_release"}.
#' @param url_rma_web_data_files Base URL for USDA RMA web data.
#'   Defaults to the official RMA “Web_Data_Files” root.
#' @return Invisibly returns \code{NULL}. Side effect: one \code{.rds}
#'   per year is written under \code{dest}, named \code{<file_name>_<year>.rds}.
#'   
#' @import data.table
#' @importFrom utils unzip download.file
#'  
#' @examples
#' \dontrun{
#' # Get sobtpu data for 2018–2022
#' download_rma_web_data_files(2018:2022, "sobtpu")
#' }
#' @export
download_rma_web_data_files <- function(
    years,
    file_name,
    dest = "./data-raw/data_release",
    url_rma_web_data_files = "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files"
) {
  # 1. Check for existing files in `dest` and drop years already
  #    downloaded within the last 5 years. This avoids redundant downloads.
  # List all files matching `file_name` (recursive)
  existing <- list.files(dest, recursive = TRUE, full.names = TRUE, pattern = file_name)
  existing <- as.numeric(gsub("[^0-9]","",existing[!grepl("pdf|txt", existing)]))
  existing <- existing[!existing %in% (as.numeric(format(Sys.Date(), "%Y")) - 5):as.numeric(format(Sys.Date(), "%Y")) ]
  years <- setdiff(years, existing)
  
  # 2. Build the download URLs based on `file_name` category
  if (file_name %in% c("sobcov", "sobtpu")) {
    # State–county–crop data
    download_urls <- data.frame(
      url  = paste0(
        url_rma_web_data_files,
        "/Summary_of_Business/state_county_crop/",
        file_name, "_", years, ".zip"
      ),
      year = years,
      stringsAsFactors = FALSE
    )
  }
  
  if (file_name %in% c("colsom")) {
    download_urls <- data.frame(
      url  = paste0(
        url_rma_web_data_files,
        "/Summary_of_Business/cause_of_loss/",
        file_name, "_", years, ".zip"
      ),
      year = years,
      stringsAsFactors = FALSE
    )
  }
  
  if (file_name %in% c("lgm", "lrp")) {
    # Livestock & dairy participation data
    download_urls <- data.frame(
      url  = paste0(
        url_rma_web_data_files,
        "/Summary_of_Business/livestock_and_dairy_participation/",
        file_name, "_", years, ".zip"
      ),
      year = years,
      stringsAsFactors = FALSE
    )
  }
  
  # 3. Loop over each URL, download, unzip, read, name columns, save .rds
  lapply(seq_len(nrow(download_urls)), function(i) {
    tryCatch({
      # a) Download to a temporary zip file
      temp_zip <- tempfile(fileext = ".zip")
      utils::download.file(
        download_urls$url[i],
        destfile = temp_zip,
        mode     = "wb",
        quiet    = TRUE
      )
      
      # b) Unzip into a temp folder
      temp_txt <- tempfile()
      utils::unzip(zipfile = temp_zip, exdir = temp_txt)
      
      # c) Read the single pipe-delimited text file
      data <- utils::read.delim2(
        file   = list.files(temp_txt, full.names = TRUE),
        sep    = "|",
        header = FALSE,
        skipNul = TRUE
      )
      
      # d) Clean up temp files immediately
      unlink(temp_zip)
      unlink(temp_txt, recursive = TRUE)
      
      # e) Apply the proper column names per `file_name`
      if (file_name == "sobtpu") {
        colnames(data) <- c(
          "commodity_year",
          "state_code",
          "state_name",
          "state_abbreviation",
          "county_code",
          "county_name",
          "commodity_code",
          "commodity_name",
          "insurance_plan_code",
          "insurance_plan_abbreviation",
          "coverage_type_code",
          "coverage_level_percent",
          "delivery_id",
          "type_code",
          "type_name",
          "practice_code",
          "practice_name",
          "unit_structure_code",
          "unit_structure_name",
          "net_reporting_level_amount",
          "reporting_level_type",
          "liability_amount",
          "total_premium_amount",
          "subsidy_amount",
          "indemnity_amount",
          "loss_ratio",
          "endorsed_commodity_reporting_level_amount"
        )
      }
      
      if (file_name == "sobcov") {
        colnames(data) <- c(
          "commodity_year",
          "state_code",
          "state_abbreviation",
          "county_code",
          "county_name",
          "commodity_code",
          "commodity_name",
          "insurance_plan_code",
          "insurance_plan_name_abbreviation",
          "coverage_category",
          "delivery_type",
          "coverage_level_percent",
          "policies_sold_count",
          "policies_earning_premium_count",
          "policies_indemnified_count",
          "units_earning_premium_count",
          "units_indemnified_count",
          "quantity_type",
          "net_reported_quantity",
          "endorsed_companion_acres",
          "liability_amount",
          "total_premium_amount",
          "subsidy_amount",
          "state_private_subsidy",
          "additional_subsidy",
          "efa_premium_discount",
          "indemnity_amount",
          "loss_ratio"
        )
      }
      
      if (file_name == "colsom") {
        colnames(data) <- c(
          "commodity_year",
          "state_code",
          "state_abbreviation",
          "county_code",
          "county_name",
          "commodity_code",
          "commodity_name",
          "insurance_plan_code",
          "insurance_plan_name_abbreviation",
          "coverage_category",
          "stage_code",
          "cause_of_loss_code",
          "cause_of_loss_description",
          "month_of_loss",
          "month_of_loss_name",
          "year_of_loss",
          "policies_earning_premium",
          "policies_indemnified",
          "net_planted_quantity",
          "net_endorsed_acres",
          "liability",
          "total_premium",
          "producer_paid_premium",
          "subsidy",
          "state_private_subsidy",
          "additional_subsidy",
          "efa_premium_discount",
          "net_determined_quantity",
          "indemnity_amount",
          "loss_ratio"
        )
      }
      
      if (file_name == "lgm") {
        colnames(data) <- c(
          "reinsurance_year",
          "commodity_year",
          "location_state_code",
          "location_state_abbreviation",
          "location_county_code",
          "location_county_name",
          "commodity_code",
          "commodity_name",
          "insurance_plan_code",
          "insurance_plan_name",
          "type_code",
          "type_code_name",
          "practice_code",
          "practice_code_name",
          "sales_effective_date",
          paste0("target_marketings_", 1:11),
          paste0("corn_equivalent_", 2:11),
          paste0("soybean_meal_equivalent_", 2:11),
          "endorsements_earning_premium",
          "endorsements_indemnified",
          "deductible",
          "live_cattle_target_weight_quantity",
          "feeder_cattle_target_weight_quantity",
          "corn_target_weight_quantity",
          "liability_amount",
          "total_premium_amount",
          "subsidy_amount",
          "producer_premium_amount",
          "indemnity_amount"
        )
      }
      
      if (file_name == "lrp") {
        colnames(data) <- c(
          "reinsurance_year",
          "commodity_year",
          "location_state_code",
          "location_state_abbreviation",
          "location_county_code",
          "location_county_name",
          "commodity_code",
          "commodity_name",
          "insurance_plan_code",
          "insurance_plan_name",
          "type_code",
          "type_code_name",
          "practice_code",
          "practice_code_name",
          "sales_effective_date",
          "endorsement_length",
          "coverage_price",
          "expected_end_value",
          "coverage_level_percent",
          "rate",
          "cost_per_cwt",
          "end_date",
          "endorsements_earning_premium",
          "endorsements_indemnified",
          "net_number_of_head",
          "total_weight",
          "subsidy_amount",
          "total_premium_amount",
          "producer_premium_amount",
          "liability_amount",
          "indemnity_amount"
        )
      }
      
      # f) Save the cleaned data to disk
      data <- data.table::as.data.table(data)
      data[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))) := lapply(
        .SD, function(x) as.numeric(as.character(x))), 
        .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))]
      
      saveRDS(data,file = file.path(dest, paste0(file_name, "_", download_urls$year[i], ".rds")))
      
    }, error = function(e) {
      # If any single-year download or parsing fails, skip silently
      message("Warning: failed for ", download_urls$url[i], ": ", e$message)
    })
    
    invisible(NULL)
  })
  
  invisible(NULL)
}



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
  names(df)[names(df) %in% "typ_cd"]                  <- "type_code"
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
#' 1. Downloads the raw SOBTPU data and reads it in.
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
  # Wheat
  df[,type_name := ifelse(commodity_code %in% 11 & grepl("DURUM", type_name),"DURUM",type_name)]
  df[,type_name := ifelse(commodity_code %in% 11 & grepl("KHORASAN", type_name),"SPRING",type_name)]
  df[,type_name := ifelse(commodity_code %in% 11 & grepl("NTS-HARVEST REVENUE OPTION", type_name),"NO TYPE SPECIFIED",type_name)]
  
  # drop the unwanted wheat rows
  df <- df[!(commodity_code %in% 11 & type_name %in% "FORAGE WHEAT FOR SEED")]
  
  # Barley
  df[,type_name := ifelse(commodity_code %in% 91 & grepl("SPRING", type_name),"SPRING",type_name)]
  df[,type_name := ifelse(commodity_code %in% 91 & grepl("WINTER", type_name),"WINTER",type_name)]
  
  # Rice
  df[,type_name := ifelse(commodity_code %in% 18 & grepl("SHORT", type_name),"SHORT",type_name)]
  df[,type_name := ifelse(commodity_code %in% 18 & grepl("LONG", type_name),"LONG",type_name)]
  df[,type_name := ifelse(commodity_code %in% 18 & grepl("MEDIUM", type_name),"MEDIUM",type_name)]
  df[,type_name := ifelse(commodity_code %in% 18 & grepl("ALL OTHERS", type_name),"NO TYPE SPECIFIED",type_name)]
  
  # drop the unwanted rice varieties
  df <- df[!(commodity_code==18 & type_name %in% c("AKITAKOMACHI","HIMENO MOCHI","KOSHIHIKARI"))]
  
  # Other crops
  df[,type_name := ifelse(commodity_code %in% 41,"GRAIN",type_name)]
  df[,type_name := ifelse(commodity_code %in% 41 & type_code %in% 26,"SILAGE",type_name)]
  df[,type_name := ifelse(commodity_code %in% c(75,94,17,81),"ALL",type_name)]
  
  
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


#' Estimate FCIP Unloaded (County) Rates
#'
#' Computes the “unloaded” loss cost rates (\code{tau}) for counties based on
#' the FCIC Rate Methodology Handbook (2009), pp. 65–70.
#'
#' @param statplan A \link[data.table]{data.table} containing FCIP rate elements
#'   with at least the columns:
#'   \describe{
#'     \item{state_code, county_code}{Identifiers for each county.}
#'     \item{contiguous_state_code, contiguous_county_code}{Mapping to county group.}
#'     \item{insured_area}{Total insured acres in the county.}
#'     \item{lcr}{Loss Cost Rate for each county.}
#'     \item{commodity_code}{Crop identifier.}
#'   }
#' @param year Integer. Crop year for which rates are being estimated
#'   (currently not used but reserved for future subsetting).
#' @param crop Optional vector of commodity codes to filter by crop.
#' @param state Optional vector of state codes to restrict the analysis.
#' @param county Optional vector of county codes to restrict the analysis.
#'
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{state_code, county_code, commodity_code}{Keys identifying county and crop.}
#'     \item{tau}{Estimated FCIP county “unloaded” rate.}
#'   }
#' @import data.table
#' 
#' @details
#' 1. **Target data** is filtered to the selected state(s)/county(ies).  
#' 2. **Group data** finds contiguous‐county groupings, unions them with the target.  
#' 3. Computes group‐level statistics:
#'   - \code{c_alpha}: mean insured acres  
#'   - \code{c_u}: mean LCR  
#'   - \code{c_a}: variance of LCR  
#' 4. Computes target county statistics:
#'   - \code{c_x}: mean LCR  
#'   - \code{c_v}: variance of LCR  
#'   - \code{c_net_acre}: total insured acres  
#' 5. Applies the blending formula  
#'   \deqn{\tau = Z\,x + (1 - Z)\,u, \quad Z = P/(P + K)}  
#'   where  
#'   \eqn{P = c_{\!net\_acre}/c_\alpha,\quad K = c_v/c_a.}
#'
#' @references
#' FCIC Rate Methodology Handbook APH (2009), pp. 65–70.  
#' \url{https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf}
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- fread("my_statplan.csv")
#' rates <- estimate_fcip_unloaded_rate(dt, year = 2011,
#'                                      state = c("01","02"),
#'                                      county = c("003","005"))
#' head(rates)
#' }
#'
#' @export
estimate_fcip_unloaded_rate <- function(
    statplan,
    year   = 2011,
    crop   = NULL,
    state  = NULL,
    county = NULL) {
  # Documentation link:
  # https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf
  
  # 1. Filter to the target county(ies)/state(s)
  target_data <- statplan[
    state_code %in% state & county_code %in% county
  ]
  
  # 2. Build the contiguous‐county group for each target county
  group_data <- target_data[contiguous_county, on = .(state_code, county_code), nomatch = 0
  ][, .(state_code = contiguous_state_code, county_code = contiguous_county_code)]
  group_data <- unique(group_data)
  
  # 3. Combine group members with the original target_data
  group_data  <- unique(rbind(group_data[statplan  , on = .(state_code, county_code), nomatch = 0],target_data))
  
  # 4. Compute group-level insured area mean (c_alpha), LCR mean (c_u), LCR variance (c_a)
  group_data <- group_data[, .(
    c_alpha = mean(insured_area,na.rm=T),c_a = var(lcr,na.rm=T),
    c_u = mean(lcr,na.rm=T)), by = .(commodity_code)]
  
  # 5. Compute target county LCR stats: variance (c_v), mean (c_x), net insured acres (c_net_acre)
  target_data <- target_data[, .(
    c_v = var(lcr,na.rm=T), c_x = mean(lcr,na.rm=T),
    c_net_acre = sum(insured_area,na.rm=T)), by = .(state_code,county_code,commodity_code)]
  
  # 6. Join target county stats with group stats by commodity_code
  res <- target_data[group_data, on = .(commodity_code), nomatch = 0]
  
  # 7. Calculate P, K, Z, and tau according to the handbook formula
  res[, c_P := c_net_acre/c_alpha]
  res[, c_K := c_v/c_a]
  res[, c_Z := c_P/(c_P+c_K)]
  res[, tau := c_Z*c_x + (1-c_Z)*c_u] # County Unloaded Rate (same as target rate).
  
  # 8. Filter out invalid or zero tau values
  res <- res[!tau %in% c(NA, Inf, -Inf, NaN, 0), ]
  
  # 9. Return a clean data.frame with only the columns of interest
  res <- res[, .(state_code, county_code, commodity_code, tau)]
  
  return(res)
}


#' Estimate FCIP Instrumental Variables (Unloaded Rates)
#'
#' Uses historical FCIP rate data to build instrumented unloaded‐rate variables
#' following:
#'   1. Tsiboe & Turner (2023), “Econometric identification of crop insurance participation”  
#'      _Agricultural and Resource Economics Review_, 52(3):476–497.  
#'      \url{https://doi.org/10.1017/age.2023.13}  
#'
#' @param year Integer. The target crop year for which to construct instruments.
#' @param statplan A data.table containing FCIP rate elements, including at least:
#'   \describe{
#'     \item{commodity_year}{Year of the rate observation.}
#'     \item{state_code, county_code}{County identifiers.}
#'     \item{commodity_code}{Crop identifier.}
#'     \item{insured_area, lcr, contiguous_state_code, contiguous_county_code}{Fields
#'       required by \code{estimate_fcip_unloaded_rate()}.}
#'   }
#'
#' @import data.table
#' @return A data.table with one row per county–crop for the specified \code{year},
#'   containing:
#'   \describe{
#'     \item{state_code, county_code, commodity_code}{Keys.}
#'     \item{tau_sob}{Smoothed unloaded rate (uses contiguous‐county means to fill zeros/NAs).}
#'     \item{commodity_year}{The input \code{year}, repeated.}
#'   }
#'
#' @details
#' 1. **Task list**: Identify all unique (state, county) pairs with data in the  
#'    2–21 years before \code{year}.  
#' 2. **Unloaded‐rate calculation**: For each county in \code{task_list}, call  
#'    \code{estimate_fcip_unloaded_rate()} on the same 2–21 year window to get \code{tau}.  
#'    Errors return \code{NULL} so processing continues.  
#' 3. **Contiguous‐county smoothing**:  
#'    - Build a lookup table of contiguous counties (using \code{contiguous_county}).  
#'    - For each contiguous group, compute the mean \code{tau} to get \code{tau_c}.  
#' 4. **Merge & fill**: Left‐join the raw \code{adm} and \code{contiguous_adm};  
#'    replace any zero/NA/Inf \code{tau} with the group mean \code{tau_c} into  
#'    \code{tau_sob}.  
#' 5. **Cleanup**: Drop helper columns (\code{tau}, \code{tau_c}), remove invalid rows,  
#'    add \code{commodity_year}, and return the result.
#'
#' @seealso \code{\link{estimate_fcip_unloaded_rate}}
#' @export
estimate_fcip_instruments <- function(year, statplan) {
  
  # 1. Build list of (state, county) with at least 2–21 years of data before 'year'
  task_list <- unique(statplan[commodity_year %in% (year-2):(year-21), .(state_code, county_code)])
  
  # 2. For each county, compute the unloaded rate via the helper function
  adm <- data.table::rbindlist(
    lapply(
      1:nrow(task_list),
      function(i){
        tryCatch({
          estimate_fcip_unloaded_rate(
            statplan = statplan[commodity_year %in% (year-2):(year-21)],
            year   = year,
            state  = task_list$state_code[i],
            county = task_list$county_code[i])
        }, error = function(e){return(NULL)})
      }), fill = TRUE)
  setDT(adm)
  
  # 3. Prepare contiguous county mapping for smoothing
  setDT(contiguous_county)
  contiguous_county[, state_code := contiguous_state_code]
  contiguous_county[, county_code := contiguous_county_code]
  contiguous_adm <- unique(contiguous_county, by = c("state_code", "county_code"))
  
  # 4. For each contiguous group, compute the mean tau => tau_c
  contiguous_adm <- data.table::rbindlist(
    lapply(
      1:nrow(contiguous_adm),
      function(ss){
        tryCatch({
          # ss <- 1
          data <- contiguous_adm[ss][contiguous_county, on = .(state_code, county_code), nomatch = 0][
            adm, on = .(state_code, county_code), nomatch = 0]
          
          data <- data[, .(tau_c = mean(tau, na.rm = TRUE)),by = .(state_code, county_code, commodity_code)]
          
          return(data)
        }, error = function(e){return(NULL)})
      }), fill = TRUE)
  
  # 5. Merge raw rates with contiguous‐county smoothed rates
  adm <- adm[contiguous_adm, on = intersect(names(adm), names(contiguous_adm)), nomatch = 0]
  
  # 6. Replace any invalid/zero tau with the smoothed tau_c
  adm[, tau_sob := fifelse(tau %in% c(NA, Inf, -Inf, NaN) | tau == 0, tau_c, tau)]
  
  # 7. Drop helper columns and invalid rows
  rm(contiguous_adm);gc()
  adm <- adm[, setdiff(names(adm), c("tau_c", "tau")), with = FALSE]
  adm <- adm[!tau_sob %in% c(NA, Inf, -Inf, NaN,0)]
  
  # 8. Tag with the target commodity_year and return
  adm[, commodity_year := year]
  gc()
  return(adm)
}


#' Formulate & Merge National Subsidy Rate Instrument (Yu et al., 2018)
#'
#' Downloads the historical “Summary of Business” RDS and computes
#' national subsidy‐rate instruments at specified coverage levels,
#' following Yu et al. (2018).
#'
#' @param dt sobcov
#' @param delivery_systems Character vector. Delivery systems to include;
#'                   default \code{c("RBUP","FBUP")}.
#' @param plan_codes Integer vector. Insurance plan codes to include;
#'                   default \code{c(1:3, 90, 44, 25, 42)}.
#' @param coverage_levels Numeric vector. Percent coverage levels to keep;
#'                   default \code{c(65, 75)}.
#'
#' @return A data.table with columns: commodity_year, subsidy_rate_65, subsidy_rate_75.
#'
#' @import data.table
#' @importFrom tidyr spread
#' @export
get_yu2018_instrument <- function(
    dt,
    delivery_systems  = c("RBUP", "FBUP"),
    plan_codes        = c(1:3, 90, 44, 25, 42),
    coverage_levels   = c(65, 75)) {
  # 2. Read into data.table
  dt <- data.table::as.data.table(dt)
  
  # 3. Filter to relevant delivery systems & plan codes
  dt <- dt[delivery_type %in% delivery_systems]
  dt <- dt[insurance_plan_code   %in% plan_codes]
  
  # 4. Create a label for each coverage level
  #    (round to nearest 5% then convert to a “subsidy_rate_##” string)
  dt[, coverage_level_percent := paste0("subsidy_rate_",(round((coverage_level_percent / 0.05)) * 0.05) * 100)]
  
  # 5. Keep only the coverage levels we need
  dt <- dt[coverage_level_percent %in% paste0("subsidy_rate_", coverage_levels)]
  
  # 6. Summarize total subsidy and premium by crop year & coverage level
  dt_sum <- dt[
    , .(
      subsidy_amount    = sum(subsidy_amount,    na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount, na.rm = TRUE)
    ),by = .(commodity_year, coverage_level_percent)]
  
  # 7. Convert to rate = subsidy_amount / total premium
  dt_sum[, subsidy_rate := subsidy_amount / total_premium_amount]
  
  # 8. Reshape to wide: one column per coverage level’s subsidy rate
  #    (requires tidyr)
  dt_wide <- dt_sum[, .(commodity_year, coverage_level_percent, subsidy_rate)] %>%
    tidyr::spread(coverage_level_percent, subsidy_rate)
  
  # 9. Return as data.table
  return(data.table::as.data.table(dt_wide))
}


#' Prepare FCIP Data for Release
#'
#' @description
#' Downloads, processes, and caches multiple Federal Crop Insurance Program (FCIP)
#' datasets—Summary of Business (SOB), Cause of Loss (COL), and Actuarial Data Master (ADM)—
#' saving them as RDS files in a local cache directory structure.
#'
#' @param dir_dest Character. Base directory under which to store processed FCIP data.
#'   Defaults to `"data-raw/data_release"`. Subdirectories `sob`, `col`, and `adm`
#'   will be created within `dir_dest` if they do not already exist.
#'
#' @details
#' 1. **Determine source paths**  
#'    - On Windows, locates the `farmpolicylab` database under the user's OneDrive→Dropbox folder.  
#'    - On non-Windows, uses `~/Database/USA/USDA/`.  
#'
#' 2. **Setup local cache directories**  
#'    - Ensures `dir_dest` exists.  
#'    - Creates subfolders `sob`, `col`, and `adm` if missing.  
#'
#' 3. **Summary of Business (SOB)**  
#'    - Downloads yearly CSV ZIPs via `download_rma_web_data_files()` for `sobtpu`
#'      and `sobcov`, plus field description PDFs.  
#'    - Unzips and reads the historic 1948–1988 SOB data, coerces selected columns to
#'      numeric, and saves as `sobscc_1948_1988.rds`.  
#'
#' 4. **Cause of Loss (COL)**  
#'    - Downloads yearly CSV ZIPs for `colsom`, plus field description PDF and
#'      Excel code listing.  
#'    - Unzips and reads historic 1948–1988 COL data, coerces selected columns
#'      (including `indemnity_amount`) to numeric, computes `loss_ratio`, filters
#'      invalid rows, and saves as `col_sob_hist.rds`.  
#'
#' 5. **Actuarial Data Master (ADM)**  
#'    - Reads RDS files under `rmaFCIPdata/.../base_rate/`, computes `tau_adm = Rr + Rf`,
#'      averages by `(crop_yr, state_cd, county_cd, crop_cd)`, and saves
#'      `fcip_demand_instruments_from_adm.rds`.  
#'    - Reads ERS price projections RDS, renames price columns, averages by
#'      `(state_cd, county_cd, typ_cd, crop_yr, crop_cd)`, standardizes column names,
#'      and saves `fcip_commodity_prices.rds`.  
#'
#' @return A list of three character vectors, naming the files saved in:
#'   - `dir_dest/sob`  
#'   - `dir_dest/col`  
#'   - `dir_dest/adm`  
#'
#' @import data.table
#' @importFrom utils download.file unzip read.delim2
#' @importFrom doBy summaryBy
#' @export
prep_fcip_data <- function(dir_dest = "data-raw/data_release"){
  
  if(Sys.info()['sysname'] %in% "Windows"){
    farmpolicylab <- paste0(gsub("OneDrive","Dropbox",Sys.getenv("OneDriveConsumer")),"/farmpolicylab/database/")
  }else{
    farmpolicylab <- paste0("~/Database/USA/USDA/")
  }
  
  # Create the cache directory if it does not already exist
  if (!dir.exists(dir_dest)) {
    dir.create(dir_dest, recursive = TRUE)
  }
  
  for(dir in c("sob","col","adm")){
    if (!dir.exists(paste0(dir_dest,"/",dir))){
      dir.create(paste0(dir_dest,"/",dir), recursive = TRUE)
    }
  }
  
  #-------------------------------------------------------------------------------
  # Summary of Business                                                        ####
  
  download_rma_web_data_files(
    years = 1999:as.numeric(format(Sys.Date(),"%Y")), 
    file_name = "sobtpu",
    dest = paste0(dir_dest,"/sob"))
  
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOBTPU_External_All_Years.pdf",
    destfile = paste0(dir_dest,"/sob/sobtpu_field_description_all_years.pdf"),mode= "wb",quiet    = TRUE)
  
  sobtpu_all <- list.files(paste0(dir_dest,"/sob"),pattern = "sobtpu",full.names = T)
  sobtpu_all <- sobtpu_all[!grepl("pdf|all",sobtpu_all)]
  sobtpu_all <- data.table::rbindlist(
    lapply(sobtpu_all,function(i){readRDS(i)}), fill = TRUE)
  saveRDS(sobtpu_all,paste0(dir_dest,"/sob/sobtpu_all.rds"));rm(sobtpu_all);gc()
  
  download_rma_web_data_files(
    years = 1989:as.numeric(format(Sys.Date(),"%Y")), 
    file_name = "sobcov",
    dest = paste0(dir_dest,"/sob"))
  
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOB_State_County_Crop_with_Coverage_Level_1989_Forward.pdf",
    destfile = paste0(dir_dest,"/sob/sobcov_field_description_1989_forward.pdf"),mode= "wb",quiet    = TRUE)
  
  sobcov_all <- list.files(paste0(dir_dest,"/sob"),pattern = "sobcov",full.names = T)
  sobcov_all <- sobcov_all[!grepl("pdf|all",sobcov_all)]
  sobcov_all <- data.table::rbindlist(
    lapply(sobcov_all,function(i){readRDS(i)}), fill = TRUE)
  saveRDS(sobcov_all,paste0(dir_dest,"/sob/sobcov_all.rds"))
  
  temp_zip <- tempfile(fileext = ".zip")
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/sobscc_1948_1988.zip",
    destfile = temp_zip,mode     = "wb",quiet    = TRUE)
  temp_txt <- tempfile()
  utils::unzip(zipfile = temp_zip, exdir = temp_txt)
  sobscc_1948_1988 <- utils::read.delim2(
    file= list.files(temp_txt, full.names = TRUE),sep= "|",header = FALSE,skipNul = TRUE)
  unlink(temp_zip)
  unlink(temp_txt, recursive = TRUE)
  colnames(sobscc_1948_1988) <- c(
    "commodity_year",
    "state_code",
    "state_abbreviation",
    "county_code",
    "county_name",
    "commodity_code",
    "commodity_name",
    "policies_sold_count",
    "policies_earning_premium_count",
    "policies_indemnified_count",
    "units_earning_premium_count",
    "units_indemnified_count",
    "net_reported_quantity",
    "liability_amount",
    "total_premium_amount",
    "subsidy_amount",
    "indemnity_amount",
    "loss_ratio")

  sobscc_1948_1988 <- as.data.table(sobscc_1948_1988)
  sobscc_1948_1988[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(sobscc_1948_1988))) := lapply(
    .SD, function(x) as.numeric(as.character(x))), 
    .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(sobscc_1948_1988))]
  saveRDS(sobscc_1948_1988,file=paste0(dir_dest,"/sob/sobscc_1948_1988.rds"))
  
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOB_State_County_Commodity_1948_1988.pdf",
    destfile = paste0(dir_dest,"/sob/sobscc_field_description.pdf"),mode= "wb",quiet    = TRUE)
  
  sobscc_all <- rbind(sobcov_all[
    , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
    by = c("commodity_year","state_code","state_abbreviation",
           "county_code","county_name","commodity_code","commodity_name"),
    .SDcols = c("policies_sold_count","policies_earning_premium_count","policies_indemnified_count",
                "units_earning_premium_count","units_indemnified_count","net_reported_quantity",
                "liability_amount","total_premium_amount","subsidy_amount","indemnity_amount")],
    sobscc_1948_1988[
      , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
      by = c("commodity_year","state_code","state_abbreviation",
             "county_code","county_name","commodity_code","commodity_name"),
      .SDcols = c("policies_sold_count","policies_earning_premium_count","policies_indemnified_count",
                  "units_earning_premium_count","units_indemnified_count","net_reported_quantity",
                  "liability_amount","total_premium_amount","subsidy_amount","indemnity_amount")])
  
  saveRDS(sobscc_all,file=paste0(dir_dest,"/sob/sobscc_all.rds"))

  #-------------------------------------------------------------------------------
  # Cause of Loss                                                              ####
  download_rma_web_data_files(
    years = 1989:as.numeric(format(Sys.Date(),"%Y")), 
    file_name = "colsom",
    dest = paste0(dir_dest,"/col"))
  
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/COL_Summary_of_Business_with_Month_All_Years.pdf",
    destfile = paste0(dir_dest,"/col/colsom_field_description.pdf"),mode= "wb",quiet    = TRUE)
  
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/Stage_Code_Listing.xlsx",
    destfile = paste0(dir_dest,"/col/stage_code_listing.xlsx"),mode= "wb",quiet    = TRUE)
  
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Miscellaneous_Files/cause_of_loss/prem_and_indem/col%20summary%20of%20business.pdf",
    destfile = paste0(dir_dest,"/col/col_sob_hist_field_description.pdf"),mode= "wb",quiet    = TRUE)
  
  temp_zip <- tempfile(fileext = ".zip")
  utils::download.file(
    "https://pubfs-rma.fpac.usda.gov/pub/Miscellaneous_Files/cause_of_loss/prem_and_indem/col_sob_hist.zip",
    destfile = temp_zip,mode     = "wb",quiet    = TRUE)
  temp_txt <- tempfile()
  utils::unzip(zipfile = temp_zip, exdir = temp_txt)
  data <- utils::read.delim2(
    file= list.files(temp_txt, full.names = TRUE),sep= "|",header = FALSE,skipNul = TRUE)
  unlink(temp_zip)
  unlink(temp_txt, recursive = TRUE)
  filed_names <-  c(
    "commodity_year",
    "state_code",
    "state_abbreviation",
    "county_code",
    "county_name",
    "commodity_code",
    "commodity_name",
    "insurance_plan_code",
    "insurance_plan_name_abbreviation",
    "coverage_category",
    "cause_of_loss_code",
    "cause_of_loss_description",
    "policies_earning_premium_count",
    "policies_indemnified_count",
    "net_reported_quantity",
    "liability_amount",
    "total_premium_amount",
    "subsidy_amount",
    "indemnity_amount",
    "loss_ratio")
  colnames(data) <- filed_names
  data <- data[filed_names]
  data <- as.data.table(data)
  data[, c(intersect(c(FCIP_FORCE_NUMERIC_KEYS,"indemnity_amount"), names(data))) := lapply(
    .SD, function(x) as.numeric(as.character(x))), 
    .SDcols = intersect(c(FCIP_FORCE_NUMERIC_KEYS,"indemnity_amount"), names(data))]
  
  data[,loss_ratio := indemnity_amount/total_premium_amount]
  data <- data[!liability_amount %in% c(0,NA,Inf,-Inf)]
  
  saveRDS(data,file=paste0(dir_dest,"/col/col_sob_hist.rds"))
  
  #-------------------------------------------------------------------------------
  # ADM                                                                        ####
  
  adm <- as.data.frame(
    data.table::rbindlist(
      lapply(
        list.files(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Output/base_rate/"),recursive = T,full.names = T),
        function(file){
          # file <- list.files(paste0(dr_adm,"Output/base_rate/"),recursive = T,full.names = T)[1]
          adm <- readRDS(file)
          adm$tau_adm <- adm$Rr + adm$Rf
          adm <- doBy::summaryBy(tau_adm~crop_yr + state_cd + county_cd + crop_cd,data=adm,FUN=mean,na.rm=T,keep.names = T)
          return(adm)
        }), fill = TRUE))
  
  saveRDS(standardize_fcip_column_names(adm),
          file=paste0(dir_dest,"/adm/fcip_demand_instruments_from_adm.rds"))
  
  price <- readRDS(file.path(farmpolicylab,
                             "rmaFCIPdata", "rmaPrices",
                             "Output", "ersFcipPrices.rds"))
  price$projecetd_price <- price$Price_Prj
  price$harvest_price <- price$Price_Hrvst
  price <- doBy::summaryBy(list(c("projecetd_price", "harvest_price"),c("state_cd","county_cd","typ_cd","crop_yr","crop_cd")),
                           data=price,FUN=mean,na.rm=T,keep.names = T)
  price <- standardize_fcip_column_names(price)
  
  saveRDS(price,file=paste0(dir_dest,"/adm/fcip_commodity_prices.rds"))
  
  # saveRDS(rmaADM:::clean_data(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/2025/2025_A01230_ContiguousCounty_YTD.rds"))),
  #         file=paste0(dir_dest,"/contiguous_county_adm.rds"))
  #-------------------------------------------------------------------------------
  
  return(c(list.files(paste0(dir_dest,"/sob")),
           list.files(paste0(dir_dest,"/col")),
           list.files(paste0(dir_dest,"/adm"))))
}


