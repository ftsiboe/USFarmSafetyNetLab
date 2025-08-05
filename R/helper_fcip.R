#' Download and Process RMA Web Data Files
#'
#' Fetches one or more years of USDA RMA Summary of Business data
#' (state-county-crop & livestock participation series), unzips,
#' reads the pipe-delimited text, applies the correct column names,
#' and saves each year as an \code{.rds} under \code{dest}.
#'
#' @param years Integer vector of crop or livestock years to fetch.
#' @param file_name Character; one of
#'   \code{"sobcov"}, \code{"sobtpu"}, \code{"lgm"}, \code{"lrp"}, or \code{"colsom"}.
#'   Determines both the subdirectory and the column schema.
#' @param dest Character path where the final \code{.rds} files will go.
#'   Defaults to \code{"./data-raw/data_release"}.
#' @param url_rma_ftp_file_access Base URL for USDA RMA web data.
#'   Defaults to the official RMA FTP file access root.
#' @return Invisibly returns \code{NULL}. Side effect: one \code{.rds}
#'   per year is written under \code{dest}, named \code{<file_name>_<year>.rds}.
#'   
#' @import data.table
#' @importFrom utils unzip download.file
#'  
#' @examples
#' \dontrun{
#' # Get sobtpu data for 2018-2022
#' download_rma_web_data_files(2018:2022, "sobtpu")
#' }
#' @export
download_rma_web_data_files <- function(
    years,
    file_name,
    dest = "./data-raw/data_release",
    url_rma_ftp_file_access = "https://pubfs-rma.fpac.usda.gov/pub"
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
    # State-county-crop data
    download_urls <- data.frame(
      url  = paste0(
        url_rma_ftp_file_access,
        "/Web_Data_Files/Summary_of_Business/state_county_crop/",
        file_name, "_", years, ".zip"
      ),
      year = years,
      stringsAsFactors = FALSE
    )
  }
  
  if (file_name %in% c("colsom")) {
    download_urls <- data.frame(
      url  = paste0(
        url_rma_ftp_file_access,
        "/Web_Data_Files/Summary_of_Business/cause_of_loss/",
        file_name, "_", years, ".zip"
      ),
      year = years,
      stringsAsFactors = FALSE
    )
  }
  
  if (file_name %in% c("col_indem","col_month","col_sob")) {
    download_urls <- data.frame(
      url  = paste0(
        url_rma_ftp_file_access,
        "/Miscellaneous_Files/cause_of_loss/",
        as.character(
          factor(
            file_name,
            levels = c("col_indem","col_month","col_sob"),
            labels = c("indem_only","indem_only_month","prem_and_indem"))),"/",
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
        url_rma_ftp_file_access,
        "/Web_Data_Files/Summary_of_Business/livestock_and_dairy_participation/",
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
        sep    = ifelse(file_name %in% "col_sob",",","|"),
        header = FALSE,
        skipNul = TRUE
      )
      
      # d) Clean up temp files immediately
      unlink(temp_zip)
      unlink(temp_txt, recursive = TRUE)
      
      # e) Apply the proper column names per `file_name`
      colnames(data) <- layouts_fcip[[file_name]]
      
      
      # if (file_name == "sobtpu") {
      #   colnames(data) <- c(
      #     "commodity_year",
      #     "state_code",
      #     "state_name",
      #     "state_abbreviation",
      #     "county_code",
      #     "county_name",
      #     "commodity_code",
      #     "commodity_name",
      #     "insurance_plan_code",
      #     "insurance_plan_abbreviation",
      #     "coverage_type_code",
      #     "coverage_level_percent",
      #     "delivery_id",
      #     "type_code",
      #     "type_name",
      #     "practice_code",
      #     "practice_name",
      #     "unit_structure_code",
      #     "unit_structure_name",
      #     "net_reporting_level_amount",
      #     "reporting_level_type",
      #     "liability_amount",
      #     "total_premium_amount",
      #     "subsidy_amount",
      #     "indemnity_amount",
      #     "loss_ratio",
      #     "endorsed_commodity_reporting_level_amount"
      #   )
      # }
      # 
      # if (file_name == "sobcov") {
      #   colnames(data) <- c(
      #     "commodity_year",
      #     "state_code",
      #     "state_abbreviation",
      #     "county_code",
      #     "county_name",
      #     "commodity_code",
      #     "commodity_name",
      #     "insurance_plan_code",
      #     "insurance_plan_abbreviation",
      #     "coverage_category",
      #     "delivery_type",
      #     "coverage_level_percent",
      #     "policies_sold_count",
      #     "policies_earning_premium_count",
      #     "policies_indemnified_count",
      #     "units_earning_premium_count",
      #     "units_indemnified_count",
      #     "quantity_type",
      #     "net_reported_quantity",
      #     "endorsed_companion_acres",
      #     "liability_amount",
      #     "total_premium_amount",
      #     "subsidy_amount",
      #     "state_private_subsidy",
      #     "additional_subsidy",
      #     "efa_premium_discount",
      #     "indemnity_amount",
      #     "loss_ratio"
      #   )
      # }
      # 
      # if (file_name == "colsom") {
      #   colnames(data) <- c(
      #     "commodity_year",
      #     "state_code",
      #     "state_abbreviation",
      #     "county_code",
      #     "county_name",
      #     "commodity_code",
      #     "commodity_name",
      #     "insurance_plan_code",
      #     "insurance_plan_name_abbreviation",
      #     "coverage_category",
      #     "stage_code",
      #     "cause_of_loss_code",
      #     "cause_of_loss_description",
      #     "month_of_loss",
      #     "month_of_loss_name",
      #     "year_of_loss",
      #     "policies_earning_premium",
      #     "policies_indemnified",
      #     "net_planted_quantity",
      #     "net_endorsed_acres",
      #     "liability",
      #     "total_premium",
      #     "producer_paid_premium",
      #     "subsidy",
      #     "state_private_subsidy",
      #     "additional_subsidy",
      #     "efa_premium_discount",
      #     "net_determined_quantity",
      #     "indemnity_amount",
      #     "loss_ratio"
      #   )
      # }
      # 
      # if (file_name == "lgm") {
      #   colnames(data) <- c(
      #     "reinsurance_year",
      #     "commodity_year",
      #     "location_state_code",
      #     "location_state_abbreviation",
      #     "location_county_code",
      #     "location_county_name",
      #     "commodity_code",
      #     "commodity_name",
      #     "insurance_plan_code",
      #     "insurance_plan_name",
      #     "type_code",
      #     "type_code_name",
      #     "practice_code",
      #     "practice_code_name",
      #     "sales_effective_date",
      #     paste0("target_marketings_", 1:11),
      #     paste0("corn_equivalent_", 2:11),
      #     paste0("soybean_meal_equivalent_", 2:11),
      #     "endorsements_earning_premium",
      #     "endorsements_indemnified",
      #     "deductible",
      #     "live_cattle_target_weight_quantity",
      #     "feeder_cattle_target_weight_quantity",
      #     "corn_target_weight_quantity",
      #     "liability_amount",
      #     "total_premium_amount",
      #     "subsidy_amount",
      #     "producer_premium_amount",
      #     "indemnity_amount"
      #   )
      # }
      # 
      # if (file_name == "lrp") {
      #   colnames(data) <- c(
      #     "reinsurance_year",
      #     "commodity_year",
      #     "location_state_code",
      #     "location_state_abbreviation",
      #     "location_county_code",
      #     "location_county_name",
      #     "commodity_code",
      #     "commodity_name",
      #     "insurance_plan_code",
      #     "insurance_plan_name",
      #     "type_code",
      #     "type_code_name",
      #     "practice_code",
      #     "practice_code_name",
      #     "sales_effective_date",
      #     "endorsement_length",
      #     "coverage_price",
      #     "expected_end_value",
      #     "coverage_level_percent",
      #     "rate",
      #     "cost_per_cwt",
      #     "end_date",
      #     "endorsements_earning_premium",
      #     "endorsements_indemnified",
      #     "net_number_of_head",
      #     "total_weight",
      #     "subsidy_amount",
      #     "total_premium_amount",
      #     "producer_premium_amount",
      #     "liability_amount",
      #     "indemnity_amount"
      #   )
      # }
      
      # f) Save the cleaned data to disk
      data <- data.table::as.data.table(data)
      data[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))) := lapply(
        .SD, function(x) as.numeric(as.character(x))), 
        .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))]
      
      # Ensure numeric columns are coerced to numeric
      amount_variables <- c(
        "net_reporting_level_amount","liability_amount","additional_subsidy","efa_premium_discount",
        "total_premium_amount","subsidy_amount","indemnity_amount","loss_ratio","endorsed_commodity_reporting_level_amount",
        "policies_sold_count","policies_earning_premium_count","policies_indemnified_count","units_earning_premium_count",
        "units_indemnified_count","net_reported_quantity","endorsed_companion_acres","state_private_subsidy"  )
      data[, c(intersect(amount_variables, names(data))) := lapply(
        .SD, function(x) as.numeric(as.character(x))), 
        .SDcols = intersect(amount_variables, names(data))]
      
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
#' Download the raw Summary of Business by Type, Practice, and Unit Structure (SOBTPU)
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
#'    - **Wheat (11):** `"DURUM"`, `"SPRING"`, or `"NO TYPE SPECIFIED"`, dropping `"FORAGE WHEAT FOR SEED"`.
#'    - **Barley (91):** `"SPRING"` or `"WINTER."`
#'    - **Rice (18):** `"SHORT"`, `"LONG"`, `"MEDIUM"`, or `"NO TYPE SPECIFIED"`, dropping certain japonica types.
#'    - **Other crops (41, 75, 94, 17, 81):** `"GRAIN"`, `"SILAGE"` (when `type_code == 26`), or "ALL".
#' 4. Summarizes total `liability_amount` by `(commodity_code, state_code, county_code, type_name)`.
#' 5. Pivots to wide form, with one column per `type_name`, filling missing with zero.
#' 6. Determines the single "dominant" type for each county-i.e. the one with positive liability
#'    when all other types are zero.
#' 7. Joins this `type_recode` back onto the full dataset and applies final cleanup:
#'    - If `type_name` is not `"NO TYPE SPECIFIED"`, it overrides the county-level rename.
#'    - Fills any remaining blanks or `NA` in `type_recode` with `"NO TYPE SPECIFIED"`.
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
#' * `data_source` (always "Summary of business by type, practice, and unit structure")
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
  
  # Define the codes we care about up-front
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
  
  # 5) Determine the one "dominant" type per county
  #    exclude "NO TYPE SPECIFIED" from consideration
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
#' This function looks for two columns-`unit_structure_code` and
#' `insurance_plan_code`-and creates recoded versions grouping similar
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
    # OU - Optional Unit;
    # UD - OU established by UDO; and
    # UA - OU established by a WUA.
    # BU - Basic Unit;
    # EU - Enterprise Unit;
    # EP - Enterprise Unit by Irrigated and/or Non-Irrigated Practices;
    # EC - Enterprise Unit by FAC and/or NFAC Cropping Practices;
    # WU - Whole-farm Unit;
    df[, unit_structure_recode := ifelse(
      unit_structure_code %in% c("UD","UA","OU","", NA),
      "OU",
      unit_structure_code
    )]
    
    # Map all enterprise variants to "EU"
    df[, unit_structure_recode := ifelse(
      unit_structure_code %in% c("EU","EP","EC","WU"),
      "EU",
      unit_structure_recode
    )]
    
    df[,unit_structure_rename := factor(
      unit_structure_recode,
      levels = c("OU","BU","EU"),
      labels = c("Optional Unit (OU)","Basic Unit (BU)","Enterprise/Whole Fram Unit (EU)"))]
  }
  
  ## Recode insurance plan codes to harmonize similar products
  if ("insurance_plan_code" %in% names(df)) {
    # Harmonize COMBO products
    # These three plans of insurance are similar but not identical. Some differences are:
    # * CRC bases the insurance guarantee on the higher of the base price or the harvest period price.
    # * IP and standard RA guarantees are determined using the base price, with no adjustment if the price increases.
    # * RA offers up-side price protection like that of CRC as an option but IP does not.
    # * IP limits unit formats to basic units, which include all interest in a crop in a county under identical ownership.
    # * RA is unique in offering coverage on whole farm units, integrating coverage from two to three crops.
    # check <- data[data$ins_plan_ab %in% c("YP","APH","IP","RP-HPE","RPHPE","CRC","RP","RA"),]
    
    # APH[90] to YP[1]
    df[, insurance_plan_recode := ifelse(
      insurance_plan_code %in% c(1, 90),
      1,
      insurance_plan_code
    )]
    
    # CRC[44] to RP[2]
    df[, insurance_plan_recode := ifelse(
      insurance_plan_code %in% c(44, 2),
      2,
      insurance_plan_recode
    )]
    
    # IP[42], RP-HPE[3], 25 to RP-HPE[3]
    df[, insurance_plan_recode := ifelse(
      insurance_plan_code %in% c(25, 42, 3),
      3,
      insurance_plan_recode
    )]
  }
  
  return(df)
}




