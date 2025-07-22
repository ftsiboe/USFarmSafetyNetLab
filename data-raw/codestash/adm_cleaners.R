
# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")

#' Download and archive ADM year-to-date data
#'
#' @description
#' Downloads raw ADM (Actuarial Data Master) "year-to-date" files for the specified years
#' and archives them in the package cache directory.
#'
#' @param years
#'   Integer vector of policy years to retrieve (e.g. 2011:2025).
#'   Defaults to 2011 through the current year.
#'
#' @details
#' - Determines the archive directory via
#'   `tools::R_user_dir("USFarmSafetyNetLab", "cache")/actuarial_data_master/archive`.
#'   Creates it if it does not exist.
#' - Downloads raw ADM files into a temporary directory using
#'   `rmaADM:::download_adm(..., helpers_only=TRUE, keep_source_files=TRUE)`.
#' - Copies all files matching `adm_ytd_` from the temp dir into the archive,
#'   overwriting any existing files.
#'
#' @return
#'   none.
#'
#' @importFrom tools R_user_dir
#' @export
#' @keywords internal
get_adm_ytd_archive <- function(
    years = 2011:as.numeric(format(Sys.Date(), "%Y"))){

  # Path to the ADM cache-archive directory if dir is not provided
  dir <- paste0(
    tools::R_user_dir("USFarmSafetyNetLab", which = "cache"),
    "/actuarial_data_master/archive")

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  lapply(
    years,
    function(year){

      # get the list of files in the directory
      files <- list.files(dir, full.names = TRUE,pattern = paste0("adm_ytd_",year,".zip"))

      # if there are no files, set last_modified to NULL
      if (length(files) == 0) {
        last_modified <- NULL
      }else{
        # get the time when the most recent file was last modified
        last_modified <- file.info(list.files(dir, full.names = TRUE,pattern = paste0("adm_ytd_",year,".zip")))$mtime
      }

      urls <- rmaADM:::locate_download_link(year = year)

      # check if the update date is greater than the last modified date
      skip = F
      if (!is.null(last_modified) && urls$update_date < last_modified) {
        cli::cli_alert_info(paste0("The data for ",year," is already up to date. Skipping download."))
        skip = T
      }

      if(skip == F){

        utils::download.file(urls$data,
                             destfile=paste0(dir,"/adm_ytd_",year,".zip"),
                             mode="wb")

        utils::download.file(urls$layout,
                             destfile=paste0(dir,"/layout_",year,".zip"),
                             mode="wb")
      }

      invisible(year)})

  invisible(years)
}

#' Clean the file names
#'
#' @param file_names the file path of the file name to clean
#'
#' @returns a version of the file name that is cleaned (i.e. snake case, .rds suffix, no extraneous information)
#'
#' @examples \dontrun{clean_file_name("2012_A01100_YieldAndTyield_YTD.txt")}
clean_file_name2 <- function(file_names){
  # file_names <- df
  # remove the first 4 digits from the suffix if the first 4 digits are numeric digits
  file_names <- gsub("^[0-9]{4}", "", file_names)

  # remove "YTD"
  file_names <- gsub("YTD", "", file_names)

  # remove "_" unless "_" follows a number
  file_names <- gsub("(?<![0-9])_(?![0-9])", "", file_names, perl = TRUE)

  # convert from camel case to snake case
  file_names <- gsub("([a-z])([A-Z])", "\\1_\\2", file_names)
  file_names <- tolower(file_names)

  # return the file name
  return(file_names)

}


#' Read a single table from an Actuarial Data Master (ADM) YTD ZIP archive into a data.table
#'
#' This function extracts a “.txt” file matching `file_name` from an
#' Actuarial Data Master (ADM) year-to-date ZIP archive, parses it as a
#' pipe-delimited text file, applies cleaning steps, converts select
#' columns to numeric, and returns the result as a `data.table`.
#'
#' @param file_name   character(1)
#'   Base name (without “.txt”) of the ADM table to read. The function
#'   will look for a file named `paste0(file_name, ".txt")` in the ZIP.
#' @param adm_ytd_archive   character(1)
#'   Path to the ADM YTD ZIP archive (e.g. the package saves these as “adm_ytd_2019.zip”).
#'
#' @return
#' A data.table containing the parsed and cleaned contents of the
#' specified ADM table. Columns whose names match the global (or package)
#' vector FCIP_FORCE_NUMERIC_KEYS will be coerced to numeric.
#' @import data.table
#' @importFrom utils unzip
#' @importFrom readr read_delim
#' @importFrom data.table setDT
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- adm_ytd_reader(file_name="A01010_BaseRate",
#' adm_ytd_archive = "./data-raw/database/actuarial_data_master/archive/2019/adm_ytd_2019.zip")
#' # Inspect first few rows
#' head(dt)
#' }
adm_ytd_reader <- function(file_name, adm_ytd_archive){

  # **Determine and prepare a clean cache directory for ADM files**
  dir_adm_clean <- paste0(
    tools::R_user_dir("USFarmSafetyNetLab", which = "cache"),
    "/actuarial_data_master/clean")
  if (!dir.exists(dir_adm_clean)) {
    dir.create(dir_adm_clean, recursive = TRUE)
  }

  # **List all files inside the ZIP archive without extracting**
  all_files <- utils::unzip(adm_ytd_archive, list = TRUE)$Name

  # **Locate the specific entry matching the requested file_name**
  match_idx   <- grepl(clean_file_name2(file_name), clean_file_name2(all_files))
  target_file <- all_files[match_idx]
  if (length(target_file) == 0) {
    stop(
      sprintf("File '%s' not found in archive '%s'",
              file_name, adm_ytd_archive),
      call. = FALSE
    )
  }

  # **Set the path for the RDS output, replacing "YTD.txt" → "YTD.rds"**
  dest_file <- file.path(
    dir_adm_clean,
    gsub("YTD.txt", "YTD.rds", target_file)
  )

  # **If we haven’t yet created the cleaned RDS, read & process the raw text**
  if (!file.exists(dest_file)) {

    # — Read the pipe-delimited text directly from the ZIP
    df <- readr::read_delim(
      unz(adm_ytd_archive, target_file),
      delim = "|",
      col_names = TRUE,
      show_col_types = FALSE
    )
    # — Apply your project’s cleaning routine
    df <- rmaADM:::clean_data(df)

    # — Convert to data.table in place
    data.table::setDT(df)

    ## COMPRESSION STRATS FROM HERE

    # **For certain tables, filter to only the “Y” reference and category == 1**
    if (unique(df[["record_type_code"]]) %in% c(
      "A01010", # Base Rate
      "A01040"  # Coverage Level Differential
    )){

      # — Keep only Yield rows if multiple reference_amount_code exist
      if (
        "reference_amount_code" %in% names(df) &&
        length(unique(df$reference_amount_code)) > 1
      ){
        df <- df[reference_amount_code == "Y"]
      }

      # — Keep only Base Rate rows if multiple record_category_code exist
      if (
        "record_category_code" %in% names(df) &&
        length(unique(df$record_category_code)) > 1
      ){
        df <- df[record_category_code == 1]
      }
    }

    # **Drop the reference_amount_code column if has only on level**
    if (
      "reference_amount_code" %in% names(df) &&
      length(unique(df$reference_amount_code)) %in% 1
    ){
      df <- df[, setdiff(names(df), c("reference_amount_code")), with = FALSE]
    }

    # **Drop the record_category_code column if it has only on level**
    if (
      "record_category_code" %in% names(df) &&
      length(unique(df$record_category_code)) %in% 1
    ){
      df <- df[, setdiff(names(df), c("record_category_code")), with = FALSE]
    }

    # **Remove metadata columns we don’t need in analysis**
    df <- df[, setdiff(
      names(df),
      c("record_type_code",
        "last_released_date",
        "released_date",
        "deleted_date",
        "filing_date")
    ), with = FALSE]

    # **Drop any column composed entirely of NAs**
    df <- df[, names(df)[colSums(is.na(df)) < nrow(df)], with = FALSE]

    # **Coerce specified keys to numeric (safely via as.character → as.numeric)**
    df[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))) := lapply(
      .SD, function(x) as.numeric(as.character(x))
    ), .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))]

    ## Recode unit_structure_code into broader classes
    if ("unit_structure_code" %in% names(df)) {
      # — Map various Optional Units to “OU”
      df[, unit_structure_code := ifelse(
        unit_structure_code %in% c("UD","UA","OU","", NA),
        "OU",
        unit_structure_code
      )]

      # — Map enterprise variants to “EU”
      df[, unit_structure_code := ifelse(
        unit_structure_code %in% c("EU","EP","EC"),
        "EU",
        unit_structure_code
      )]
    }

    ## Recode insurance_plan_code to harmonize similar products
    if ("insurance_plan_code" %in% names(df)) {
      # APH[90] → YP[1]
      df[, insurance_plan_code := ifelse(
        insurance_plan_code %in% c(1, 90),
        1,
        insurance_plan_code
      )]

      # CRC[44] → RP[2]
      df[, insurance_plan_code := ifelse(
        insurance_plan_code %in% c(44, 2),
        2,
        insurance_plan_code
      )]

      # IP[42], RP-HPE[3], 25 → RP-HPE[3]
      df[, insurance_plan_code := ifelse(
        insurance_plan_code %in% c(25, 42, 3),
        3,
        insurance_plan_code
      )]
    }

    # **Save the cleaned & compressed table for future fast loads**
    saveRDS(df, file = dest_file)

  } else {
    # **Otherwise just load the pre-saved RDS**
    df <- readRDS(dest_file)
  }

  # **Return the final data.table**
  return(df)
}


#' Fetch and extend ADM data for analysis
#'
#' @description
#' Retrieves ADM data via \code{rmaADM::get_adm_data()}, coerces key columns to numeric,
#' and harmonizes recoding of unit structure and insurance plan codes for downstream analysis.
#'
#' @param year Integer or character. Year(s) to fetch. Passed through to \code{get_adm_data()}.
#'   Defaults to \code{NULL}.
#' @param dataset Character. Name of the ADM dataset to retrieve (e.g., \code{'baserate'}).
#'   Defaults to \code{'baserate'}.
#'
#' @return
#' A \code{data.table} containing the requested ADM data with numeric conversions
#'   and recoded \code{unit_structure_code} and \code{insurance_plan_code}.
#'
#' @examples
#' \dontrun{
#' dt <- get_adm_data_extended(year = 2020, dataset = 'baserate')
#' }
#'
#' @import data.table
#' @importFrom rmaADM get_adm_data
#' @export
get_adm_data_extended <- function(year = NULL, dataset = "baserate"){

  # year <- 2011;dataset<-"A01010_BaseRate"
  # get_adm_ytd_archive(year)

  if(dataset %in% "county_yield_history"){
    df <- as.data.table(get_adm_data(year = year, dataset = dataset))

    df[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))) := lapply(
      .SD, function(x) as.numeric(as.character(x))
    ), .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))]
  }else{
    #df <- as.data.table(get_adm_data(year = year, dataset = dataset))

    df <- adm_ytd_reader(file_name=paste0(year,"_",dataset, "_YTD.txt"),
                         adm_ytd_archive = paste0(tools::R_user_dir("USFarmSafetyNetLab", which = "cache"),
                                                  "/actuarial_data_master/archive/adm_ytd_",year,".zip"))
  }

  return(df)
}

