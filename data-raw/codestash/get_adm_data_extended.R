
#' Read a single table from an Actuarial Data Master (ADM) YTD ZIP archive into a data.table
#'
#' This function extracts a `.txt` file matching `file_name` from an
#' Actuarial Data Master (ADM) year-to-date ZIP archive, parses it as a
#' pipe-delimited text file, applies cleaning steps, converts select
#' columns to numeric, and returns the result as a `data.table`.
#'
#' @param file_name   character(1)
#'   Base name (without `.txt`) of the ADM table to read. The function
#'   will look for a file named `paste0(file_name, ".txt")` in the ZIP.
#' @param adm_ytd_archive   character(1)
#'   Path to the ADM YTD ZIP archive (e.g. the package saves these as `adm_ytd_2019.zip`).
#' @family helpers
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
adm_ytd_reader <- function(file_name){







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
#' @family helpers
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


  # **Determine and prepare a clean cache directory for ADM files**
  dir_adm_compressed <- paste0(
    tools::R_user_dir("rmaADM", which = "cache"),
    "/adm_compressed")

  if (!dir.exists(dir_adm_compressed)) {
    dir.create(dir_adm_compressed, recursive = TRUE)
  }


  dest_file <- rmaADM:::list_data_assets()
  dest_file <- dest_file[grepl(paste0(year),dest_file)]
  dest_file <- dest_file[grepl(toupper(dataset),toupper(dest_file))]
  dest_file <- paste0(dir_adm_compressed,"/",dest_file)


  # **If we have not yet created the cleaned RDS, read & process the raw text**
  if (!file.exists(dest_file)) {
    df <- as.data.table(get_adm_data(year = year, dataset = dataset))

    data.table::setDT(df)

    ## COMPRESSION STRATS FROM HERE

    # **For certain tables, filter to only the `Y` reference and category == 1**
    if (unique(df[["record_type_code"]]) %in% c(
      "A01010", # Base Rate
      "A01040"  # Coverage Level Differential
    )){

      # - Keep only Yield rows if multiple reference_amount_code exist
      if (
        "reference_amount_code" %in% names(df) &&
        length(unique(df$reference_amount_code)) > 1
      ){
        df <- df[reference_amount_code == "Y"]
      }

      # - Keep only Base Rate rows if multiple record_category_code exist
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

    # **Remove metadata columns we do not need in analysis**
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

    # **Coerce specified keys to numeric (safely via as.character to as.numeric)**
    df[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))) := lapply(
      .SD, function(x) as.numeric(as.character(x))
    ), .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))]

    ## Recode unit_structure_code into broader classes
    if ("unit_structure_code" %in% names(df)) {
      # - Map various Optional Units to `OU`
      df[, unit_structure_code := ifelse(
        unit_structure_code %in% c("UD","UA","OU","", NA),
        "OU",
        unit_structure_code
      )]

      # - Map enterprise variants to `EU`
      df[, unit_structure_code := ifelse(
        unit_structure_code %in% c("EU","EP","EC"),
        "EU",
        unit_structure_code
      )]
    }

    ## Recode insurance_plan_code to harmonize similar products
    if ("insurance_plan_code" %in% names(df)) {
      # APH[90] to YP[1]
      df[, insurance_plan_code := ifelse(
        insurance_plan_code %in% c(1, 90),
        1,
        insurance_plan_code
      )]

      # CRC[44] to RP[2]
      df[, insurance_plan_code := ifelse(
        insurance_plan_code %in% c(44, 2),
        2,
        insurance_plan_code
      )]

      # IP[42], RP-HPE[3], 25 to RP-HPE[3]
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










  df[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))) := lapply(
    .SD, function(x) as.numeric(as.character(x))
  ), .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))]

  return(df)
}
