#' Build a mini ADM-YTD archive for unit tests
#'
#' Extracts a tiny, deterministic subset of the USDA actuarial “ADM Year-To-Date”
#' data for one crop year and bundles it into a single ZIP file that is safe to
#' ship with \pkg{testthat} fixtures.
#'
#' @param year           Integer. Crop year to extract (default \code{2020}).
#' @param dir_fastscratch Character. Directory that will temporarily hold the
#'   individual pipe-delimited \code{*.txt} tables **before** they are zipped
#'   (default \code{"./data-raw/fastscratch/testthat_adm"}). It is created if
#'   needed and emptied at the end.
#' @param dir_release    Character. Directory that will receive the finished
#'   \code{<year>_ADM_YTD.zip} (default
#'   \code{"./data-raw/data_release/testthat_adm"}). It is created if needed.
#'
#' @return (Invisibly) the full path to the ZIP file that was created.
#' @import readr
#' @importFrom utils unzip zip
#' @importFrom tools R_user_dir
#' @keywords internal
#' @export
build_adm_test_zip <- function(
    year            = 2020,
    dir_fastscratch = "./data-raw/fastscratch/testthat_adm",
    dir_release     = "./data-raw/data_release/testthat_adm"){
  
  ## ---- 1.  housekeeping ---------------------------------------------------
  dir.create(dir_fastscratch, recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(dir_release,"/",year), recursive = TRUE, showWarnings = FALSE)
  
  get_adm_ytd_archive(year)
  
  adm_ytd_archive <- file.path(
    tools::R_user_dir("USFarmSafetyNetLab", which = "cache"),
    sprintf("actuarial_data_master/archive/adm_ytd_%s.zip", year)
  )
  
  ## ---- 2.  tables we want --------------------------------------------------
  file_name_list <- c(
    "A01010_BaseRate",
    "A01040_CoverageLevelDifferential",
    "A00030_InsuranceOffer",
    "A01110_HistoricalRevenueCapping",
    "A00810_Price",
    "A01005_AreaRiskRate",
    "A01030_ComboRevenueFactor",
    "A00070_SubsidyPercent",
    "A01020_Beta",
    "A01130_AreaCoverageLevel",
    "A01135_AreaRate"
  )
  
  ## ---- 3.  extract-trim-write ---------------------------------------------
  all_files     <- utils::unzip(adm_ytd_archive, list = TRUE)$Name
  txt_out_files <- character(0)
  
  for (file_name in file_name_list) {
    
    match_idx   <- grepl(clean_file_name2(file_name),
                         clean_file_name2(all_files),
                         ignore.case = TRUE)
    target_file <- all_files[match_idx]
    if (!length(target_file)) {
      warning(sprintf("No match found for '%s'; skipping.", file_name))
      next
    }
    
    df <- readr::read_delim(
      unz(adm_ytd_archive, target_file[1]),
      delim = "|", col_names = TRUE, show_col_types = FALSE
    )
    
    ## ---- filters (as in your original script) ----------------------------
    if (!identical(file_name, "A00070_SubsidyPercent")) {
      if ("Commodity Code"      %in% names(df)) df <- df[as.numeric(df$`Commodity Code`)      %in% 41, ]
      if ("State Code"          %in% names(df)) df <- df[as.numeric(df$`State Code`)          %in% 19, ]
      if ("County Code"         %in% names(df)) df <- df[as.numeric(df$`County Code`)         %in% 1,  ]
      if ("Type Code"           %in% names(df)) df <- df[as.numeric(df$`Type Code`)           %in% 16, ]
      if ("Practice Code"       %in% names(df)) df <- df[as.numeric(df$`Practice Code`)       %in% 2,  ]
      if ("Unit Structure Code" %in% names(df)) df <- df[df$`Unit Structure Code` %in% c("BU", "OU"), ]
      if ("Insurance Plan Code" %in% names(df)) df <- df[as.numeric(df$`Insurance Plan Code`) %in% c(1:6, 16:17, 90), ]
    }
    
    if (identical(file_name, "A00030_InsuranceOffer")) {
      adm_insurance_offer_id <- unique(df$`ADM Insurance Offer ID`)
    }
    if (identical(file_name, "A01130_AreaCoverageLevel")) {
      df <- df[df$`ADM Insurance Offer ID` %in% adm_insurance_offer_id, ]
      area_rate_id <- unique(df$`Area Rate ID`)
    }
    if (identical(file_name, "A01135_AreaRate")) {
      df <- df[df$`Area Rate ID` %in% area_rate_id, ]
    }
    
    out_path <- file.path(dir_fastscratch, sprintf("%s.txt", file_name))
    readr::write_delim(df, out_path, delim = "|", na = "", col_names = TRUE)
    txt_out_files <- c(txt_out_files, out_path)
  }
  
  ## ---- 4.  zip and clean ---------------------------------------------------
  zip_path <- file.path(paste0(dir_release,"/",year), sprintf("%d_ADM_YTD.zip", year))
  if (length(txt_out_files)) {
    utils::zip(zipfile = zip_path, files = txt_out_files, flags = "-j")
  } else {
    warning("No tables survived the filters—zip not created.")
  }
  unlink(txt_out_files)
  
  
  layout_zip <- file.path(
    tools::R_user_dir("USFarmSafetyNetLab", which = "cache"),
    sprintf("actuarial_data_master/archive/layout_%s.zip", year)
  )
  
  file.copy(from=layout_zip, 
            to = paste0(dir_release,"/",year,"/ ADMLayout.zip"), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  
  invisible(zip_path)
}