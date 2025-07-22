
rm(list = ls(all = TRUE)); gc()

devtools::document()

# 1.House-keeping & archive location
if (!dir.exists("./data-raw/fastscratch")) {
  dir.create("./data-raw/fastscratch", recursive = TRUE)
}

if (!dir.exists("./data-raw/testthat_adm")) {
  dir.create("./data-raw/testthat_adm", recursive = TRUE)
}

for(year in c(2020,2016)){
  get_adm_ytd_archive(year)

  adm_ytd_archive <- file.path(
    tools::R_user_dir("rfcipCalcPass", which = "cache"),
    sprintf("actuarial_data_master/archive/adm_ytd_%s.zip", year)
  )

  # 2. ADM Files to pull out of the archive
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
    "A01135_AreaRate")

  # 3.  Loop, filter, write
  all_files <- utils::unzip(adm_ytd_archive, list = TRUE)$Name
  txt_out_files <- character(0)   # keep track of outputs we create

  for(file_name in file_name_list){

    # file_name <- "A01130_AreaCoverageLevel"

    ## 4a.  Find the matching file inside the zip
    match_idx   <- grepl(clean_file_name2(file_name),
                         clean_file_name2(all_files),
                         ignore.case = TRUE)
    target_file <- all_files[match_idx]

    if (length(target_file) == 0) {
      warning(sprintf("No match found in archive for '%s'; skipping.", file_name))
      next
    }

    ## 4b.  Read, filter
    df <- readr::read_delim(
      unz(adm_ytd_archive, target_file[1]),  # use first match if >1
      delim       = "|",
      col_names   = TRUE,
      show_col_types = FALSE
    )

    if(!file_name %in% "A00070_SubsidyPercent"){

      if("Commodity Code" %in% names(df)) {
        df <- df[as.numeric(as.character(df$`Commodity Code`)) %in% 41, ]
      }

      if("State Code" %in% names(df)){
        df <- df[as.numeric(as.character(df$`State Code`)) %in% 19, ]
      }

      if("County Code" %in% names(df)) {
        df <- df[as.numeric(as.character(df$`County Code`)) %in% 1, ]
      }

      if("Type Code" %in% names(df)) {
        df <- df[as.numeric(as.character(df$`Type Code`)) %in% 16, ]
      }

      if("Practice Code" %in% names(df)) {
        df <- df[as.numeric(as.character(df$`Practice Code`)) %in% 2, ]
      }

      if("Unit Structure Code" %in% names(df)) {
        df <- df[df$`Practice Code` %in% c("BU","OU"), ]
      }

      if("Insurance Plan Code" %in% names(df)) {
        df <- df[as.numeric(as.character(df$`Insurance Plan Code`)) %in% c(1:6,16:17,90), ]
      }

    }

    if(file_name %in% "A00030_InsuranceOffer"){
      adm_insurance_offer_id <- unique(df$`ADM Insurance Offer ID`)
    }

    if(file_name %in% "A01130_AreaCoverageLevel"){
      df <- df[df$`ADM Insurance Offer ID` %in% adm_insurance_offer_id,]
      area_rate_id <- unique(df$`Area Rate ID`)
    }

    if(file_name %in% "A01135_AreaRate"){
      df <- df[df$`Area Rate ID` %in% area_rate_id,]
    }

    ## 4c.  Write out as pipe-delimited .txt
    out_path <- file.path("./data-raw/fastscratch",sprintf("%s.txt", file_name))
    readr::write_delim(df, out_path, delim = "|", na = "", col_names = TRUE)
    txt_out_files <- c(txt_out_files, out_path)
  }

  # 4. Zip every txt we just created (no sub-folders inside the zip)
  zip_path <- paste0("./data-raw/testthat_adm/",year,"_ADM_YTD.zip")
  ## -j (“junk paths”) strips directories so only bare file names appear inside
  utils::zip(zipfile = zip_path, files = txt_out_files, flags = "-j")

  unlink(list.files("./data-raw/fastscratch",pattern = ".txt",full.names = T))
}


adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/"



