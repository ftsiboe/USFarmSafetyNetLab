#-------------------------------------------------------------------------------
# Preliminaries                                                              ####
rm(list = ls(all = TRUE))

# usethis::edit_r_environ()

# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")

# Clean dn deply HISTORICAL SUMMARY OF BUSINESS and CAUSE OF LOSS DATA as a data release
devtools::document()

if(Sys.info()['sysname'] %in% "Windows"){
  farmpolicylab <- paste0(gsub("OneDrive","Dropbox",Sys.getenv("OneDriveConsumer")),"/farmpolicylab/database/")
}else{
  farmpolicylab <- paste0("~/Database/USA/USDA/")
}

# Directory to store cached calibrations
dir_data_release <- "data-raw/data_release"

# Create the cache directory if it does not already exist
if (!dir.exists(dir_data_release)) {
  dir.create(dir_data_release, recursive = TRUE)
}
#-------------------------------------------------------------------------------
# Summary of Business                                                        ####
download_rma_web_data_files(years=1999:as.numeric(format(Sys.Date(),"%Y")), file_name="sobtpu")
utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOBTPU_External_All_Years.pdf",
  destfile = "./data-raw/data_release/sobtpu_field_description_all_years.pdf",mode= "wb",quiet    = TRUE)

download_rma_web_data_files(1989:as.numeric(format(Sys.Date(),"%Y")), "sobcov")
utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOB_State_County_Crop_with_Coverage_Level_1989_Forward.pdf",
  destfile = "./data-raw/data_release/sobcov_field_description_1989_forward.pdf",mode= "wb",quiet    = TRUE)

temp_zip <- tempfile(fileext = ".zip")
utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/sobscc_1948_1988.zip",
  destfile = temp_zip,mode     = "wb",quiet    = TRUE)
temp_txt <- tempfile()
utils::unzip(zipfile = temp_zip, exdir = temp_txt)
data <- utils::read.delim2(
  file= list.files(temp_txt, full.names = TRUE),sep= "|",header = FALSE,skipNul = TRUE)
unlink(temp_zip)
unlink(temp_txt, recursive = TRUE)
colnames(data) <- c(
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
data <- as.data.table(data)
data[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))) := lapply(
  .SD, function(x) as.numeric(as.character(x))), 
  .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))]
saveRDS(data,file=paste0(dir_data_release,"/sobscc_1948_1988.rds"))
utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOB_State_County_Commodity_1948_1988.pdf",
  destfile = "./data-raw/data_release/sobscc_field_description.pdf",mode= "wb",quiet    = TRUE)

tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "SOB")
}, error = function(e){NULL})
piggyback::pb_release_create(
  repo = "ftsiboe/US-FarmSafetyNet-Lab", 
  tag = "SOB",
  name = "Summary of Business",
  body = paste("Summary of Business data breaks out FCIP participation at variaous levels:",
               paste0("**sobtpu** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their contract choice (i), the insurance pool they selected (j), ",
                      "and the crop year (t). Contract choices combine insurance plan (e.g., APH, RP), ",
                      "coverage level, and unit structure (e.g., Optional [OU], Enterprise [EU]). ",
                      "Pools are the most granular rate‐setting level and are distinguished by county, commodity, ",
                      "crop type, and practice (e.g., irrigated, organic).", collapse = ""),
               paste0("**sobcov** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their coverage level, county, commodity, and  commodity year.", collapse = ""),
               paste0("**sobscc** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their county, commodity, and  commodity year.", collapse = ""),sep = "\n\n"))
# piggyback::pb_new_release( repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "SOB")
piggyback::pb_upload(
  c(list.files(dir_data_release, full.names = TRUE, recursive = TRUE,pattern = "sobtpu"),
    list.files(dir_data_release, full.names = TRUE, recursive = TRUE,pattern = "sobcov"),
    list.files(dir_data_release, full.names = TRUE, recursive = TRUE,pattern = "sobscc")),
  repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "SOB",overwrite = TRUE)

#-------------------------------------------------------------------------------
# Cause of Loss                                                              ####
download_rma_web_data_files(1989:as.numeric(format(Sys.Date(),"%Y")), "colsom")

utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/COL_Summary_of_Business_with_Month_All_Years.pdf",
  destfile = "./data-raw/data_release/colsom_field_description.pdf",mode= "wb",quiet    = TRUE)

utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/Stage_Code_Listing.xlsx",
  destfile = "./data-raw/data_release/stage_code_listing.xlsx",mode= "wb",quiet    = TRUE)

utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Miscellaneous_Files/cause_of_loss/prem_and_indem/col%20summary%20of%20business.pdf",
  destfile = "./data-raw/data_release/col_sob_hist_field_description.pdf",mode= "wb",quiet    = TRUE)

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

saveRDS(data,file=paste0(dir_data_release,"/col_sob_hist.rds"))

tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "COL")
}, error = function(e){NULL})
piggyback::pb_release_create(
  repo = "ftsiboe/US-FarmSafetyNet-Lab", 
  tag  = "COL",
  name = "Cause of Loss",
  body = "Cause of Loss breaks out FCIP participation by peril")
# piggyback::pb_new_release( repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "COL")
piggyback::pb_upload(
  c(list.files(dir_data_release, full.names = TRUE, recursive = TRUE,pattern = "colsom"),
    list.files(dir_data_release, full.names = TRUE, recursive = TRUE,pattern = "col_sob_hist"),
    list.files(dir_data_release, full.names = TRUE, recursive = TRUE,pattern = "stage_code_listing")),
  repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "COL",overwrite = TRUE)

#-------------------------------------------------------------------------------
# Miscellaneous                                                              ####
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
        file=paste0(dir_data_release,"/fcip_demand_instrumental_from_adm.rds"))

saveRDS(rmaADM:::clean_data(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/2025/2025_A01230_ContiguousCounty_YTD.rds"))),
        file=paste0(dir_data_release,"/contiguous_county_adm.rds"))

tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/US-FarmSafetyNet-Lab", tag = "ADM")
}, error = function(e){NULL})

piggyback::pb_release_create(
  repo = "ftsiboe/US-FarmSafetyNet-Lab", 
  tag = "ADM",
  name = "Actuarial Data Master",
  body = "Various items aggregated from the FCIP's Actuarial Data Master")
# piggyback::pb_new_release( repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "ADM")
piggyback::pb_upload(
  list.files(dir_data_release, full.names = TRUE, recursive = TRUE,pattern = "_adm"),
  repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "ADM",overwrite = TRUE)

# piggyback::pb_upload(
#   list.files(paste0(tools::R_user_dir("rFarmPolicySim", which = "cache"),"/actuarial_data_master/archive/"), 
#              full.names = TRUE, recursive = TRUE),
#   repo = "ftsiboe/US-FarmSafetyNet-Lab", tag  = "ADM",overwrite = TRUE)

#-------------------------------------------------------------------------------


# Livestock Gross Margin – Summary of Business Data 
# download_rma_web_data_files(2003:as.numeric(format(Sys.Date(),"%Y")), "lgm")

# Livestock Risk Protection – Summary of Business Data
# download_rma_web_data_files(2003:as.numeric(format(Sys.Date(),"%Y")), "lrp")

