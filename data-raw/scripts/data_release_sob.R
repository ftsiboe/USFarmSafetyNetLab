
source("data-raw/scripts/environment_setup.R")

devtools::document()

## 1. Summary of business by State/County/Crop/Type/Practice/Unit Structure/Insurance Plan/Coverage Category/Coverage Level
download_rma_web_data_files(
  years = 1999:as.numeric(format(Sys.Date(),"%Y")), 
  file_name = "sobtpu",
  dest = paste0(dir_data_release,"/sob"))

utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOBTPU_External_All_Years.pdf",
  destfile = paste0(dir_data_release,"/sob/sobtpu_field_description_all_years.pdf"),mode= "wb",quiet    = TRUE)

sobtpu_all <- list.files(paste0(dir_data_release,"/sob"),pattern = "sobtpu",full.names = T)
sobtpu_all <- sobtpu_all[!grepl("pdf|all",sobtpu_all)]
sobtpu_all <- data.table::rbindlist(
  lapply(sobtpu_all,function(i){readRDS(i)}), fill = TRUE)
saveRDS(sobtpu_all,paste0(dir_data_release,"/sob/sobtpu_all.rds"));rm(sobtpu_all);gc()

## 2. Summary of business by State/County/Crop/Insurance Plan/Coverage Category/Coverage Level
download_rma_web_data_files(
  years = 1989:as.numeric(format(Sys.Date(),"%Y")), 
  file_name = "sobcov",
  dest = paste0(dir_data_release,"/sob"))

utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOB_State_County_Crop_with_Coverage_Level_1989_Forward.pdf",
  destfile = paste0(dir_data_release,"/sob/sobcov_field_description_1989_forward.pdf"),mode= "wb",quiet    = TRUE)

sobcov_all <- list.files(paste0(dir_data_release,"/sob"),pattern = "sobcov",full.names = T)
sobcov_all <- sobcov_all[!grepl("pdf|all",sobcov_all)]
sobcov_all <- data.table::rbindlist(lapply(sobcov_all,function(i){readRDS(i)}), fill = TRUE)
saveRDS(sobcov_all,paste0(dir_data_release,"/sob/sobcov_all.rds"))

## 3. Summary of business by State/County/Crop/Insurance Plan/Coverage Category
# sobsccCat_1980_1988 <-  data.table::rbindlist(
#   lapply(
#     1980:1988,
#     function(year){
#       temp_txt <- tempfile()
#       utils::unzip(zipfile = paste0(farmpolicylab,"rmaFCIPdata/rmaSumOfBussiness/Archive/SOBSCC/sobscc_",year,".zip"), exdir = temp_txt)
#       sobsccCat <- utils::read.delim2(
#         file= list.files(temp_txt, full.names = TRUE),sep= "|",header = FALSE,skipNul = TRUE)
#       unlink(temp_txt, recursive = TRUE)
#       
#       colnames(sobsccCat) <- c(
#         "commodity_year",
#         "state_code",
#         "state_abbreviation",
#         "county_code",
#         "county_name",
#         "commodity_code",
#         "commodity_name",
#         "insurance_plan_code",
#         "insurance_plan_abbreviation",
#         "coverage_type_code",
#         "policies_sold_count",
#         "policies_earning_premium_count",
#         "policies_indemnified_count",
#         "units_earning_premium_count",
#         "units_indemnified_count",
#         "net_reported_quantity",
#         "liability_amount",
#         "total_premium_amount",
#         "subsidy_amount",
#         "indemnity_amount",
#         "loss_ratio")
#       return(sobsccCat)
#     }),fill = TRUE)
# 
# sobsccCat_1980_1988 <- as.data.table(sobsccCat_1980_1988)
# sobsccCat_1980_1988[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(sobsccCat_1980_1988))) := lapply(
#   .SD, function(x) as.numeric(as.character(x))), 
#   .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(sobsccCat_1980_1988))]
# 
# sobsccCat_1980_1988[, c(intersect(amount_variables, names(sobsccCat_1980_1988))) := lapply(
#   .SD, function(x) as.numeric(as.character(x))), 
#   .SDcols = intersect(amount_variables, names(sobsccCat_1980_1988))]
# 
# saveRDS(sobsccCat_1980_1988,paste0(dir_data_release,"/sob/sobsccCat_1980_1988.rds"))
# 
# file.copy(from=paste0(farmpolicylab,"rmaFCIPdata/rmaSumOfBussiness/Archive/SOBSCC/sobscc_1980-1988.pdf"), 
#           to = paste0(dir_data_release,"/sob/sobsccCat_1980-1988.pdf"), 
#           overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)

## 4. Summary of business by State/County/Crop
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

sobscc_1948_1988 <- data.table::as.data.table(sobscc_1948_1988)
sobscc_1948_1988[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(sobscc_1948_1988))) := lapply(
  .SD, function(x) as.numeric(as.character(x))), 
  .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(sobscc_1948_1988))]

sobscc_1948_1988[, c(intersect(FCIP_FORCE_CHARACTER_KEYS, names(sobscc_1948_1988))) := lapply(
  .SD, function(x) trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(as.character(x)))), which = c("both"))), 
  .SDcols = intersect(FCIP_FORCE_CHARACTER_KEYS, names(sobscc_1948_1988))]

sobscc_1948_1988[, c(intersect(FCIP_FORCE_AMOUNT_VARIABLES, names(sobscc_1948_1988))) := lapply(
  .SD, function(x) as.numeric(as.character(x))), 
  .SDcols = intersect(FCIP_FORCE_AMOUNT_VARIABLES, names(sobscc_1948_1988))]

saveRDS(sobscc_1948_1988,file=paste0(dir_data_release,"/sob/sobscc_1948_1988.rds"))

utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/SOB_State_County_Commodity_1948_1988.pdf",
  destfile = paste0(dir_data_release,"/sob/sobscc_field_description.pdf"),mode= "wb",quiet    = TRUE)

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
saveRDS(sobscc_all,file=paste0(dir_data_release,"/sob/sobscc_all.rds"))

# Send Summary of Business to Github
#  tryCatch({
#   piggyback::pb_release_delete(repo = "ftsiboe/USFarmSafetyNetLab", tag = "sob")
# }, error = function(e){NULL})
# 
# piggyback::pb_release_create(  
#   repo = "ftsiboe/USFarmSafetyNetLab", 
#   tag = "sob",
#   name = "Summary of Business",
#   body = paste("Summary of Business data breaks out FCIP participation at variaous levels:",
#                paste0("**sobtpu** aggregates loss experience for groups of producers who are ",
#                       "similarly defined by their contract choice (i), the insurance pool they selected (j), ",
#                       "and the crop year (t). Contract choices combine insurance plan (e.g., APH, RP), ",
#                       "coverage level, and unit structure (e.g., Optional [OU], Enterprise [EU]). ",
#                       "Pools are the most granular rate‐setting level and are distinguished by county, commodity, ",
#                       "crop type, and practice (e.g., irrigated, organic).", collapse = ""),
#                paste0("**sobcov** aggregates loss experience for groups of producers who are ",
#                       "similarly defined by their coverage level, county, commodity, and  commodity year.", collapse = ""),
#                paste0("**sobscc** aggregates loss experience for groups of producers who are ",
#                       "similarly defined by their county, commodity, and  commodity year.", collapse = ""),sep = "\n\n"))
#
#piggyback::pb_new_release( repo = "ftsiboe/USFarmSafetyNetLab", tag  = "sob")

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/sob"), full.names = TRUE, recursive = TRUE,pattern = "sobtpu"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "sob",overwrite = TRUE)

Sys.sleep(60)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/sob"), full.names = TRUE, recursive = TRUE,pattern = "sobscc"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "sob",overwrite = TRUE)

Sys.sleep(60)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/sob"), full.names = TRUE, recursive = TRUE,pattern = "sobcov"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "sob",overwrite = TRUE)

