#' Prepare FCIP Data for Release
#'
#' @description
#' Downloads, processes, and caches multiple Federal Crop Insurance Program (FCIP)
#' datasets-Summary of Business (SOB), Cause of Loss (COL), and Actuarial Data Master (ADM)-
#' saving them as RDS files in a local cache directory structure.
#'
#' @param dir_dest Character. Base directory under which to store processed FCIP data.
#'   Defaults to `"data-raw/data_release"`. Subdirectories `sob`, `col`, and `adm`
#'   will be created within `dir_dest` if they do not already exist.
#'
#' @details
#' 1. **Determine source paths**  
#'    - On Windows, locates the `farmpolicylab` database under the user's OneDrive to Dropbox folder.  
#'    - On non-Windows, uses `~/Database/USA/USDA/`.  
#'
#' 2. **Setup local cache directories**  
#'    - Ensures `dir_dest` exists.  
#'    - Creates subfolders `sob`, `col`, and `adm` if missing.  
#'
#' 3. **Summary of Business (SOB)**  
#'    - Downloads yearly CSV ZIPs via `download_rma_web_data_files()` for `sobtpu`
#'      and `sobcov`, plus field description PDFs.  
#'    - Unzips and reads the historic 1948-1988 SOB data, coerces selected columns to
#'      numeric, and saves as `sobscc_1948_1988.rds`.  
#'
#' 4. **Cause of Loss (COL)**  
#'    - Downloads yearly CSV ZIPs for `colsom`, plus field description PDF and
#'      Excel code listing.  
#'    - Unzips and reads historic 1948-1988 COL data, coerces selected columns
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
  sobcov_all <- data.table::rbindlist(lapply(sobcov_all,function(i){readRDS(i)}), fill = TRUE)
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
  
  amount_variables <- c(
    "policies_sold_count","policies_earning_premium_count","policies_indemnified_count","units_earning_premium_count","units_indemnified_count",
    "net_reported_quantity","liability_amount","total_premium_amount","subsidy_amount","indemnity_amount","loss_ratio")
  sobscc_1948_1988[, c(intersect(amount_variables, names(sobscc_1948_1988))) := lapply(
    .SD, function(x) as.numeric(as.character(x))), 
    .SDcols = intersect(amount_variables, names(sobscc_1948_1988))]
  
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
  
  amount_variables <- c(
    "policies_earning_premium_count","policies_indemnified_count","net_reported_quantity","liability_amount",
    "total_premium_amount","subsidy_amount","indemnity_amount","loss_ratio")
  data[, c(intersect(amount_variables, names(data))) := lapply(
    .SD, function(x) as.numeric(as.character(x))), 
    .SDcols = intersect(amount_variables, names(data))]
  
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
  
  #-------------------------------------------------------------------------------
  
  return(c(list.files(paste0(dir_dest,"/sob")),
           list.files(paste0(dir_dest,"/col")),
           list.files(paste0(dir_dest,"/adm"))))
}