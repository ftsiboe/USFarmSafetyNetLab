
source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

# Cause of Loss Summary of Business [1989-current]
download_rma_web_data_files(
  years = 1989:as.numeric(format(Sys.Date(),"%Y")), 
  file_name = "colsom",
  dest = paste0(dir_data_release,"/col"))

colsom_all <- list.files(paste0(dir_data_release,"/col"),pattern = "colsom",full.names = T)
colsom_all <- colsom_all[!grepl("pdf|all",colsom_all)]
colsom_all <- data.table::rbindlist(
  lapply(colsom_all,function(i){readRDS(i)}), fill = TRUE)
saveRDS(colsom_all,paste0(dir_data_release,"/col/colsom_all.rds"));rm(colsom_all);gc()

utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/COL_Summary_of_Business_with_Month_All_Years.pdf",
  destfile = paste0(dir_data_release,"/col/colsom_field_description.pdf"),mode= "wb",quiet    = TRUE)


# # Cause of Loss Indemnities Only 
# download_rma_web_data_files(
#   years = 1989:2009,
#   file_name = "col_indem",
#   dest = paste0(dir_data_release,"/col"))
# 
# utils::download.file(
#   "https://pubfs-rma.fpac.usda.gov/pub/Miscellaneous_Files/cause_of_loss/indem_only/col%20indemnities%20only.pdf",
#   destfile = paste0(dir_data_release,"/col/col_indemnities_only.pdf"),mode= "wb",quiet    = TRUE)
# 
# # Cause of Loss Indemnities With Month of Loss
# download_rma_web_data_files(
#   years = 1989:2009,
#   file_name = "col_month",
#   dest = paste0(dir_data_release,"/col"))
# 
# utils::download.file(
#   "https://pubfs-rma.fpac.usda.gov/pub/Miscellaneous_Files/cause_of_loss/indem_only_month/col%20indemnities%20with%20month.pdf",
#   destfile = paste0(dir_data_release,"/col/col_indemnities_with_month.pdf"),mode= "wb",quiet    = TRUE)
# 
# # Cause of Loss Summary of Business [1989-2009]
# download_rma_web_data_files(
#   years = 1989:2009,
#   file_name = "col_sob",
#   dest = paste0(dir_data_release,"/col"))
# 
# utils::download.file(
#   "https://pubfs-rma.fpac.usda.gov/pub/Miscellaneous_Files/cause_of_loss/prem_and_indem/col%20summary%20of%20business.pdf",
#   destfile = paste0(dir_data_release,"/col/col_summary_of_business.pdf"),mode= "wb",quiet    = TRUE)


# Cause of Loss Summary of Business [Hist]
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
colnames(data) <- layouts_fcip$col_sob
data <- data[layouts_fcip$col_sob]

data <- as.data.table(data)

data[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))) := lapply(
  .SD, function(x) as.numeric(as.character(x))), 
  .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(data))]

data[, c(intersect(FCIP_FORCE_CHARACTER_KEYS, names(data))) := lapply(
  .SD, function(x) trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(as.character(x)))), which = c("both"))), 
  .SDcols = intersect(FCIP_FORCE_CHARACTER_KEYS, names(data))]

data[, c(intersect(FCIP_FORCE_AMOUNT_VARIABLES, names(data))) := lapply(
  .SD, function(x) as.numeric(as.character(x))), 
  .SDcols = intersect(FCIP_FORCE_AMOUNT_VARIABLES, names(data))]

data[,loss_ratio := indemnity_amount/total_premium_amount]
data <- data[!liability_amount %in% c(0,NA,Inf,-Inf)]

saveRDS(data,file=paste0(dir_data_release,"/col/col_sob_hist.rds"))


# Stage Code Listing
utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/Stage_Code_Listing.xlsx",
  destfile = paste0(dir_data_release,"/col/stage_code_listing.xlsx"),mode= "wb",quiet    = TRUE)
utils::download.file(
  "https://pubfs-rma.fpac.usda.gov/pub/Miscellaneous_Files/cause_of_loss/prem_and_indem/col%20summary%20of%20business.pdf",
  destfile = paste0(dir_data_release,"/col/col_sob_hist_field_description.pdf"),mode= "wb",quiet    = TRUE)


# Send Cause of Loss to Github       
# piggyback::pb_upload(
#   list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE),
#   repo = "ftsiboe/USFarmSafetyNetLab", tag  = "col",overwrite = TRUE)
# 
# col_list <- list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE)
# 
# col_list <- col_list[!grepl("colsom_",col_list)]
# col_list <- col_list[!grepl("col_month_",col_list)]
# col_list <- col_list[!grepl("col_indem_",col_list)]
# col_list <- col_list[!grepl(".pdf|.xlsx",col_list)]
# col_list <- col_list[!grepl("col_sob_",col_list)]

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE,pattern = "colsom_"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "col",overwrite = TRUE)

Sys.sleep(60)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE,pattern = "col_month_"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "col",overwrite = TRUE)

Sys.sleep(60)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE,pattern = "col_indem_"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "col",overwrite = TRUE)

Sys.sleep(60)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE,pattern = ".pdf"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "col",overwrite = TRUE)

Sys.sleep(60)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE,pattern = ".xlsx"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "col",overwrite = TRUE)

Sys.sleep(60)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/col"), full.names = TRUE, recursive = TRUE,pattern = "col_sob_"),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "col",overwrite = TRUE)

