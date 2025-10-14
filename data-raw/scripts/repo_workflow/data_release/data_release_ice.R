
source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

# AO Expense Subsidy
source("data-raw/scripts/repo_workflow/data_release/data_release_ice_ao_expense.R")
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
df <- harmonize_codes_and_names(df)
saveRDS(df,file=paste0(dir_data_release,"/ice/ice_ao_expense_subsidy_percent.rds"));rm(df);gc() 

# Program indicator                   
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "IceProgramIndicator")
df <- unique(df[, c("reinsurance_year","program_indicator_code","program_indicator_description"), with = FALSE])
df <- df[complete.cases(df)]
table(df$program_indicator_code,df$reinsurance_year)
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file=paste0(dir_data_release,"/ice/ice_program_indicator_code.rds"));rm(df);gc()

# Administrative Fee Waiver Code 
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "IceAdministrativeFeeWaiver")
df <- unique(df[, c("reinsurance_year","administrative_fee_waiver_code","administrative_fee_waiver_description"), with = FALSE])
df <- df[complete.cases(df)]
table(df$administrative_fee_waiver_code,df$reinsurance_year)
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file=paste0(dir_data_release,"/ice/ice_administrative_fee_waiver_code.rds"));rm(df);gc()

# Policy History Request Code  
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "IceHistoryRequest")
df <- unique(df[, c("reinsurance_year","producer_policy_history_request_code","policy_producer_history_request_code_description"), with = FALSE])
df <- df[complete.cases(df)]
table(df$producer_policy_history_request_code,df$reinsurance_year)
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file=paste0(dir_data_release,"/ice/ice_policy_history_request_code.rds"));rm(df);gc()

# Yield Type Code       
source("data-raw/scripts/repo_workflow/data_release/data_release_ice_yield_type_description.R")
df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]
saveRDS(df,file=paste0(dir_data_release,"/ice/ice_yield_type_code.rds"))

# Send  Insurance Control Elements to Github
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/ice"), full.names = TRUE, recursive = TRUE),
  repo = "ftsiboe/USFarmSafetyNetLab", tag  = "ice",overwrite = TRUE)

