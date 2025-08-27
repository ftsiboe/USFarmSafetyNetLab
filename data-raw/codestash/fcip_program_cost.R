# https://pubfs-rma.fpac.usda.gov/pub/Publications/M13_Handbook/2011/Approved/2011_ADMINISTRATIVE_FEE_SCHEDULE.PDF
# https://pubfs-rma.fpac.usda.gov/pub/Publications/M13_Handbook/2012/Approved/2012_ADMINISTRATIVE_FEE_SCHEDULE.PDF
# https://pubfs-rma.fpac.usda.gov/pub/Publications/M13_Handbook/2013/Approved/2013_Administrative_Fee_Schedule.pdf
# https://pubfs-rma.fpac.usda.gov/pub/Publications/M13_Handbook/2026/Approved/2026_Administrative_Fee_Schedule.pdf
# 1995–2000
# With the Federal Crop Insurance Reform Act of 1994, FCIC introduced the CAT endorsement effective for the 1995 crop year. Under 7 CFR § 400.655:
# https://www.govinfo.gov/content/pkg/FR-1995-01-06/html/95-358.htm
# * CAT (and “limited” coverage): $50 per crop per county (capped at $200 per county; $600 total) 
# * Additional (“buy-up”) coverage: $10 per crop per county 
# 
# 2001–2018
# The Agricultural Risk Protection Act of 2000 raised fees effective for the 2001 crop year:
# https://www.govinfo.gov/content/pkg/FR-2000-06-30/pdf/00-16583.pdf
# * CAT: $100 per crop per county
# * Buy-up: $30 per crop per county 
# 
# 2019–present
# Section 11110 of the 2018 Farm Bill increased the CAT administrative fee effective April 30 2019:
# https://rma.usda.gov/policy-procedure/bulletins-memos/managers-bulletin/2019/mgr-19-006-increased-catastrophic-risk?utm_source=chatgpt.com  
# https://www.law.cornell.edu/uscode/text/7/1508?utm_source=chatgpt.com
# * CAT: $655 per crop per county 
# * Buy-up: remains $30 per crop per county 
# 
# Period	      | CAT Fee	                                    |Buy-up Fee
# Pre-1995	    |N/A (no CAT endorsement)	                    |N/A
# 1995–2000	    |$50 (crop/county; max $200/cty, $600 total)	|$10 (crop/county)
# 2001–2018	    |$100 (crop/county)	                          |$30 (crop/county)
# 2019–present	|$655 (crop/county)	                          |$30 (crop/county)




rm(list = ls(all = TRUE))

source("data-raw/work_environment_setup.R")

# AO Expense Subsidy
ao_subsidy <- readRDS(paste0(dir_data_release,"/ice/ice_ao_expense_subsidy_percent.rds"))[
  , lapply(.SD, function(x) max(x, na.rm = TRUE)),
  by = c("commodity_year","insurance_plan_code","coverage_level_percent_recode"),
  .SDcols = c("ao_expense_subsidy_percent")]


# Send Summary of Business
sob <- data.table::rbindlist(
  list(
    readRDS(paste0(dir_data_release,"/sob/sobscc_all.rds"))[commodity_year < 1989][
      , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
      by = c("commodity_year"),
      .SDcols = c("policies_sold_count","policies_earning_premium_count","liability_amount",
                  "total_premium_amount","subsidy_amount","indemnity_amount")],
    
    readRDS(paste0(dir_data_release,"/sob/sobcov_all.rds"))[commodity_year >= 1989][
      , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
      by = c("commodity_year","insurance_plan_code","coverage_type_code_recode","coverage_level_percent_recode"),
      .SDcols = c("policies_sold_count","policies_earning_premium_count","liability_amount",
                  "total_premium_amount","subsidy_amount","indemnity_amount")]
  ), fill = TRUE)

sob <- sob[!total_premium_amount %in% c(0,NA)]

sob <- dplyr::full_join(sob,ao_subsidy,by=c("commodity_year","insurance_plan_code","coverage_level_percent_recode"))
sob <- sob[!total_premium_amount %in% c(0,NA)]

sob[commodity_year %in% 1995:2000,cat_admin_fee_rate:=200]
sob[commodity_year %in% 2001:2018,cat_admin_fee_rate:=100]
sob[commodity_year        >= 2019,cat_admin_fee_rate:=655]

sob[commodity_year %in% 1995:2000,buyup_admin_fee_rate:=10]
sob[commodity_year %in% 2001:2018,buyup_admin_fee_rate:=30]
sob[commodity_year        >= 2019,buyup_admin_fee_rate:=30]

sob[commodity_year %in% 1995:2000,cat_lae_rate:=0.11]
sob[commodity_year %in% 2001:2008,cat_lae_rate:=0.08]
sob[commodity_year        >= 2009,cat_lae_rate:=0.06]

sob[,total_cat_fees_amount := policies_earning_premium_count * cat_admin_fee_rate] 
sob[,total_buyup_fees_amount := policies_earning_premium_count * buyup_admin_fee_rate] 
sob[coverage_type_code_recode %in% "Buy-up",total_ao_subsidy_amount := ao_expense_subsidy_percent * total_premium_amount] 
sob[coverage_type_code_recode %in% "CAT",total_cat_lae_subsidy_amount := cat_lae_rate*total_premium_amount]


data <- sob[
  , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
  by = c("commodity_year"),
  .SDcols = c("liability_amount","total_premium_amount","subsidy_amount","indemnity_amount",
              "total_cat_fees_amount","total_buyup_fees_amount","total_ao_subsidy_amount","total_cat_lae_subsidy_amount")]

data[commodity_year <= 1980,subsidy_amount:=0]




# There was no premium subsidy before 1980
# A premium subsidy of 30% was introduced following the passage of the Federal Crop Insurance Act of 1980 [P.L. 96-365]
# Introduction of greater subsidy levels, in The Federal Crop Insurance Reform and Dept. of Ag Reorganization Act of 1994 [P.L 103-354] (up to 60%)
tryCatch({data$subsidy <- ifelse(data$crop_yr < 1980,0,data$subsidy)}, error=function(e){})
tryCatch({data$subsidy <- ifelse(data$crop_yr >=1980 & data$crop_yr>1994,0.3*data$total_prem,data$subsidy)}, error=function(e){})
tryCatch({data$subsidy <- rowsum(sob[names(sob)[grepl("subsidy",names(sob))]])}, error=function(e){})





fcip_cost$costA <- total_premium_amount

fcip_cost$costB <- subsidy_amount


fcip_cost$costD <- indemnity_amount

fcip_cost$NWR <- ifelse(fcip_cost$Underwritings_bud %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$Underwritings_rin,fcip_cost$Underwritings_bud)

fcip_cost$costE <- abs(ifelse(fcip_cost$NWR > 0,fcip_cost$NWR,0)) # UWG
fcip_cost$costF <- abs(ifelse(fcip_cost$NWR < 0,fcip_cost$NWR,0)) # UWL

fcip_cost$costG <- ifelse(fcip_cost$AO_bud %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$AO_sob,fcip_cost$AO_bud)




total_direct_cost <- (total_premium_amount - subsidy_amount) - indemnity_amount -
  UWG + 
  UWL - 
  program_delivery_costs






sob <- unique(rbind(sobcov,sobtpu))


















rm(list= ls()[!(ls() %in% c(Keep.List))])
aoSubsidy <- as.data.frame(
  data.table::rbindlist(
    lapply(
      1988:current_year ,
      function(crop_yr){
        # file <- list.files(paste0(Dr.FCIP,"rmaSumOfBussiness/Output/SOBCOV/"),full.names = T)[1]
        aoSubsidy <- readRDS(paste0(Dr.FCIP,"rmaActuarialDataMaster/Output/AO_Subsidy/AO_Subsidy_",max(c(crop_yr,2001)),".rds"))
        aoSubsidy$crop_yr <- crop_yr
        return(aoSubsidy)
      }), fill = TRUE))









