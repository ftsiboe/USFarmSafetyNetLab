
source("data-raw/work_environment_setup.R")

devtools::document()
# AO Expense Subsidy Percent 2001-2010  ####

SOB0 <- doBy::summaryBy(list(c("total_premium_amount","subsidy_amount"),c("commodity_year","insurance_plan_code","coverage_level_percent")),
                        data=as.data.frame(readRDS("data-raw/data_release/sob/sobtpu_all.rds")),
                        FUN=sum,keep.names = T,na.rm=T)

ins_plan <- unique(as.data.frame(readRDS(paste0(dir_data_release,"/adm/recodes_insurance_plan.rds")))[
  c("commodity_year","insurance_plan_code","insurance_plan_abbreviation","outcome_protected","insurance_plan_recode")])

ins_plan$ins_plan_ab_rc <- ifelse(ins_plan$insurance_plan_code %in% c(1,90),"YP or APH", ins_plan$insurance_plan_abbreviation) 
ins_plan$ins_plan_ab_rc <- ifelse(ins_plan$insurance_plan_code %in% c(44,2),"RA or CRC", ins_plan$ins_plan_ab_rc) 
ins_plan$ins_plan_ab_rc <- ifelse(ins_plan$insurance_plan_code %in% c(25,42,3),"RP-HPE or IP", ins_plan$ins_plan_ab_rc) 

SOB0 <- dplyr::full_join(SOB0,ins_plan,by=names(SOB0)[names(SOB0) %in% names(ins_plan)])

# 1998-2004 Standard Reinsurance Agreement (https://legacy.rma.usda.gov/pubs/ra/sraarchives/98SRA.pdf)
# 2005-2009 Standard Reinsurance Agreement (https://legacy.rma.usda.gov/pubs/ra/05SRA_final.pdf)
# 2010      Standard Reinsurance Agreement (https://legacy.rma.usda.gov/pubs/ra/05sra_final.pdf)
# CAT eligible coverage                                = 00.0%.
SOB0$AO_subsidy_rate <- NA

# 1998:2004
# revenue insurance plans that can not increase liability whenever the market price at harvest exceeds the market price at the time of planting, 27.0 percent
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 1998:2024 & SOB0$AO_subsidy_rate %in% NA & SOB0$outcome_protected %in% c("Revenue") & grepl("RP-HPE or IP",SOB0$ins_plan_ab_rc), 0.2700,SOB0$AO_subsidy_rate)
# For revenue insurance plans that can increase liability whenever the market price at the time of harvest exceeds the market price at the time of planting
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 1998:2024 & SOB0$AO_subsidy_rate %in% NA & SOB0$coverage_level_percent %in% c("Revenue") & grepl("RA or CRC",SOB0$ins_plan_ab_rc), 0.2325,SOB0$AO_subsidy_rate)
# For eligible crop insurance contracts that provide coverage under the GRP
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 1998:2024 & SOB0$AO_subsidy_rate %in% NA & SOB0$insurance_plan_abbreviation %in% c("GRP","GRIP"),0.2500,SOB0$AO_subsidy_rate)
# other plan
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 1998:2024 & SOB0$AO_subsidy_rate %in% NA, 0.2700,SOB0$AO_subsidy_rate)


# 2005
# Group Risk Plan @ 50-75% coverage in 2005          = 22.6%
# Group Risk Plan @ 80% coverage in 2005             = 21.4%
# Group Risk Plan @ 85% coverage in 2005             = 21.1%
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & SOB0$insurance_plan_abbreviation %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) <=  75, 0.226,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & SOB0$insurance_plan_abbreviation %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.214,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & SOB0$insurance_plan_abbreviation %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.211,SOB0$AO_subsidy_rate)
# Revenue plan @ 50-75% coverage in 2005      = 21.0%
# Revenue plan @ 80% coverage in 2005         = 19.9%
# Revenue plan @ 85% coverage in 2005         = 19.6%
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & SOB0$outcome_protected %in% c("Revenue") & round(SOB0$coverage_level_percent*100) <=  75, 0.210,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & SOB0$outcome_protected %in% c("Revenue") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.199,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & SOB0$outcome_protected %in% c("Revenue") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.196,SOB0$AO_subsidy_rate)
# other plan @ 50-75% coverage in 2005        = 24.4%
# other plan @ 80% coverage in 2005           = 23.1%
# other plan @ 85% coverage in 2005           = 22.8%
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & round(SOB0$coverage_level_percent*100) <=  75, 0.244,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.231,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2005 & SOB0$AO_subsidy_rate %in% NA & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.228,SOB0$AO_subsidy_rate)


# 2006:2010
# Group Risk Plan @ 50-75% coverage in 2006-2009     = 22.4%
# Group Risk Plan @ 80% coverage in 2006-2009        = 20.1%
# Group Risk Plan @ 85% coverage in 2006-2009        = 19.4%
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & SOB0$insurance_plan_abbreviation %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) <=  75, 0.224,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & SOB0$insurance_plan_abbreviation %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.201,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & SOB0$insurance_plan_abbreviation %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.194,SOB0$AO_subsidy_rate)
# Revenue plan @ 50-75% coverage in 2006-2009 = 20.8%
# Revenue plan @ 80% coverage in 2006-2009    = 18.7%
# Revenue plan @ 85% coverage in 2006-2009    = 18.1%
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & SOB0$outcome_protected %in% c("Revenue") & round(SOB0$coverage_level_percent*100) <=  75, 0.280,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & SOB0$outcome_protected %in% c("Revenue") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.187,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & SOB0$outcome_protected %in% c("Revenue") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.181,SOB0$AO_subsidy_rate)
# other plan @ 50-75% coverage in 2006-2009   = 24.2%
# other plan @ 80% coverage in 2006-2009      = 21.7%
# other plan @ 85% coverage in 2006-2009      = 21.0%
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & round(SOB0$coverage_level_percent*100) <=  75, 0.242,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.217,SOB0$AO_subsidy_rate)
SOB0$AO_subsidy_rate <- ifelse(SOB0$commodity_year %in% 2006:2010 & SOB0$AO_subsidy_rate %in% NA & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.210,SOB0$AO_subsidy_rate)

SOB0$ao_expense_subsidy_percent <- SOB0$AO_subsidy_rate
SOB0 <- doBy::summaryBy(ao_expense_subsidy_percent~commodity_year+insurance_plan_code+coverage_level_percent,
                        data=SOB0,FUN=mean,na.rm=T,keep.names = T)

SOB0$ao_expense_subsidy_percent <- ifelse(SOB0$ao_expense_subsidy_percent %in% c(0,NaN,Inf,-Inf),NA,SOB0$ao_expense_subsidy_percent)
SOB0 <- SOB0[!SOB0$ao_expense_subsidy_percent %in% c(NA,NaN,Inf,-Inf,0),]

# 2011:Current
df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "D00097_IceAOExpenseSubsidy")

df[,commodity_year := reinsurance_year ]
df <- unique(df[, c("commodity_year","insurance_plan_code","coverage_level_percent","ao_expense_subsidy_percent"), with = FALSE])

df <- df[complete.cases(df)]

df <- data.table::as.data.table(rbind(as.data.frame(df),SOB0))

df[, data_source := "USDA-RMA, Insurance Control Elements - PASS - D00154"]

df <- harmonize_codes_and_names(df)

saveRDS(df,file=paste0(dir_data_release,"/ice/ice_ao_expense_subsidy_percent.rds"));rm(df);gc()
