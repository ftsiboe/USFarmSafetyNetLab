
source("data-raw/scripts/environment_setup.R")

devtools::document()

# 1) Retrieve ADM data for each year from 2011 to current, binding into one data.frame
adm <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2011:as.numeric(format(Sys.Date(), "%Y")),
      function(year) {
        tryCatch({ 
          df <- rfcip::get_adm_data(year = year,dataset = "A00070_SubsidyPercent")
          df$commodity_year <- year
          df$insurance_plan_code <- as.numeric(as.character(df$insurance_plan_code))
          df$coverage_level_percent <- as.numeric(as.character(df$coverage_level_percent))
          df$coverage_type_code <- as.character(df$coverage_type_code)
          df$unit_structure_code <- as.character(df$unit_structure_code)
          return(df)
        }, error = function(e){return(NULL)})
      }), fill = TRUE))
adm$unit_structure_code <- ifelse(adm$unit_structure_code %in% c("",NA),"OU",adm$unit_structure_code)
adm <- doBy::summaryBy(list(c("subsidy_percent"),
                            c("commodity_year","insurance_plan_code","coverage_level_percent","coverage_type_code","unit_structure_code")),
                               data=adm, FUN=mean,keep.names = T,na.rm=T)

# 2) Prepend archived 2001–2010 
adm_legacy <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2001:2010,
      function(year) {
        tryCatch({ 
          year <- 2010
          # Premium Subsidy                                      ####
          Subsidy_sob <- as.data.frame(readRDS(paste0(dir_data_release,"/sob/sobtpu_",year,".rds")))
          Subsidy_sob <- doBy::summaryBy(list(c("total_premium_amount","subsidy_amount"),c("insurance_plan_code","coverage_level_percent","coverage_type_code","unit_structure_code")),
                                         data=Subsidy_sob[!Subsidy_sob$coverage_type_code %in% "C",],
                                         FUN=sum,keep.names = T,na.rm=T)
          Subsidy_sob$coverage_level_percent <- ifelse(Subsidy_sob$coverage_level_percent>1,Subsidy_sob$coverage_level_percent/100,Subsidy_sob$coverage_level_percent)
          Subsidy_sob$coverage_level_percent <- round((Subsidy_sob$coverage_level_percent/0.05))*0.05
          Subsidy_sob$coverage_level_percent <- ifelse(Subsidy_sob$coverage_level_percent< 0.5,0.5,Subsidy_sob$coverage_level_percent)
          Subsidy_sob$coverage_level_percent <- ifelse(Subsidy_sob$coverage_level_percent> 0.95,0.95,Subsidy_sob$coverage_level_percent)
          Subsidy_sob <- doBy::summaryBy(list(c("total_premium_amount","subsidy_amount"),c("insurance_plan_code","coverage_level_percent","coverage_type_code","unit_structure_code","coverage_type_code")),
                                         data=Subsidy_sob,FUN=sum,keep.names = T,na.rm=T)
          Subsidy_sob$SOB_Subsidy <- Subsidy_sob$subsidy_amount/Subsidy_sob$total_premium_amount
          Subsidy_sob$unit_structure_code <- ifelse(Subsidy_sob$unit_structure_code %in% c("",NA),"ALL",Subsidy_sob$unit_structure_code)
          Subsidy_sob <- Subsidy_sob[names(Subsidy_sob)[!names(Subsidy_sob) %in% c("total_premium_amount","subsidy_amount")]]
          Subsidy_sob <- Subsidy_sob[!Subsidy_sob$coverage_level_percent %in% NA,]
          Subsidy_sob <- Subsidy_sob[!Subsidy_sob$SOB_Subsidy %in% c(NA,NaN,Inf,-Inf),]
          SOB0 <- Subsidy_sob
          
          SOB0$ADM_Subsidy <- NA
          
          if(year %in% 2001:2008){
            # Premium Subsidy Schedule
            # Effective for all 2001 crop year crops filed 8/31/00 and later. Premium subsidy factors apply to all policies. 
            # When the coverage enhancement option is applicable and elected, the premium subsidy factor is 
            # based on the coverage enhance option coverage level.
            #                   Subsidies and Fees
            # Coverage level				            CAT	  50	55	60	65	70	75	80*	85*	90*
            # Premium subsidy factor**		      1.00	.67	.64	.64	.59	.59	.55	.48	.38	NA
            # GRP/GRIP premium subsidy factor		1.00	NA	NA	NA	NA	.64	.64	.59	.59	.55
            # Administrative fee			          $100	$30	$30	$30	$30	$30	$30	$30	$30	$30
            # * Where applicable.
            # **Applies to all plans of insurance except GRP and GRIP, and livestock.
            # For more information, contact Craig.Witt@rma.usda.gov
            # Last Modified: 04/25/2006
            # source: https://web.archive.org/web/20070711044133/http://www.rma.usda.gov/data/premium.html
            
            ins_plan <- as.data.frame(readRDS(paste0(dir_data_release,"/adm/fcip_recodes_insurance_plan.rds")))
            ins_plan <- unique(ins_plan[ins_plan$commodity_year %in% year,c("insurance_plan_code","insurance_plan_abbreviation")])
            names(ins_plan) <- c("insurance_plan_code","insurance_plan_abbreviationX")
            SOB0 <- dplyr::full_join(SOB0,ins_plan,by=names(SOB0)[names(SOB0) %in% names(ins_plan)])
            
            SOB0$ADM_Subsidy <- ifelse(SOB0$coverage_type_code %in% "C", 1.00,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  50:50, 0.67,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  55:60, 0.64,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  65:70, 0.59,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  75:75, 0.55,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.48,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.38,SOB0$ADM_Subsidy)
            
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  50:65, NA  ,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  70:75, 0.64,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  80:85, 0.59,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  90:90, 0.55,SOB0$ADM_Subsidy)
          }
          
          if(year %in% 2009:2010){
            # Effective for all 2009 reinsurance year crops filed 4/30/08 and later. Premium subsidy factors apply to all policies. When the coverage enhancement option is applicable and elected, the premium subsidy factor is based on the coverage enhance option coverage level.
            # 
            #                                               Subsidies
            # Coverage level			        CAT	  50	55	60	65	70	75	80*	85*	90*
            # Premium subsidy factor**	  1.00	.67	.64	.64	.59	.59	.55	.48	.38	NA
            # GRP premium subsidy factor	1.00	NA	NA	NA	NA	.59	.59	.55	.55	.51
            # GRIP premium subsidy factor	NA	  NA	NA	NA	NA	.59	.55	.55	.49	.44
            # 
            #                         Enterprise and Whole Farm Unit
            #                           Premium Subsidy Factors***
            # Coverage level			      50	55	60	65	70	75	80*	85*
            # Enterprise unit factors*	.80	.80	.80	.80	.80	.77	.68	.53
            # Whole farm unit factors*	NA	NA	NA	.80	.80	.80	.71	.56
            # 
            # * Where applicable.
            # **Applies to all plans of insurance except GRP and GRIP, and livestock.
            # ***See http://www.rma.usda.gov/news/2008/11/1104wholefarm.html for more information on Enterprise and Whole Farm Unit subsidies.
            # For more information, contact Craig Witt.
            # 
            # https://web.archive.org/web/20101007135248/http://www.rma.usda.gov/data/premium.html
            
            ins_plan <- as.data.frame(readRDS(paste0(dir_data_release,"/adm/fcip_recodes_insurance_plan.rds")))
            ins_plan <- unique(ins_plan[ins_plan$commodity_year %in% year,c("insurance_plan_code","insurance_plan_abbreviation")])
            names(ins_plan) <- c("insurance_plan_code","insurance_plan_abbreviationX")
            SOB0 <- dplyr::full_join(SOB0,ins_plan,by=names(SOB0)[names(SOB0) %in% names(ins_plan)])
            
            SOB0$ADM_Subsidy <- ifelse(SOB0$coverage_type_code %in% "C", 1.00,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  50:50, 0.67,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  55:60, 0.64,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  65:70, 0.59,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  75:75, 0.55,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.48,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(! SOB0$insurance_plan_abbreviationX %in% c("GRP","GRIP") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.38,SOB0$ADM_Subsidy)
            
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP") & round(SOB0$coverage_level_percent*100) %in%  50:65, NA  ,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP") & round(SOB0$coverage_level_percent*100) %in%  70:75, 0.59,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP") & round(SOB0$coverage_level_percent*100) %in%  80:85, 0.55,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRP") & round(SOB0$coverage_level_percent*100) %in%  90:90, 0.51,SOB0$ADM_Subsidy)
            
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRIP") & round(SOB0$coverage_level_percent*100) %in%  50:65, NA  ,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRIP") & round(SOB0$coverage_level_percent*100) %in%  70:70, 0.59,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRIP") & round(SOB0$coverage_level_percent*100) %in%  75:80, 0.55,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRIP") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.49,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$insurance_plan_abbreviationX %in% c("GRIP") & round(SOB0$coverage_level_percent*100) %in%  90:90, 0.44,SOB0$ADM_Subsidy)
            
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("EU") & round(SOB0$coverage_level_percent*100) %in%  50:70, 0.80,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("EU") & round(SOB0$coverage_level_percent*100) %in%  75:75, 0.77,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("EU") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.68,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("EU") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.53,SOB0$ADM_Subsidy)
            
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("WU") & round(SOB0$coverage_level_percent*100) %in%  50:60, NA  ,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("WU") & round(SOB0$coverage_level_percent*100) %in%  65:75, 0.80,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("WU") & round(SOB0$coverage_level_percent*100) %in%  80:80, 0.71,SOB0$ADM_Subsidy)
            SOB0$ADM_Subsidy <- ifelse(SOB0$unit_structure_code %in% c("WU") & round(SOB0$coverage_level_percent*100) %in%  85:85, 0.56,SOB0$ADM_Subsidy)
            
          }
          
          
          
          SOB0$subsidy_percent <- ifelse(SOB0$ADM_Subsidy %in% c(NaN,NA,Inf,-Inf,0),round(SOB0$SOB_Subsidy,4),SOB0$ADM_Subsidy)
          SOB0 <- SOB0[c("insurance_plan_code","coverage_level_percent","coverage_type_code","unit_structure_code","subsidy_percent")]
          SOB0$subsidy_percent <- ifelse(SOB0$coverage_type_code  %in% "C",1,SOB0$subsidy_percent)
          
          SOB0$subsidy_percent <- ifelse(SOB0$subsidy_percent %in% c(0,NaN,Inf,-Inf),NA,SOB0$subsidy_percent)
          SOB0 <- SOB0[!SOB0$subsidy_percent %in% c(NA,NaN,Inf,-Inf,0),]
          SOB0$commodity_year <- year
          return(SOB0)
        }, error = function(e){return(NULL)})
      }), fill = TRUE))

adm_legacy$insurance_plan_code <- as.numeric(as.character(adm_legacy$insurance_plan_code))
adm_legacy$coverage_level_percent <- as.numeric(as.character(adm_legacy$coverage_level_percent))
adm_legacy$coverage_type_code <- as.character(adm_legacy$coverage_type_code)
adm_legacy$unit_structure_code <- as.character(adm_legacy$unit_structure_code)

adm_legacy$unit_structure_code <- ifelse(adm_legacy$unit_structure_code %in% c("",NA),"OU",adm_legacy$unit_structure_code)

adm_legacy <- doBy::summaryBy(list(c("subsidy_percent"),
                            c("commodity_year","insurance_plan_code","coverage_level_percent","coverage_type_code","unit_structure_code")),
                       data=adm_legacy, FUN=mean,keep.names = T,na.rm=T)

data <- rbind(adm_legacy,adm)

