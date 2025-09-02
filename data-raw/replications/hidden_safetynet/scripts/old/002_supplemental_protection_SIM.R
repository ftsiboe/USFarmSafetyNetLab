# https://farmdocdaily.illinois.edu/2024/06/cotton-stax-and-modified-supplemental-coverage-option-concerns-with-moving-crop-insurance-from-risk-management-to-income-support.html
# https://farmdocdaily.illinois.edu/2024/06/farm-bill-proposals-to-enhance-supplemental-coverage-option-sco.html
rm(list=ls(all=TRUE));gc();gc()
library('magrittr');library(tidyverse);library(future.apply);library(data.table)
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),paste0("/homes/",Sys.info()['user'],"/Articles/USA/supplemental_protection/")))
source("000_supplemental_protection_FUN.R")

# unlink(dir_sim,recursive = T)
dir.create(dir_sim)
lapply(
  as.numeric(gsub("[^0-9]","",list.files("data/agentdata"))),
  function(crop_yr){
    tryCatch({ 
      # crop_yr <- 2015
      dir.create(paste0(dir_sim,crop_yr,"/"))
      return(crop_yr)
    }, error = function(e){return(NULL)})
  })

function(){ #
  lapply(
    as.numeric(gsub("[^0-9]","",list.files("data/agentdata"))),
    function(crop_yr){
      return(print(paste0(crop_yr,"=", length(list.files(path=paste0(dir_sim,crop_yr),recursive = T,full.names = T)))))
    })
  
  plan(multisession)
  future_lapply(
    year_beg:year_end,
    function(crop_yr){
      
      Check <- as.data.frame(file.info(list.files(path=paste0(dir_sim,crop_yr),recursive = T,full.names = T)))
      
      lapply(
        row.names(Check),
        function(file){
          DONE <- NULL
          tryCatch({
            data <- readRDS(file)
            
            if (nrow(data) == 0) {
              # Delete the file if the data frame is empty
              unlink(file)
              cat("Deleted empty file:", file, "\n")
            } else {
              cat("File not empty:", file, "\n")
            }
            
          }, error=function(e){})
          return(DONE)
        })
      return(crop_yr)
    })
  plan(sequential)
  
}

work_list <-as.data.frame(
  data.table::rbindlist(
    lapply(
      as.numeric(gsub("[^0-9]","",list.files("data/agentdata"))),
      function(crop_yr){
        data.frame(crop_yr=crop_yr,draw=1:500)
      }),fill = TRUE))

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  1:nrow(work_list),
  function(work){
    tryCatch({ 
      # work <- 4001
      sim <- work_list$draw[work]
      crop_yr <- work_list$crop_yr[work]
      if(!paste0("sim",stringr::str_pad(sim,pad="0",3),".rds") %in% list.files(paste0(dir_sim,crop_yr))){
        #--------------------------------------------------------------
        # Preliminaries                                             ####
        data <- as.data.frame(readRDS(paste0(Dr.FCIP,"rmaMultiverse/Output/yield_price_sim/sim_rma/sim_rma_",crop_yr,".rds")))
        data <- data %>% tidyr::unnest(cols = c(sim_id,sim_Yield,sim_Price))
        data <- as.data.frame(data)
        data <- data[data$sim_id %in% sim,]
        data$sim_Yield <- ifelse(data$sim_Yield<0,0,data$sim_Yield)
        names(data)[names(data) %in% "sim_Yield"] <- "actual_farm_yield"
        names(data)[names(data) %in% "sim_Price"] <- "actual_price"
        
        data <- dplyr::inner_join(data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl",
                                         "actual_farm_yield","actual_price")],
                                  readRDS(paste0("data/agentdata/agentdata",crop_yr,".rds"))); gc(); gc()
        
        setDT(data)
        countydata <- data[,.(
          Expected_county_yield = weighted.mean(x=yield_approved,w=planted_acres,na.rm=T),
          Final_county_yield = weighted.mean(x=actual_farm_yield,w=planted_acres,na.rm=T),
          Harvest_Price = weighted.mean(x=actual_price,w=planted_acres,na.rm=T)), 
          by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd")]
        
        data <- dplyr::inner_join(as.data.frame(data),as.data.frame(countydata))
        data$revenue <- data$actual_farm_yield*data$actual_price*data$planted_acres
        
        rm(countydata); gc(); gc()
        #--------------------------------------------------------------
        # Base policy: Liability                                    #### 
        # Calculate the Premium Guarantee Per Acre Amount (Round to 1 decimal).
        data$guaranteed_yield <- round(data$yield_approved*data$cov_lvl,1)
        
        # Calculate the Total Guarantee Amount (Round to 2 decimals).
        data$price_election_amount <- data$Projected_Price*data$price_election
        data$insurance_guarantee   <- round(data$guaranteed_yield*data$price_election_amount,2)
        
        # 	Calculate the Liability Amount (Round to whole number).
        data$insured_acres <- data$planted_acres*data$insured_share
        data$liability     <- round(data$insurance_guarantee*data$insured_acres,0)
        #--------------------------------------------------------------
        # Base policy: Total Premium, Subsidy, and Producer Premium #### 
        data$total_premium    <- round(data$premium_rate*data$liability)
        data$subsidy_amount   <- round(data$total_premium*round(data$Subsidy_Percent,2))
        data$producer_premium <- round(data$total_premium - data$subsidy_amount)
        #--------------------------------------------------------------
        # Base policy: Indemnity                                    #### 
        
        data$determined_acreage      <- data$planted_acres*data$dmage_are_rate
        data$price_risk              <- ifelse(data$price_election_amount*2<data$Harvest_Price,data$price_election_amount*2,data$Harvest_Price)
        data$price_risk              <- ifelse(data$price_risk>data$price_election_amount,data$price_risk,data$price_election_amount)
        data$new_insurance_guarantee <- ifelse(data$ins_plan_cd %in% 2,data$price_risk*data$guaranteed_yield,data$insurance_guarantee)
        data$revenue_to_count        <- data$actual_farm_yield*ifelse(data$ins_plan_cd %in% c(1,90),data$price_election_amount,data$Harvest_Price)
        data$indemnity               <- ifelse(data$revenue_to_count<data$new_insurance_guarantee,data$new_insurance_guarantee-data$revenue_to_count,0)
        data$indemnity               <- data$indemnity*data$determined_acreage*data$insured_share
        
        data <- data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl","sco", "eco90", "eco95",
                       "actual_farm_yield","actual_price","Expected_county_yield","Final_county_yield","Harvest_Price","Projected_Price",
                       "new_insurance_guarantee","liability","insured_acres","total_premium","subsidy_amount","producer_premium","indemnity","revenue")]; gc(); gc()
        
        #--------------------------------------------------------------
        # Supplemental-FACTORS                                      ####
        
        supplist <- rbind(data.frame(ins_plan_cd=31:33,Trigger=0.86,plan="sco8665",Subsidy_factor=0.65),
                          # The House Proposal would:
                          # Increase the coverage level of SCO to 90%.
                          # Increase the risk subsidy to 80% from the current 65% level for SCO and the current 44% level for ECO.
                          data.frame(ins_plan_cd=31:33,Trigger=0.86,plan="sco8680",Subsidy_factor=0.80),
                          data.frame(ins_plan_cd=51:53,Trigger=0.90,plan="sco9065",Subsidy_factor=0.65),
                          data.frame(ins_plan_cd=51:53,Trigger=0.90,plan="sco9080",Subsidy_factor=0.80),
                          data.frame(ins_plan_cd=87:89,Trigger=0.90,plan="eco9080",Subsidy_factor=0.80),
                          data.frame(ins_plan_cd=87:89,Trigger=0.95,plan="eco9580",Subsidy_factor=0.80),
                          
                          # The Senate Proposal would:
                          # Increase SCO’s coverage level to 88%, less than the House proposed 90%.
                          # Increase the risk subsidy to 80% identically to the House Proposal.
                          data.frame(ins_plan_cd=41:43,Trigger=0.88,plan="sco8865",Subsidy_factor=0.65),
                          data.frame(ins_plan_cd=41:43,Trigger=0.88,plan="sco8880",Subsidy_factor=0.80))
        
        if(crop_yr >=2021 ){
          supplist <- rbind(supplist,
                            data.frame(ins_plan_cd=87:89,Trigger=0.90,plan="eco9044",Subsidy_factor=0.44), 
                            data.frame(ins_plan_cd=87:89,Trigger=0.95,plan="eco9544",Subsidy_factor=0.44))
        }
        
        sim_SUPP <-as.data.frame(
          data.table::rbindlist(
            lapply(
              1:nrow(supplist),
              function(endors){
                tryCatch({ 
                  # endors <- 2
                  data_ii <- data
                  Trigger <- supplist[endors,"Trigger"]
                  
                  # ECO’s subsidized rate is as follows: 44% rate for revenue plans and 51% rate for yield plans
                  # The Federal Government pays 65 percent of the premium cost for SCO
                  adm00 <- as.data.frame(readRDS("data/rma_supplemental_premium_rates.rds"))
                  adm00 <- adm00[adm00$crop_yr %in% crop_yr,]; gc(); gc()
                  adm00 <- adm00[adm00$ins_plan_cd %in% supplist[endors,"ins_plan_cd"],]
                  adm00 <- dplyr::inner_join(
                    adm00,unique(data_ii[c("state_cd","county_cd","crop_cd","typ_cd","pract_cd")]),
                    by =c("state_cd","county_cd","crop_cd","typ_cd","pract_cd"))
                  
                  if(supplist[endors,"ins_plan_cd"] %in% c(31:33)){
                    data_ii$ENDOS_Subsidy_factor <- ifelse(data_ii$ins_plan_cd %in% 1:3,supplist[endors,"Subsidy_factor"],NA)
                    data_ii$END_cov_lvl <- data_ii$cov_lvl
                    adm00$ins_plan_cd <- adm00$ins_plan_cd-30
                    data_ii <- data_ii[data_ii$ins_plan_cd %in% (supplist[endors,"ins_plan_cd"]-30),]
                  }
                  
                  if(supplist[endors,"ins_plan_cd"] %in% c(41:43)){
                    data_ii$ENDOS_Subsidy_factor <- ifelse(data_ii$ins_plan_cd %in% 1:3,supplist[endors,"Subsidy_factor"],NA)
                    data_ii$END_cov_lvl <- data_ii$cov_lvl
                    adm00$ins_plan_cd <- adm00$ins_plan_cd-40
                    data_ii <- data_ii[data_ii$ins_plan_cd %in% (supplist[endors,"ins_plan_cd"]-40),]
                  }
                  
                  if(supplist[endors,"ins_plan_cd"] %in% c(51:53)){
                    data_ii$ENDOS_Subsidy_factor <- ifelse(data_ii$ins_plan_cd %in% 1:3,supplist[endors,"Subsidy_factor"],NA)
                    data_ii$END_cov_lvl <- data_ii$cov_lvl
                    adm00$ins_plan_cd <- adm00$ins_plan_cd-50
                    data_ii <- data_ii[data_ii$ins_plan_cd %in% (supplist[endors,"ins_plan_cd"]-50),]
                  }
                  
                  if(supplist[endors,"ins_plan_cd"] %in% c(87:89)){
                    data_ii$ENDOS_Subsidy_factor <- ifelse(data_ii$ins_plan_cd %in% 1,0.51,supplist[endors,"Subsidy_factor"])
                    data_ii$END_cov_lvl <- 0.86
                    adm00 <- adm00[adm00$cov_lvl %in% supplist[endors,"Trigger"],names(adm00)[!names(adm00) %in% "cov_lvl"]]
                    adm00$ins_plan_cd <- adm00$ins_plan_cd-86
                    data_ii <- data_ii[data_ii$ins_plan_cd %in% (supplist[endors,"ins_plan_cd"]-86),]
                  }
                  
                  # Shallow loss protection
                  data_ii$Expected_county_value <- data_ii$Expected_county_yield*data_ii$Projected_Price
                  data_ii$Final_county_value    <- data_ii$Final_county_yield*data_ii$Harvest_Price
                  
                  data_ii$Coverage_Range      <- Trigger - data_ii$END_cov_lvl
                  data_ii$Expected_Crop_Value <- data_ii$liability/data_ii$cov_lvl
                  data_ii$ENDOS_protection    <- data_ii$Expected_Crop_Value*data_ii$Coverage_Range
                  
                  # Shallow loss premium 
                  setDT(adm00)
                  
                  adm00 <- adm00[,.(base_rate = mean(base_rate,na.rm=T)),by = c(names(adm00)[names(adm00) %in% c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","ins_plan_cd","cov_lvl")])]
                  adm01 <- as.data.frame(adm00[,.(base_rate01 = mean(base_rate,na.rm=T)),by = c(names(adm00)[names(adm00) %in% c("crop_yr","state_cd","county_cd","crop_cd","pract_cd","ins_plan_cd","cov_lvl")])])
                  adm02 <- as.data.frame(adm00[,.(base_rate02 = mean(base_rate,na.rm=T)),by = c(names(adm00)[names(adm00) %in% c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","ins_plan_cd","cov_lvl")])])
                  adm03 <- as.data.frame(adm00[,.(base_rate03 = mean(base_rate,na.rm=T)),by = c(names(adm00)[names(adm00) %in% c("crop_yr","state_cd","county_cd","crop_cd","ins_plan_cd","cov_lvl")])])
                  
                  data_ii <- dplyr::full_join(data_ii,adm00);rm(adm00);data_ii <- data_ii[!data_ii$insured_acres %in% c(NA,Inf,Inf,NaN),];gc();gc()
                  data_ii <- dplyr::full_join(data_ii,adm01);rm(adm01);data_ii <- data_ii[!data_ii$insured_acres %in% c(NA,Inf,Inf,NaN),];gc();gc()
                  data_ii <- dplyr::full_join(data_ii,adm02);rm(adm02);data_ii <- data_ii[!data_ii$insured_acres %in% c(NA,Inf,Inf,NaN),];gc();gc()
                  data_ii <- dplyr::full_join(data_ii,adm03);rm(adm03);data_ii <- data_ii[!data_ii$insured_acres %in% c(NA,Inf,Inf,NaN),];gc();gc()
                  
                  data_ii$base_rate <- ifelse(data_ii$base_rate %in% c(NA,Inf,Inf,NaN),data_ii$base_rate01,data_ii$base_rate)
                  data_ii$base_rate <- ifelse(data_ii$base_rate %in% c(NA,Inf,Inf,NaN),data_ii$base_rate02,data_ii$base_rate)
                  data_ii$base_rate <- ifelse(data_ii$base_rate %in% c(NA,Inf,Inf,NaN),data_ii$base_rate03,data_ii$base_rate)
                  data_ii <- data_ii[!data_ii$base_rate %in% c(NA,Inf,Inf,NaN),]
                  
                  data_ii$ENDOS_Total_Premium    <- data_ii$ENDOS_protection*data_ii$base_rate
                  data_ii$ENDOS_Subsidy_amount   <- data_ii$ENDOS_Subsidy_factor*data_ii$ENDOS_Total_Premium
                  data_ii$ENDOS_Producer_Premium <- data_ii$ENDOS_Total_Premium - data_ii$ENDOS_Subsidy_amount
                  
                  # Shallow loss indemnity
                  New_Expected_Crop_Value <- data_ii$new_insurance_guarantee/data_ii$cov_lvl
                  New_ENDOS_protection <- New_Expected_Crop_Value*data_ii$Coverage_Range*data_ii$insured_acres
                  ENDOS_Reduction_rate <- ifelse(data_ii$ins_plan_cd %in% 1,Trigger-data_ii$Final_county_yield/data_ii$Expected_county_yield,NA)
                  ENDOS_Reduction_rate <- ifelse(data_ii$ins_plan_cd %in% 3,Trigger-data_ii$Final_county_value/data_ii$Expected_county_value,ENDOS_Reduction_rate)
                  ENDOS_Reduction_rate <- ifelse(data_ii$ins_plan_cd %in% 2,Trigger-(data_ii$Final_county_value/(data_ii$Expected_county_yield * data_ii$Harvest_Price)),ENDOS_Reduction_rate)
                  ENDOS_Reduction_rate <- ifelse(ENDOS_Reduction_rate<0,0,ENDOS_Reduction_rate)
                  ENDOS_Payment_Factor <- ifelse(ENDOS_Reduction_rate/data_ii$Coverage_Range>1,1,ENDOS_Reduction_rate/data_ii$Coverage_Range)
                  ENDOS_Indemnity <- ENDOS_Payment_Factor*New_ENDOS_protection
                  
                  data_ii$liability        <- data_ii$ENDOS_protection
                  data_ii$total_premium    <- data_ii$ENDOS_Total_Premium
                  data_ii$subsidy_amount   <- data_ii$ENDOS_Subsidy_amount
                  data_ii$producer_premium <- data_ii$ENDOS_Producer_Premium
                  data_ii$indemnity        <- ENDOS_Indemnity
                  
                  data_ii <- data_ii[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl",
                                       "liability","total_premium","subsidy_amount","producer_premium","indemnity")]
                  rm(New_Expected_Crop_Value,New_ENDOS_protection,ENDOS_Reduction_rate,ENDOS_Payment_Factor,ENDOS_Indemnity);gc();gc()
                  
                  data_ii$sup <- supplist[endors,"plan"]
                  return(data_ii)
                }, error = function(e){return(NULL)})
                
              }),fill = TRUE))
        
        sim_SUPP$sup <- toupper(sim_SUPP$sup)
        rm(supplist); gc(); gc()
        #--------------------------------------------------------------
        # Supplemental-CURENT                                       ####
        sim_CURENT <- as.data.frame(data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl","sco", "eco90", "eco95")])
        sim_CURENT <- sim_CURENT[!rowSums(sim_CURENT[c("sco", "eco90", "eco95")],na.rm=T) %in% c(0,NA,Inf,-Inf,NaN),]
        sim_CURENT <- dplyr::full_join(sim_CURENT,sim_SUPP[sim_SUPP$sup %in% c("SCO8665","ECO9044","ECO9544"),])
        for(ww in c("liability","total_premium","subsidy_amount","producer_premium","indemnity")){
          sim_CURENT[,ww] <- ifelse(sim_CURENT$sup %in% "SCO8665",sim_CURENT[,ww]*sim_CURENT$sco,sim_CURENT[,ww])
          sim_CURENT[,ww] <- ifelse(sim_CURENT$sup %in% "ECO9044",sim_CURENT[,ww]*sim_CURENT$eco90,sim_CURENT[,ww])
          sim_CURENT[,ww] <- ifelse(sim_CURENT$sup %in% "ECO9544",sim_CURENT[,ww]*sim_CURENT$eco95,sim_CURENT[,ww])
        }
        setDT(sim_CURENT)
        sim_CURENT <- sim_CURENT[,.(
          liability = sum(liability,na.rm=T),
          total_premium = sum(total_premium,na.rm=T),
          subsidy_amount = sum(subsidy_amount,na.rm=T),
          producer_premium = sum(producer_premium,na.rm=T),
          indemnity = sum(indemnity,na.rm=T)), 
          by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")]
        
        sim_CURENT <- data.table::rbindlist(
          list(sim_CURENT,
               as.data.frame(data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl",
                                    "liability","total_premium","subsidy_amount","producer_premium","indemnity","revenue")])),fill = TRUE)
        
        sim_CURENT <- sim_CURENT[,.(
          revenue = sum(revenue,na.rm=T),
          liability = sum(liability,na.rm=T),
          total_premium = sum(total_premium,na.rm=T),
          subsidy_amount = sum(subsidy_amount,na.rm=T),
          producer_premium = sum(producer_premium,na.rm=T),
          indemnity = sum(indemnity,na.rm=T),
          combination="Basic+CURENT"), 
          by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")];gc();gc()
        
        #--------------------------------------------------------------
        # Supplemental-FULL                                         ####
        sim_FULL <- data.frame(sco=c("SCO8665","SCO8680","SCO8865","SCO8880","SCO9065","SCO9080"),eco="")
        if(crop_yr >=2021 ){
          sim_FULL <- rbind(sim_FULL,
                            data.frame(sco="",eco=c("ECO9044","ECO9544","ECO9080","ECO9580")),
                            data.frame(sco=c("SCO8665","SCO8680","SCO8865","SCO8880"),eco="ECO9044"),
                            data.frame(sco=c("SCO8665","SCO8680","SCO8865","SCO8880"),eco="ECO9080"),
                            data.frame(sco=c("SCO8665","SCO8680","SCO8865","SCO8880","SCO9065","SCO9080"),eco="ECO9544"),
                            data.frame(sco=c("SCO8665","SCO8680","SCO8865","SCO8880","SCO9065","SCO9080"),eco="ECO9580"))
        }
        
        sim_FULL <- as.data.frame(
          data.table::rbindlist(
            lapply(
              1:nrow(sim_FULL),
              function(xxx){
                tryCatch({ 
                  # xxx <- 6
                  setDT(sim_SUPP)
                  scoecoii <- sim_SUPP[sup %in% c(sim_FULL$sco[xxx],sim_FULL$eco[xxx]),.(
                    liability = sum(liability,na.rm=T),
                    total_premium = sum(total_premium,na.rm=T),
                    subsidy_amount = sum(subsidy_amount,na.rm=T),
                    producer_premium = sum(producer_premium,na.rm=T),
                    indemnity = sum(indemnity,na.rm=T)), 
                    by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")]
                  
                  scoecoii <- data.table::rbindlist(
                    list(scoecoii,
                         as.data.frame(data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","struct_cd","pract_cd","ins_plan_cd","cov_lvl",
                                              "liability","total_premium","subsidy_amount","producer_premium","indemnity","revenue")])),fill = TRUE)
                  
                  scoecoii <- scoecoii[,.(
                    revenue = sum(revenue,na.rm=T),
                    liability = sum(liability,na.rm=T),
                    total_premium = sum(total_premium,na.rm=T),
                    subsidy_amount = sum(subsidy_amount,na.rm=T),
                    producer_premium = sum(producer_premium,na.rm=T),
                    indemnity = sum(indemnity,na.rm=T),
                    combination=paste0("Basic+",sim_FULL$sco[xxx],"+",sim_FULL$eco[xxx])), 
                    by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")]
                  
                  return(scoecoii)
                }, error = function(e){return(NULL)})
                
              }),fill = TRUE));gc();gc()
        sim_FULL$combination <- ifelse(sim_FULL$combination %in% "Basic+SCO8665+","Basic+SCO8665",sim_FULL$combination)
        #--------------------------------------------------------------
        # Supplemental-ALT                                          ####
        sim_ALT <- unique(as.data.frame(data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")]))
        sim_ALT <- dplyr::full_join(sim_ALT,sim_SUPP[sim_SUPP$sup %in% "SCO8665",])
        
        sim_ALT <- as.data.frame(
          data.table::rbindlist(
            lapply(c(5,10,15,20,25,30,35,40,45,50,60,70,80,90,100),
                   function(alt){
                     tryCatch({ 
                       # alt <- 10
                       
                       for(ww in c("liability","total_premium","subsidy_amount","producer_premium","indemnity")){
                         sim_ALT[,ww] <- ifelse(sim_ALT$sup %in% "SCO8665",sim_ALT[,ww]*(alt/100),sim_ALT[,ww])
                       }
                       setDT(sim_ALT)
                       sim_ALT <- sim_ALT[,.(
                         liability = sum(liability,na.rm=T),
                         total_premium = sum(total_premium,na.rm=T),
                         subsidy_amount = sum(subsidy_amount,na.rm=T),
                         producer_premium = sum(producer_premium,na.rm=T),
                         indemnity = sum(indemnity,na.rm=T)), 
                         by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")]
                       
                       sim_ALT <- data.table::rbindlist(
                         list(sim_ALT,
                              as.data.frame(data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl",
                                                   "liability","total_premium","subsidy_amount","producer_premium","indemnity","revenue")])),fill = TRUE)
                       
                       sim_ALT <- sim_ALT[,.(
                         revenue = sum(revenue,na.rm=T),
                         liability = sum(liability,na.rm=T),
                         total_premium = sum(total_premium,na.rm=T),
                         subsidy_amount = sum(subsidy_amount,na.rm=T),
                         producer_premium = sum(producer_premium,na.rm=T),
                         indemnity = sum(indemnity,na.rm=T),
                         combination=paste0("Basic+ALT",stringr::str_pad(alt,pad="0",3))), 
                         by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")]
                       
                       return(sim_ALT)
                     }, error = function(e){return(NULL)})
                   }),fill = TRUE))

        #--------------------------------------------------------------
        # FINAL                                                     ####
        setDT(data)
        sim_FCIP <- data[,.(
          revenue = sum(revenue,na.rm=T),
          liability = sum(liability,na.rm=T),
          total_premium = sum(total_premium,na.rm=T),
          subsidy_amount = sum(subsidy_amount,na.rm=T),
          producer_premium = sum(producer_premium,na.rm=T),
          indemnity = sum(indemnity,na.rm=T),
          combination="Basic only"), 
          by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")]
        
        sim_FINAL <- rbind(sim_FCIP,sim_CURENT,sim_FULL,sim_ALT);rm(sim_FCIP,sim_CURENT,sim_FULL,sim_ALT,sim_SUPP,data); gc(); gc()

        saveRDS(sim_FINAL,file =paste0(dir_sim,crop_yr,"/sim",stringr::str_pad(sim,pad="0",3),".rds"))
        
        rm(sim_FINAL);gc();gc();gc()
        
        #--------------------------------------------------------------
      }
      return(sim)
    }, error = function(e){return(NULL)})
  })

