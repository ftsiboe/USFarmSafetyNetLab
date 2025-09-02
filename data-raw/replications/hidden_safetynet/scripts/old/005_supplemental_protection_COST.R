
rm(list=ls(all=TRUE));gc()
library('magrittr');library(tidyverse);library(future.apply);library(data.table)
library(sandwich);library(lmtest)
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),paste0("/homes/",Sys.info()['user'],"/Articles/USA/supplemental_protection/")))
Keep.List<-c("Keep.List",ls())
source("000_supplemental_protection_FUN.R")

function(){
  unlink(dir_drawcost,recursive = T)
  
  dir.create(dir_drawcost)
  drawlist <- lapply(
    0:100,
    function(d){
      dir.create(paste0(dir_drawcost,stringr::str_pad(d,pad="0",4)))
      return(d)
    })
}

function(){
  work_list <- as.numeric(list.files(dir_expected))
  work_list <- work_list[! work_list %in% NA]
  
  work_list <- as.data.frame(
    data.table::rbindlist(
      lapply(work_list,
             function(crop_yr){
               return(data.frame(crop_yr=crop_yr,draw=stringr::str_pad(0:100,pad="0",4)))}), fill = TRUE))
  
  work_list <- work_list[!paste0(work_list$draw,"/draw_cost_",work_list$crop_yr,"_",work_list$draw,".rds") %in%
                           list.files(dir_drawcost,recursive = T),]
  #3280
  saveRDS(work_list,file="work_list_cost.rds")
}

work_list <- readRDS("work_list_cost.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

gc()
lapply(
  1:nrow(work_list),
  function(ii){
    tryCatch({ 
      # ii <- 1
      crop_yr <- work_list$crop_yr[ii]
      draw  <- work_list$draw[ii]
      
      data <- as.data.frame(
        data.table::rbindlist(
          lapply(
            list.files(paste0(dir_sim,crop_yr),full.names = T),
            function(sim){
              tryCatch({
                # sim <- list.files(paste0(dir_sim,crop_yr),full.names = T)[1]
                data <- as.data.frame(readRDS(paste0("data/agentdata/agentdata",crop_yr,".rds")))[c("crop_yr","state_cd","county_cd","crop_cd",
                                                                                                    "typ_cd","struct_cd","pract_cd","ins_plan_cd","cov_lvl","comm_amt")]
                data <- dplyr::inner_join(readRDS(sim),data,by=c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl"))
                data <- data[!data$combination %in% c("NONE"),]
                data$sim_id <- gsub(paste0(dir_sim,crop_yr,"/sim"),"",sim)
                data$sim_id <- as.numeric(gsub("[^0-9]","",data$sim_id))
                setDT(data)
                ao <- readRDS(paste0(Dr.FCIP,"rmaActuarialDataMaster/Output/ao_subsidy_percent/ao_subsidy_percent_",crop_yr,".rds"));setDT(ao)
                ao[, ins_plan_cd := ifelse(ins_plan_cd %in% 90,1,ins_plan_cd)] # APH(90) to YP(1)
                ao[, ins_plan_cd := ifelse(ins_plan_cd %in% 44,2,ins_plan_cd)] # CRC(44) to RP(2)
                ao[, ins_plan_cd := ifelse(ins_plan_cd %in% 25,3,ins_plan_cd)] # RA(25)  to RPHPE(3)
                ao[, ins_plan_cd := ifelse(ins_plan_cd %in% 42,3,ins_plan_cd)] # IP(42)  to RPHPE(3)
                ao <- ao[, .(ao_rate = mean(ao_subsidy_percent, na.rm = TRUE)), by = .(ins_plan_cd,cov_lvl)]
                data <- data[ao, on = intersect(names(ao), names(data)), nomatch = 0]
                data[, AOE :=  total_premium*ao_rate]
                
                rm(ao)
                
                data <- dplyr::inner_join(readRDS("output/drawlist.rds")[[stringr::str_pad(draw,pad="0",4)]],as.data.frame(data),
                                          by=c("state_cd","county_cd","crop_cd","typ_cd","pract_cd"))
                setDT(data)
                data <- data[, .(
                  comm_amt = sum(comm_amt, na.rm = TRUE),
                  liability = sum(liability, na.rm = TRUE),
                  indemnity = sum(indemnity, na.rm = TRUE),
                  total_premium = sum(total_premium, na.rm = TRUE),
                  subsidy = sum(subsidy_amount, na.rm = TRUE),
                  AOE = sum(total_premium*ao_rate, na.rm = TRUE)),
                  by = c("combination","crop_yr","state_cd","sim_id")]
                
                data <- dplyr::inner_join(as.data.frame(data),
                                          readRDS(paste0(Dr.FCIP,"rmaReinsurance/Output/underwriting_factors.rds"))[
                                            c("crop_yr","state_cd","Fund","fundaloc_liab","fundaloc_prem",
                                              "fundaloc_indm","retain_liab","retain_prem","retain_indm",
                                              "SGF1","SGF2","SGF3","SGF4", "SGF5", "SGF6", "SGF7")],by=c("state_cd","crop_yr"))
                data$GrossLiability <- data$fundaloc_liab*data$liability
                data$GrossPremium   <- data$fundaloc_prem*data$total_premium
                data$GrossIndemnity <- data$fundaloc_indm*data$indemnity
                
                data$RetainedLiability <- data$retain_liab*data$GrossLiability
                data$RetainedPremium   <- data$retain_prem*data$GrossPremium
                data$RetainedIndemnity <- data$retain_indm*data$GrossIndemnity
                
                data$lr <- data$RetainedIndemnity/data$RetainedPremium
                
                data$UWL <-
                  ifelse(data$lr > 1.00 & data$lr <= 1.60, (apply(data.frame(data$lr,1.60), 1, FUN = min)-1.00)*data$SGF4,0)*data$RetainedPremium +
                  ifelse(data$lr > 1.60 & data$lr <= 2.20, (apply(data.frame(data$lr,2.20), 1, FUN = min)-1.60)*data$SGF5,0)*data$RetainedPremium +
                  ifelse(data$lr > 2.20 & data$lr <= 5.00, (apply(data.frame(data$lr,5.00), 1, FUN = min)-2.20)*data$SGF6,0)*data$RetainedPremium +
                  ifelse(data$lr > 5.00, 0,0)*data$RetainedPremium
                
                data$UWG <-
                  ifelse(data$lr >= 0.65 & data$lr <= 1.00, (1.00-apply(data.frame(data$lr,0.65), 1, FUN = max))*data$SGF3,0)*data$RetainedPremium +
                  ifelse(data$lr >= 0.50 & data$lr <  0.65,(0.65-apply(data.frame(data$lr,0.50), 1, FUN = max))*data$SGF2,0)*data$RetainedPremium +
                  ifelse(data$lr <  0.50 ,(0.50-data$lr)*data$SGF1,0)*data$RetainedPremium
                
                data <- doBy::summaryBy(list(c("comm_amt","liability","total_premium","subsidy",
                                               "indemnity","AOE","UWG","UWL"),c("combination","crop_yr","state_cd","sim_id")),
                                        data=data,FUN=sum,na.rm=T,keep.names = T)
                data00 <- data
                data00$state_cd <- 0
                
                data <- rbind(data,data00)
                rm(data00)
                data$costA <- data$total_prem
                data$costB <- data$subsidy
                data$costC <- data$costA-data$costB
                data$costD <- data$indemnity
                data$costE <- data$UWG
                data$costF <- data$UWL
                data$costG <- data$AOE
                data$costJ <- data$costC-data$costD-data$costE+data$costF-data$costG
                data$lr    <- data$indemnity/data$total_premium
                data <- data[c("combination","crop_yr","state_cd","sim_id","comm_amt","liability","costA","costB","costC","costD","costE","costF","costG","costJ","lr")]
                data$draw <- draw
                
                return(data)
              }, error = function(e){return(NULL)})
            }),fill = TRUE))
      #plan(sequential)
      
      setDT(data)
      
      data <- as.data.frame(
        data[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
             by = c("combination","crop_yr","state_cd","draw" ), 
             .SDcols = c("comm_amt","liability","costA","costB","costC","costD","costE","costF","costG","costJ","lr")])
      
      data <- data %>% tidyr::gather(variable, value, c("liability","costA","costB","costC","costD","costE","costF","costG","costJ","lr"))
      data <- data[data$variable %in% c("liability","costA","costB","costC","costD","costE","costF","costG","costJ","lr"),]
      base <- data[data$combination %in% "Basic+CURENT",]
      names(base)[names(base) %in% "value"] <- "base"
      base <- base[c("crop_yr","state_cd","draw","variable","base")]
      
      data <- dplyr::inner_join(data,base)
      rm(base);gc()
      data$pct <- ((data$value - data$base)/abs(data$base))*100
      data$lvl <- data$value - data$base
      data$draw <- draw
      saveRDS(data,file=paste0(dir_drawcost,draw,"/draw_cost_",crop_yr,"_",draw,".rds"))
      return(ii)
    }, error = function(e){return(NULL)})
  })

