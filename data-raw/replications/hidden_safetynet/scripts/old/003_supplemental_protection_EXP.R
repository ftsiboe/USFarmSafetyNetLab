
rm(list=ls(all=TRUE));gc();gc()
library('magrittr');library(tidyverse);library(future.apply);library(data.table)
library(sandwich);library(lmtest)
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),paste0("/homes/",Sys.info()['user'],"/Articles/USA/supplemental_protection/")))
source("000_supplemental_protection_FUN.R")
Keep.List<-c("Keep.List",ls())

if(Sys.getenv("SLURM_JOB_NAME") %in% "SUP_EXP01"){

  work_list <-as.data.frame(
    data.table::rbindlist(
      lapply(
        as.numeric(list.files(dir_sim)),
        function(crop_yr){
          dir.create(paste0(dir_expected,crop_yr,"/"))
          return(data.frame(crop_yr=crop_yr,task=1:500))
        }),fill = TRUE))
  
  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  }
  #work_list <- work_list[work_list$crop_yr %in% 2022,]
  
  lapply(
    1:nrow(work_list),
    function(work){
      tryCatch({ 
        # work <- 3585
        crop_yr <- work_list$crop_yr[work]
        task    <- work_list$task[work]
        
        if(!paste0("expected_",crop_yr,"_",task,".rds") %in% list.files(paste0(dir_expected,crop_yr))){
          
          data <- as.data.frame(readRDS(paste0("data/agentdata/agentdata",crop_yr,".rds")))[
            c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl","comm_amt")]
          data$task <- rep(1:500,length=nrow(data))
          
          data <- data[data$task %in% task,]
          
          data <- as.data.frame(
            data.table::rbindlist(
              lapply(list.files(paste0(dir_sim,crop_yr,"/"),full.names = T),
                     function(sim){
                       tryCatch({
                         return(dplyr::inner_join(readRDS(sim),data,by=c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl")))
                       }, error = function(e){return(NULL)})
                     }),fill = TRUE));gc();gc()
          
          uid <- c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl","combination","comm_amt")
          
          setDT(data)
          
          data[, `:=`(Revenue_Inc = pmax(0, revenue + indemnity - producer_premium),
                      Revenue = pmax(0, revenue))]
          
          Expected <- data[, .(Revenue.mean = mean(Revenue, na.rm = TRUE),
                               Revenue.sd = sd(Revenue, na.rm = TRUE),
                               Revenue_Inc.mean = mean(Revenue_Inc, na.rm = TRUE),
                               Revenue_Inc.sd = sd(Revenue_Inc, na.rm = TRUE)), by = uid];gc();gc()
          
          for(var in c("Revenue", "Revenue_Inc")){
            Expected[, (paste0(var,".sd")) := fifelse(get(paste0(var,".sd")) %in% c(Inf, -Inf, NaN), NA_real_, get(paste0(var,".sd")))]
            Expected[, (paste0(var,".mean")) := fifelse(get(paste0(var,".mean")) %in% c(Inf, -Inf, NaN), NA_real_, get(paste0(var,".mean")))]
          }
          
          LPM <- merge(data, Expected, by = uid, all.x = TRUE)
          
          # Compute new variables for each 'var'
          for (var in c("Revenue", "Revenue_Inc")) {
            
            # Compute residuals
            LPM[, (paste0(var, ".res")) := get(var) - get(paste0(var, ".mean"))]
            
            # Compute squared residuals if residuals are negative; NA otherwise
            LPM[, (paste0(var, ".lres2")) := fifelse(get(paste0(var, ".res")) > 0, NA_real_, (get(paste0(var, ".res")))^2)]
            
            # Compute cdf indicator
            LPM[, (paste0(var, ".cdf")) := fifelse(get(paste0(var, ".res")) < 0, 1L, 0L)]
          }
          
          # Aggregating measures of income risk
          LPM <- LPM %>% group_by(across(all_of(uid))) %>%
            summarise(across(all_of(c(names(LPM)[grepl(".lres2",names(LPM))],
                                      names(LPM)[grepl(".cdf",names(LPM))])), ~ mean(., na.rm = TRUE),
                             .names = "{.col}")) %>% ungroup()
          setDT(LPM)
          
          # Join 'Expected' with aggregated 'LPM'
          Expected <- merge(Expected, LPM, by = uid, all.x = TRUE);rm(LPM);gc()
          
          # Compute additional measures
          for (var in c("Revenue", "Revenue_Inc")) {
            # Variance, Coefficient of Variation, and other measures
            Expected[, paste0(var, ".var") := get(paste0(var, ".sd"))^2]
            Expected[, paste0(var, ".cv") := fifelse(get(paste0(var, ".sd")) %in% c(Inf, -Inf, NaN), NA_real_, get(paste0(var, ".sd")) / get(paste0(var, ".mean")))]
            Expected[, paste0(var, ".lapv") := get(paste0(var, ".lres2"))]
            Expected[, paste0(var, ".lrpv") := get(paste0(var, ".lapv")) / get(paste0(var, ".cdf"))]
            Expected[, paste0(var, ".nlapv") := get(paste0(var, ".lapv")) / get(paste0(var, ".mean"))]
            Expected[, paste0(var, ".nlrpv") := get(paste0(var, ".lrpv")) / get(paste0(var, ".mean"))]
          }
          
          # Relative measures
          Expected[, `:=`(Relmean = Revenue_Inc.mean / Revenue.mean,
                          Relsd = Revenue_Inc.sd / Revenue.sd,
                          Relcv = Revenue_Inc.cv / Revenue.cv,
                          Rellapv = Revenue_Inc.lapv / Revenue.lapv,
                          Rellrpv = Revenue_Inc.lrpv / Revenue.lrpv,
                          Relnlapv = Revenue_Inc.nlapv / Revenue.nlapv,
                          Relnlrpv = Revenue_Inc.nlrpv / Revenue.nlrpv,
                          Relvar = Revenue_Inc.var / Revenue.var)];gc();gc()
          
          # upper_limits <- Expected[, lapply(.SD, function(x) quantile(unique(x[!x %in% c(-Inf,Inf,NaN,NA)]), probs = 0.95, na.rm = TRUE)), 
          #                          by = c("crop_yr","state_cd","crop_cd","typ_cd","combination"), 
          #                          .SDcols = c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")]
          # names(upper_limits) <- c("crop_yr","state_cd","crop_cd","typ_cd","combination",
          #                      paste0("upper_",c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")))
          # Expected <- merge(Expected, upper_limits, by = intersect(names(Expected),names(upper_limits)), all = TRUE);rm(upper_limits);gc()
          # 
          # lower_limits <- Expected[, lapply(.SD, function(x) quantile(unique(x[!x %in% c(-Inf,Inf,NaN,NA)]), probs = 0.05, na.rm = TRUE)), 
          #                          by = c("crop_yr","state_cd","crop_cd","typ_cd","combination"), 
          #                          .SDcols = c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")]
          # names(lower_limits) <- c("crop_yr","state_cd","crop_cd","typ_cd","combination",
          #                      paste0("lower_",c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")))
          # Expected <- merge(Expected, lower_limits, by = intersect(names(Expected),names(lower_limits)), all = TRUE);rm(lower_limits);gc()
          # 
          # 
          # Expected[, `:=`(Relmean  = ifelse(Relmean >upper_Relmean,upper_Relmean,Relmean),
          #                 Relsd    = ifelse(Relsd   >upper_Relsd,upper_Relsd,Relsd),
          #                 Relcv    = ifelse(Relcv   >upper_Relcv,upper_Relcv,Relcv),
          #                 Rellapv  = ifelse(Rellapv >upper_Rellapv,upper_Rellapv,Rellapv),
          #                 Rellrpv  = ifelse(Rellrpv >upper_Rellrpv,upper_Rellrpv,Rellrpv),
          #                 Relnlapv = ifelse(Relnlapv>upper_Relnlapv,upper_Relnlapv,Relnlapv),
          #                 Relnlrpv = ifelse(Relnlrpv>upper_Relnlrpv,upper_Relnlrpv,Relnlrpv),
          #                 Relvar   = ifelse(Relvar  >upper_Relvar,upper_Relvar,Relvar))]
          # 
          # Expected[, `:=`(Relmean  = ifelse(Relmean <lower_Relmean,lower_Relmean,Relmean),
          #                 Relsd    = ifelse(Relsd   <lower_Relsd,lower_Relsd,Relsd),
          #                 Relcv    = ifelse(Relcv   <lower_Relcv,lower_Relcv,Relcv),
          #                 Rellapv  = ifelse(Rellapv <lower_Rellapv,lower_Rellapv,Rellapv),
          #                 Rellrpv  = ifelse(Rellrpv <lower_Rellrpv,lower_Rellrpv,Rellrpv),
          #                 Relnlapv = ifelse(Relnlapv<lower_Relnlapv,lower_Relnlapv,Relnlapv),
          #                 Relnlrpv = ifelse(Relnlrpv<lower_Relnlrpv,lower_Relnlrpv,Relnlrpv),
          #                 Relvar   = ifelse(Relvar  <lower_Relvar,lower_Relvar,Relvar))]
          # 
          # Expected <- Expected[, .SD, .SDcols = !patterns("lower_")]
          # Expected <- Expected[, .SD, .SDcols = !patterns("upper_")]
          
          Expected <- Expected[, .SD, .SDcols = !patterns("Revenue")];gc();gc()
          
          data[, `:=`(Simrate = total_premium / liability,
                      SimrateP = (total_premium - subsidy_amount) / liability,
                      Simsuby = subsidy_amount / total_premium,
                      Simlcr = indemnity / liability,
                      lr_indemnity = sum(indemnity),
                      lr_premium = sum(total_premium))];gc();gc()
          
          data <- data %>% group_by(across(all_of(uid))) %>%
            summarise(across(all_of(c("liability","total_premium","subsidy_amount",
                                      "producer_premium","indemnity","Simrate","SimrateP","Simsuby","Simlcr","lr_indemnity","lr_premium",
                                      "Revenue","Revenue_Inc")), ~ mean(., na.rm = TRUE),
                             .names = "{.col}")) %>% ungroup()
          setDT(data)
          
          Expected <- merge(Expected, data, by = uid, all = TRUE);rm(data);gc();gc()
          
          saveRDS(as.data.frame(Expected),paste0(dir_expected,crop_yr,"/expected_",crop_yr,"_",task,".rds"))
          
          rm(Expected);gc();gc()
          
        }
        
        return(crop_yr)
      }, error = function(e){return(NULL)})
    })
  
}

if(Sys.getenv("SLURM_JOB_NAME") %in% "SUP_EXP02"){
  
  function(){
    lapply(
      as.numeric(list.files(dir_sim)),
      function(crop_yr){
        return(print(paste0(crop_yr,"=", length(list.files(path=paste0(dir_expected,crop_yr),recursive = T,full.names = T)))))
      })
  }
  
  work_list <- as.numeric(list.files(dir_expected))
  work_list <- work_list[! work_list %in% NA]
  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))]
  }
  
  lapply(
    work_list,
    function(crop_yr){
      
      # crop_yr <- 2015
      
      Expected <- as.data.frame(
        data.table::rbindlist(
          lapply(
            list.files(path=paste0(dir_expected,crop_yr),recursive = T,full.names = T),
            function(expected){
              tryCatch({ 
                return(readRDS(expected))
              }, error = function(e){return(NULL)})
            }), fill = TRUE))
      
      setDT(Expected)
      
      upper_limits <- Expected[, lapply(.SD, function(x) quantile(unique(x[!x %in% c(-Inf,Inf,NaN,NA)]), probs = 0.95, na.rm = TRUE)),
                               by = c("crop_yr","state_cd","crop_cd","typ_cd","combination"),
                               .SDcols = c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")]
      names(upper_limits) <- c("crop_yr","state_cd","crop_cd","typ_cd","combination",
                               paste0("upper_",c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")))
      Expected <- merge(Expected, upper_limits, by = intersect(names(Expected),names(upper_limits)), all = TRUE);rm(upper_limits);gc()
      
      lower_limits <- Expected[, lapply(.SD, function(x) quantile(unique(x[!x %in% c(-Inf,Inf,NaN,NA)]), probs = 0.05, na.rm = TRUE)),
                               by = c("crop_yr","state_cd","crop_cd","typ_cd","combination"),
                               .SDcols = c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")]
      names(lower_limits) <- c("crop_yr","state_cd","crop_cd","typ_cd","combination",
                               paste0("lower_",c("Relmean","Relsd","Relcv","Rellapv","Rellrpv","Relnlapv","Relnlrpv","Relvar")))
      Expected <- merge(Expected, lower_limits, by = intersect(names(Expected),names(lower_limits)), all = TRUE);rm(lower_limits);gc()
      
      Expected[, `:=`(Relmean  = ifelse(Relmean >upper_Relmean,upper_Relmean,Relmean),
                      Relsd    = ifelse(Relsd   >upper_Relsd,upper_Relsd,Relsd),
                      Relcv    = ifelse(Relcv   >upper_Relcv,upper_Relcv,Relcv),
                      Rellapv  = ifelse(Rellapv >upper_Rellapv,upper_Rellapv,Rellapv),
                      Rellrpv  = ifelse(Rellrpv >upper_Rellrpv,upper_Rellrpv,Rellrpv),
                      Relnlapv = ifelse(Relnlapv>upper_Relnlapv,upper_Relnlapv,Relnlapv),
                      Relnlrpv = ifelse(Relnlrpv>upper_Relnlrpv,upper_Relnlrpv,Relnlrpv),
                      Relvar   = ifelse(Relvar  >upper_Relvar,upper_Relvar,Relvar))]
      
      Expected[, `:=`(Relmean  = ifelse(Relmean <lower_Relmean,lower_Relmean,Relmean),
                      Relsd    = ifelse(Relsd   <lower_Relsd,lower_Relsd,Relsd),
                      Relcv    = ifelse(Relcv   <lower_Relcv,lower_Relcv,Relcv),
                      Rellapv  = ifelse(Rellapv <lower_Rellapv,lower_Rellapv,Rellapv),
                      Rellrpv  = ifelse(Rellrpv <lower_Rellrpv,lower_Rellrpv,Rellrpv),
                      Relnlapv = ifelse(Relnlapv<lower_Relnlapv,lower_Relnlapv,Relnlapv),
                      Relnlrpv = ifelse(Relnlrpv<lower_Relnlrpv,lower_Relnlrpv,Relnlrpv),
                      Relvar   = ifelse(Relvar  <lower_Relvar,lower_Relvar,Relvar))]
      
      Expected <- Expected[, .SD, .SDcols = !patterns("lower_")]
      Expected <- Expected[, .SD, .SDcols = !patterns("upper_")]
      
      saveRDS(as.data.frame(Expected),paste0(dir_expected,"expected_",crop_yr,".rds"))
      
      rm(Expected);gc();gc()
      
      return(crop_yr)
    })
}

