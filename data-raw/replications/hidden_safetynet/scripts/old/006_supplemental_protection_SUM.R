
rm(list=ls(all=TRUE));gc()
library('magrittr');library(tidyverse);library(future.apply);library(data.table)
library(sandwich);library(lmtest)
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),paste0("/homes/",Sys.info()['user'],"/Articles/USA/supplemental_protection/")))
Keep.List<-c("Keep.List",ls())
source("000_supplemental_protection_FUN.R")
dir.create(paste0("output/summary/"))

function(){
  work_list <- unique(readRDS(paste0(dir_expected,"expected_2022.rds"))$combination)
  
  work_list <- rbind(
    data.frame(crop_yr_min=2015,crop_yr_max=2023,combination=work_list),
    data.frame(crop_yr_min=2021,crop_yr_max=2023,combination=work_list),
    as.data.frame(
      data.table::rbindlist(
        lapply(c(2015:2023),
               function(crop_yr){return(data.frame(crop_yr_min=crop_yr,crop_yr_max=crop_yr,
                                                   combination=c("Basic+CURENT","Basic only","Basic+SCO8665","Basic+SCO8665+ECO9544")))}), fill = TRUE)))

  work_list <- work_list[!paste0("summary_",gsub("__","_",gsub("[+]","_",work_list$combination)),"_",
                                 work_list$crop_yr_min,"_",work_list$crop_yr_max,".rds") %in%
                           list.files("output/summary/"),]
  #work_list <- work_list[work_list$combination %in% "Basic+SCO8680+",]
  saveRDS(work_list,file="work_summary.rds")
}

work_list <- readRDS("work_summary.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  1:nrow(work_list),
  function(ii){
    tryCatch({ 
      # ii <- 28
      
      data <- as.data.frame(
        data.table::rbindlist(
          lapply(
            work_list$crop_yr_min[ii]:work_list$crop_yr_max[ii],
            function(crop_yr){
              # crop_yr <- 2021
              tryCatch({ 
                if(work_list$combination[ii] %in% c("Basic+SCO8665+ECO9044","Basic+SCO8665+ECO9544") & crop_yr < 2021){
                  data <- list.files(dir_drawfarm,pattern = paste0("Basic_SCO8665_",crop_yr,"_"),recursive = T,full.names = T)
                }else{
                  data <- list.files(dir_drawfarm
                                     ,pattern = gsub("__","_",paste0(gsub("__","_",gsub("[+]","_",work_list$combination[ii])),"_",crop_yr,"_")),
                                     recursive = T,full.names = T)
                }
                
                data <- as.data.frame(
                  data.table::rbindlist(
                    lapply(
                      unique(data),
                      function(fl){
                        tryCatch({ 
                          return(readRDS(fl))
                        }, error = function(e){return(NULL)})
                        }), fill = TRUE))
                return(data)
              }, error = function(e){return(NULL)})
            }), fill = TRUE));gc();gc()
      
      data$combination <- work_list$combination[ii]
      data <- data[data$variable %in% c("its","rrs1","rrs2","rrs3","Simrate","SimrateP","Simsuby","rrp1","rrp2","rrp3","itp"),]
      setDT(data)
      
      data <- as.data.frame(data[, .(est = mean(value, na.rm = TRUE)),by = c("variable","combination","disag","level","aggregation","draw")])
      
      data <- as.data.frame(data)
      
      rrm <- data[(data$variable %in% c("rrp1","rrp2","rrp3","itp")),]
      SimrateP <- data[(data$variable %in% c("SimrateP")),c("aggregation","combination","disag","level","draw","est")]
      names(SimrateP) <- c("aggregation","combination","disag","level","draw","SimrateP")
      rrm <- dplyr::inner_join(SimrateP,rrm,by=c("aggregation","combination","disag","level","draw"));rm(SimrateP);gc();gc()
      rrm$est <- rrm$est/rrm$SimrateP
      rrm$variable <- gsub("p","m",rrm$variable)
      setDT(rrm)
      rrm <- as.data.frame(rrm[, .(est = mean(est, na.rm = TRUE)),by = c("variable","combination","disag","level","aggregation","draw")])
      
      data <- rbind(rrm,data);rm(rrm);gc();gc()
      
      setDT(data)
      
      data <- dplyr::inner_join(
        as.data.frame(data[draw %in% "0000",.(est = mean(est,na.rm=T)), by = c("variable","combination","disag","level","aggregation")]),
        as.data.frame(data[!draw %in% "0000",.(est_mean = mean(est,na.rm=T),est_se = sd(est,na.rm=T),
                                               est_n  = sum(as.numeric(!est %in% c(NA,Inf,NaN,-Inf)),na.rm=T)), 
                           by = c("variable","combination","disag","level","aggregation")]),
        by=c("variable","combination","disag","level","aggregation"));gc();gc()

      data$est_zv <- data$est/data$est_se
      data$est_pv <- round(2 * (1 - pt(abs(data$est_zv), df=data$est_n)),5)
      data$baseline <- paste0(work_list$crop_yr_min[ii],"-",work_list$crop_yr_max[ii])
      
      saveRDS(data,paste0("output/summary/summary_",gsub("__","_",gsub("[+]","_",work_list$combination[ii])),"_",
                          work_list$crop_yr_min[ii],"_",work_list$crop_yr_max[ii],".rds"))
      rm(data);gc();gc()
      #--------------------------------------------------
    return(ii)
    }, error = function(e){return(NULL)})
  })

function(){
  #--------------------------------------------------------------------------------------------------------------
  # Impact - main                                                                                             ####
  rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
  
  data <- list.files("output/summary",full.names = T)
  data <- unique(data[!grepl("ALT",data)])
  #data <- c(unique(data[grepl("_2015_2023.rds",data)]),unique(data[grepl("_2021_2023.rds",data)]))

  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        data,
        function(fl){
          data <- readRDS(fl)
          data <- data[data$disag %in% "FCIP",]
          data <- data[data$variable %in% c("its","rrs1","rrs2","rrs3","sner1","sner2","sner3","rrp1","rrp2","rrp3","itp","Simrate","Simsuby","SimrateP",
                                            "liability","costA","costB","costD","costG","costE","costF","costJ","lr",
                                            "rrm1","rrm2","rrm3","rrc1","rrc2","rrc3","rrx1","rrx2","rrx3"),]
          data <- data[data$aggregation %in% c("avg_valueT","value","avg_chglvl00T","chglvl","avg_chgpct00T","avg_chglvl01T","avg_chgpct01T","chgpct"),]
          return(data)
        }), fill = TRUE))
  saveRDS(data,file="output/figure_data/impact_main.rds")
  #--------------------------------------------------------------------------------------------------------------
  # Impact Heterogeneity 2021-Date                                                                            ####
  rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
  
  data <- list.files("output/summary",full.names = T)
  data <- unique(data[!grepl("ALT",data)])
  data <- c(unique(data[grepl("_2015_2023.rds",data)]),unique(data[grepl("_2021_2023.rds",data)]))
  data <- data[!grepl("Basic only",data)]
  data <- data[!grepl("Basic only",data)]
  
  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        data,
        function(fl){
          data <- readRDS(fl)
          data <- data[data$variable %in% c("rrm2","itp","rrp2","SimrateP"),]
          data <- data[data$aggregation %in% c("avg_chgpct00T","avg_valueT","avg_chglvl01T","avg_chgpct01T"),]
          return(data)
        }), fill = TRUE))

  saveRDS(data,file="output/figure_data/impact_heterogeneity.rds")
  #--------------------------------------------------------------------------------------------------------------
  # Impact incremental 2021-Date                                                                              ####
  rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
  data <- c(list.files("output/summary",full.names = T,pattern = "2015_2023.rds"))
  data <- data[ grepl("ALT",data)]
  
  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        data,
        function(fl){
          data <- readRDS(fl)
          data <- data[data$variable %in% c("its","rrs1","rrs2","rrs3","sner1","sner2","sner3","rrp1","rrp2","rrp3","itp","Simrate","Simsuby","SimrateP",
                                            "liability","costA","costB","costD","costG","costE","costF","costJ","lr",
                                            "rrm1","rrm2","rrm3","rrc1","rrc2","rrc3","rrx1","rrx2","rrx3"),]
          data <- data[data$aggregation %in% c("avg_valueT","value","avg_chglvl00T","chglvl","avg_chgpct00T","chgpct"),]
          data <- data[data$disag %in% "FCIP",]
          return(data)
        }), fill = TRUE))
  
  data$combination <- as.numeric(gsub("[^0-9]","",data$combination))
  
  saveRDS(data,file="output/figure_data/impact_incremental.rds")
  #--------------------------------------------------------------------------------------------------------------
}

