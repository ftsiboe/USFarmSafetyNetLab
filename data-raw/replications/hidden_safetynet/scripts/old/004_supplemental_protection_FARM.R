
rm(list=ls(all=TRUE));gc();gc()
library('magrittr');library(tidyverse);library(future.apply);library(data.table)
library(sandwich);library(lmtest)
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),paste0("/homes/",Sys.info()['user'],"/Articles/USA/supplemental_protection/")))
Keep.List<-c("Keep.List",ls())
source("000_supplemental_protection_FUN.R")

function(){
  unlink(dir_drawfarm,recursive = T)
  
  dir.create(dir_drawfarm)
  lapply(
    0:100,
    function(d){
      dir.create(paste0(dir_drawfarm,stringr::str_pad(d,pad="0",4)))
      return(d)
    })
}

if(! "drawlist.rds" %in% list.files(paste0(getwd(),"/output"))){
  set.seed(12262024)
  
  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        list.files(dir_expected,full.names = T,pattern = "expected_"),
        function(expected){
          tryCatch({ 
            return(unique(readRDS(expected)[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd")]))
          }, error = function(e){return(NULL)})
        }), fill = TRUE))
  
  data$combination <- 1
  
  pract <- readRDS(paste0(Dr.FCIP,"rmaActuarialDataMaster/Output/recodes/recodes_pract.rds"))
  pract$NONIRR   <- as.numeric(pract$irrgate_rc %in% "NON-IRR")
  pract$IRR      <- as.numeric(pract$irrgate_rc %in% "IRR" )
  pract$IRROther <- as.numeric(pract$irrgate_rc %in% "Other" )
  pract$CERTIFIED    <- as.numeric(pract$organic_rc %in% "ORGANIC (CERTIFIED)" )
  pract$TRANSITIONAL <- as.numeric(pract$organic_rc %in% "ORGANIC (TRANSITIONAL)")
  pract$ORGANICOther <- as.numeric(pract$organic_rc %in% "NO PRACTICE SPECIFED" )
  pract <- doBy::summaryBy(list(c("NONIRR","IRR","IRROther","CERTIFIED","TRANSITIONAL","ORGANICOther"),
                                c("crop_yr","crop_cd","pract_cd")),data=pract,FUN=max,keep.names = T,na.rm=T)
  data <- dplyr::full_join(pract,data,by=c("crop_yr","crop_cd","pract_cd"))
  data$irrgate_rc <- ifelse(data$IRR %in% 1,"Irrigated","Unspecified")
  data$irrgate_rc <- ifelse(data$irrgate_rc %in% "Unspecified" & data$NONIRR %in% 1,"Nonirrigated",data$irrgate_rc)
  data$organic_rc <- ifelse(data$CERTIFIED  %in% 1,"Organic (Certified)","Unspecified")
  data$organic_rc <- ifelse(data$organic_rc %in% "Unspecified" & data$TRANSITIONAL %in% 1,"Organic (Transitional)",data$organic_rc)
  data$IRRG <- as.character(data$irrgate_rc)
  data$ORGC <- as.character(data$organic_rc)
  data <- data[!data$combination %in% c(NaN,Inf,-Inf,NA),]
  rm(pract);gc();gc()
  
  data <- unique(data[c("state_cd","county_cd","crop_cd","typ_cd","pract_cd","IRR","ORGC","combination")]);gc();gc()

  data$COUNTY <- as.numeric(paste0(stringi::stri_pad(data$state_cd,pad="0",2),stringi::stri_pad(data$county_cd,pad="0",3)))
  ERSReg <- as.data.frame(readxl::read_excel(paste0(Dr.POLYG,"/County-to-ERS Resource Region aggregation.xls")))
  names(ERSReg)[names(ERSReg) %in% "name"] <- "ERSReg"
  names(ERSReg)[names(ERSReg) %in% "id"]   <- "ERSReg_cd"
  names(ERSReg)[names(ERSReg) %in% "county_fips"]   <- "COUNTY"
  data <- dplyr::full_join(data,ERSReg[c("COUNTY","ERSReg","ERSReg_cd")],by="COUNTY")
  data <- data[!data$combination %in% c(NaN,Inf,-Inf,NA),]
  rm(ERSReg);gc();gc()
  
  data$COUNTY <- paste0(stringi::stri_pad(data$state_cd,pad="0",2),stringi::stri_pad(data$county_cd,pad="0",3))
  
  crd <- as.data.frame(readr::read_csv(paste0(Dr.POLYG,"NASS County and District Codes.csv"),skip = 2))
  crd <- crd[3:nrow(crd),]
  names(crd) <- c("state_cd","CRD","county_cd","county","flag")
  crd <- crd[as.numeric(as.character(crd$flag)) %in% 1,]
  crd$county_cd <- as.numeric(as.character(crd$county_cd))
  crd$state_cd <- as.numeric(as.character(crd$state_cd))
  crd$CRD <- paste0(stringr::str_pad(crd$state_cd,pad="0",2),stringr::str_pad(as.numeric(as.character(crd$CRD)),pad="0",3))
  data <- dplyr::full_join(data,crd[c("state_cd","county_cd","CRD")],by=c("state_cd","county_cd"))
  data <- data[!data$combination %in% c(NaN,Inf,-Inf,NA),]
  rm(crd);gc();gc()
  
  data <- unique(data[c("state_cd","county_cd","crop_cd","typ_cd","pract_cd",
                        "IRR","ORGC","ERSReg","CRD","COUNTY")]);gc();gc()
  
  data$STATE  <- data$state_cd
  data$FCIP  <- 0
  data$CROP <- data$crop_cd
  data$MCROP <- ifelse(data$crop_cd %in% c(11,18,21,41,81),data$crop_cd,0)
  
  data$CROP_STATE  <- paste0(data$crop_cd,"-",data$STATE)
  data$MCROP_STATE <- paste0(data$MCROP,"-",data$STATE)
  
  data$CROP_ERSReg  <- paste0(data$crop_cd,"-",data$ERSReg)
  data$MCROP_ERSReg <- paste0(data$MCROP,"-",data$ERSReg)
  
  data$CROP_CRD  <- paste0(data$crop_cd,"-",data$CRD)
  data$MCROP_CRD <- paste0(data$MCROP,"-",data$CRD)
  
  data <- lapply(
    0:100,
    function(d){
      draw <- data
      if(!d %in% 0){ draw <- data[sample(1:nrow(data), nrow(data), replace=TRUE), ]}
      draw$drawID <- 1:nrow(draw)
      return(draw)
    })
  names(data) <- stringr::str_pad(0:100,pad="0",4)
  saveRDS(data,file="output/drawlist.rds");rm(data) ;gc();gc()
}

function(){
  
  work_list <- unique(readRDS(paste0(dir_expected,"expected_2022.rds"))$combination)
  
  year_list <- as.numeric(list.files(dir_expected))
  year_list <- year_list[! year_list %in% NA]
  
  work_list <- as.data.frame(
    data.table::rbindlist(
      lapply(year_list,
             function(crop_yr){return(data.frame(crop_yr=crop_yr,combination=work_list))}), fill = TRUE))
  
  work_list <- work_list[!(work_list$crop_yr<= 2020 & grepl("ECO",work_list$combination)),]
  
  table(work_list$combination,work_list$crop_yr)
  
  work_list <- as.data.frame(
    data.table::rbindlist(
      lapply(0:100,
             function(draw){
               dir.create(paste0(dir_drawfarm,stringr::str_pad(draw,pad="0",4)))
               return(data.frame(draw=stringr::str_pad(draw,pad="0",4),work_list))}), fill = TRUE))
  
  work_list <- work_list[!gsub("__","_",paste0(work_list$draw,"/",gsub("[+]","_",work_list$combination),"_",
                                               work_list$crop_yr,"_",work_list$draw,".rds")) %in%
                           list.files(dir_drawfarm,recursive = T),]
  #3280
  saveRDS(work_list,file="work_list_farm.rds")
}

work_list <- readRDS("work_list_farm.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  work_list <- work_list[as.numeric(work_list$draw) %in% as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  #work_list <- work_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

gc()
plan(list(tweak(multisession, workers = 8)))
future_lapply(
  1:nrow(work_list),
  function(ii){
    tryCatch({ 
      # ii <- 1
      combo <- work_list$combination[ii]
      draw<- work_list$draw[ii]
      crop_yr<- work_list$crop_yr[ii]
      if(!gsub("__","_",paste0(gsub("[+]","_",combo),"_",crop_yr,"_",draw,".rds")) %in% 
         list.files(paste0(dir_drawfarm,draw))){
        
        data <- readRDS(paste0(dir_expected,"expected_",crop_yr,".rds"))
        data <- data[data$combination %in% unique(c("Basic+CURENT",combo,"Basic+SCO8665")),];gc();gc()
        
        # data <- data[data$crop_cd %in% 41,]
        
        data$rrs1 <- data$Relcv
        data$rrs2 <- data$Relnlrpv
        data$rrs3 <- data$Relnlapv
        
        data$Irrs1 <- data$rrs1<1
        data$Irrs2 <- data$rrs2<1
        data$Irrs3 <- data$rrs3<1
        
        data$its  <- data$Relmean
        data$Iits <- data$its>1
        
        data$sner1 <- -data$Irrs1*data$Iits*((data$rrs1-1)/(data$its-1))
        data$sner2 <- -data$Irrs2*data$Iits*((data$rrs2-1)/(data$its-1))
        data$sner3 <- -data$Irrs3*data$Iits*((data$rrs3-1)/(data$its-1))
        
        data$rrp1 <- -100*(data$rrs1-1)
        data$rrp2 <- -100*(data$rrs2-1)
        data$rrp3 <- -100*(data$rrs3-1)
        data$itp  <-  100*(data$its-1)
        
        data$Simrate <- data$Simrate*100
        data$Simsuby <- data$Simsuby*100
        data$SimrateP <- data$SimrateP*100
        data$Simlcr <- data$Simlcr*100
        
        # data$rrx1 <- data$rrp1/data$SimrateP
        # data$rrx2 <- data$rrp2/data$SimrateP
        # data$rrx3 <- data$rrp3/data$SimrateP
        
        xlist <- c("its","Iits","rrs1","rrs2","rrs3","Irrs1","Irrs2","Irrs3","sner1","sner2","sner3",
                   "Simrate","SimrateP","Simsuby","Simlcr","rrp1","rrp2","rrp3","itp") #,"rrx1","rrx2","rrx3"
        
        data <- data[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd",
                       "ins_plan_cd","cov_lvl","combination","comm_amt",xlist)];gc();gc()
        
        data <- data %>% tidyr::gather(variable, value,c(xlist))
        data <- data[!data$value %in% c(NA,Inf,-Inf,NaN),];gc();gc()
        
        setDT(data)
        data <- as.data.frame(data[,.(value = mean(value,na.rm=T)),
                                   by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd",
                                          "ins_plan_cd","cov_lvl","combination","variable","comm_amt")]);gc();gc()
        
        if(!combo %in% "Basic+CURENT"){
          base <- data[data$combination %in% "Basic+CURENT",]
          names(base)[names(base) %in% "value"] <- "base00"
          base <- base[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl","variable","base00")]
          data <- dplyr::inner_join(data,base)
          rm(base);gc();gc()
        }
        
        if(!combo %in% c("Basic+SCO8665","Basic+CURENT")){
          base <- data[data$combination %in% "Basic+SCO8665",]
          names(base)[names(base) %in% "value"] <- "base01"
          base <- base[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl","variable","base01")]
          data <- dplyr::inner_join(data,base)
          rm(base);gc();gc()
        }
        
        data <- data[data$combination %in% combo,];gc();gc()
        
        data$PLAN <- ifelse(data$ins_plan_cd %in% c(1,90),"Yield",NA)
        data$PLAN <- ifelse(data$ins_plan_cd %in% c(2,3),"Revenue",data$PLAN)
        data$RPYP <- ifelse(data$ins_plan_cd %in% c(1,90),"YP",NA)
        data$RPYP <- ifelse(data$ins_plan_cd %in% c(2),"RP",data$RPYP)
        data$RPYP <- ifelse(data$ins_plan_cd %in% c(3),"RPHPE",data$RPYP)
        data$COV <- ifelse(round(data$cov_lvl*100) %in% c(50,55),"50-55%",NA)
        data$COV <- ifelse(round(data$cov_lvl*100) %in% c(60,65),"60-65%",data$COV)
        data$COV <- ifelse(round(data$cov_lvl*100) %in% c(70,75),"70-75%",data$COV)
        data$COV <- ifelse(round(data$cov_lvl*100) %in% c(80,85),"80-85%",data$COV)
        data$STRUCT <- ifelse(data$struct_cd %in% c("EU","WU","EP","EC"),"EU","OU")
        data$STRUCT <- ifelse(data$struct_cd %in% c("OU","UA","UD"),"OU",data$STRUCT)
        data$STRUCT <- ifelse(data$struct_cd %in% "BU","BU",data$STRUCT)

        if(!combo %in% "Basic+CURENT"){
          data$chglvl00 <- data$value - data$base00
          data$chgpct00 <- ((data$value - data$base00)/abs(data$base00))*100
        }
        
        if(!combo %in% c("Basic+SCO8665","Basic+CURENT")){
          data$chglvl01 <- data$value - data$base01
          data$chgpct01 <- ((data$value - data$base01)/abs(data$base01))*100
        }
        
        data <- data[names(data)[!names(data) %in% c("base00","base01")]];gc();gc()
        
        data <- data[!data$value %in% c(NaN,Inf,-Inf,NA),];gc();gc()
        
        data <- dplyr::inner_join(readRDS("output/drawlist.rds")[[draw]],data,
                                  by=c("state_cd","county_cd","crop_cd","typ_cd","pract_cd"))
        data <- data[!data$combination %in% c(NaN,Inf,-Inf,NA),];gc();gc()

        for(xxx in c("chgpct01","chglvl01","chgpct00","chglvl00")){if(! xxx %in% names(data)){data[,xxx] <- NA}}
        
        
        # data00 <- data[data$variable %in% "SimrateP",]
        # for(xxx in c("chgpct01","chglvl01","chgpct00","chglvl00","value","variable")){
        #   names(data00)[names(data00) %in% xxx] <- paste0("SimrateP_",xxx)
        # }
        # 
        # data01 <- data[data$variable %in% c("rrp1","rrp2","rrp3"),]
        # 
        # data01 <- dplyr::inner_join(data01,data00);rm(data00);gc();gc()
        # 
        # for(xxx in c("chgpct01","chglvl01","chgpct00","chglvl00","value")){
        #   data01[,xxx] <- data01[,xxx]/data01[,paste0("SimrateP_",xxx)]
        # }
        # 
        # data01 <- data01[names(data01)[!grepl("SimrateP_",names(data01))]]
        # data01$variable <- gsub("rrp","rrm",data01$variable)
        # 
        # data <- rbind(data,data01);rm(data01);gc();gc()
        
        setDT(data)
        
        data_limits <- data[, .(ll_nn  = length(value),
                                ll_value = quantile(value, probs = 0.05,na.rm=T),
                                ul_value = quantile(value, probs = 0.95,na.rm=T),
                                
                                ll_chglvl00 = quantile(chglvl00, probs = 0.05,na.rm=T),
                                ul_chglvl00 = quantile(chglvl00, probs = 0.95,na.rm=T),
                                
                                ll_chgpct00 = quantile(chgpct00, probs = 0.05,na.rm=T),
                                ul_chgpct00 = quantile(chgpct00, probs = 0.95,na.rm=T),
                                
                                ll_chglvl01 = quantile(chglvl01, probs = 0.05,na.rm=T),
                                ul_chglvl01 = quantile(chglvl01, probs = 0.95,na.rm=T),
                                
                                ll_chgpct01 = quantile(chgpct01, probs = 0.05,na.rm=T),
                                ul_chgpct01 = quantile(chgpct01, probs = 0.95,na.rm=T)),
                            by = c("variable","combination","state_cd","IRR","crop_cd")]
        
        data_limits <- data_limits[ll_nn >= 20]
        
        data <- dplyr::inner_join(as.data.frame(data),as.data.frame(data_limits),
                                  by = c("variable","combination","state_cd","IRR","crop_cd"));rm(data_limits);gc();gc()
        
        setDT(data)
        
        data[, chgpct00T := ifelse(chgpct00  <= -100  , -100  ,chgpct00)]
        data[, chgpct00T := ifelse(chgpct00T >= ul_chgpct00, ul_chgpct00,chgpct00T)]
        
        data[, chglvl00T := ifelse(chglvl00  <= ll_chglvl00, ll_chglvl00,chglvl00)]
        data[, chglvl00T := ifelse(chglvl00T >= ul_chglvl00, ul_chglvl00,chglvl00T)]
        
        data[, chgpct01T := ifelse(chgpct01  <= -100  , -100  ,chgpct01)]
        data[, chgpct01T := ifelse(chgpct01T >= ul_chgpct01, ul_chgpct01,chgpct01T)]
        
        data[, chglvl01T := ifelse(chglvl01  <= ll_chglvl01, ll_chglvl01,chglvl01)]
        data[, chglvl01T := ifelse(chglvl01T >= ul_chglvl01, ul_chglvl01,chglvl01T)]
        
        data[, valueT := ifelse(value  <= ll_value, ll_value,value)]
        data[, valueT := ifelse(valueT >= ul_value, ul_value,valueT)]
        
        data <- as.data.frame(data)
        
        data <- as.data.frame(
          data.table::rbindlist(
            lapply(
              c("FCIP","CROP","STATE","CROP_STATE","ERSReg","CRD","COUNTY","ins_plan_cd","PLAN","RPYP","cov_lvl","COV","STRUCT","struct_cd"),
              function(disag){
                tryCatch({ 
                  # disag <- "cov_lvl"
                  data$level <- data[,disag]
                  setDT(data)
  
                  data_avg <- data[, lapply(.SD, function(x)  weighted.mean(x,w=comm_amt, na.rm = TRUE)), by = c("variable","combination","level"),
                                   .SDcols = c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T")]
                  
                  data_med <- data[, lapply(.SD, function(x)  matrixStats::weightedMedian(x,w=comm_amt, na.rm = TRUE)), by = c("variable","combination","level"),
                                   .SDcols = c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T")]
                   
                  setnames(data_avg,  old = c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T"), 
                           new = paste0("avg_",c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T")))
                  
                  setnames(data_med,  old = c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T"), 
                           new = paste0("med_",c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T")))
                 
                  data_avg <- as.data.frame(data_avg) %>% 
                    tidyr::gather(aggregation, value, c(paste0("avg_",c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T"))))
                  
                  data_med <- as.data.frame(data_med) %>% 
                    tidyr::gather(aggregation, value, c(paste0("med_",c("value","valueT","chglvl00","chgpct00","chglvl01","chgpct01","chgpct00T","chglvl00T","chgpct01T","chglvl01T"))))
                  
                  data <- rbind(as.data.frame(data_avg),as.data.frame(data_med))
                  
                  data$disag <- disag
                  
                  data <- data[!data$value %in% c(NaN,Inf,-Inf,NA),c("variable","combination","disag","level","aggregation","value")];gc();gc()

                  return(data)
                }, error = function(e){return(NULL)})
              }), fill = TRUE))
        
        data$draw <- draw
        data$crop_yr <- crop_yr
        
        saveRDS(data,file=gsub("__","_",paste0(dir_drawfarm,draw,"/",gsub("[+]","_",combo),"_",crop_yr,"_",draw,".rds")))
        rm(data);gc();gc()
        
      }
      return(ii)
    }, error = function(e){return(NULL)})
  })
plan(sequential)
