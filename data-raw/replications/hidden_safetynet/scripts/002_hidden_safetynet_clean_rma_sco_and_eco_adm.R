
rm(list=ls(all=TRUE));gc()

source("scripts/000_hidden_safetynet_helpers.R")

study_env <- setup_environment()

plan(list(tweak(multisession, workers = 4)))
data <- as.data.frame(
  data.table::rbindlist(
    future_lapply(
      year_beg:year_end,
      function(crop_yr){
        # crop_yr <- 2022
        
        adm <- get_compressed_adm_area(year = crop_yr, insurance_plan = c(31:33,35:36,87:89))
        
        adm <- readRDS(paste0(Dr.FCIP,"rmaActuarialDataMaster/Output/area_rate/area_rate_",crop_yr,".rds"))
        adm <- adm[adm$ins_plan_cd %in% c(31:33,35:36,87:89),]
        adm <- adm[!adm$base_rate %in% c(0,NA,NaN,Inf,-Inf),]
        setDT(adm)
        adm <- adm[,.(base_rate = mean(base_rate,na.rm=T)), 
                   by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","ins_plan_cd","cov_lvl")];gc()
        adm <- as.data.frame(adm)
        
        ayp <- readRDS(paste0(Dr.FCIP,"rmaActuarialDataMaster/Output/area_rate/area_rate_",crop_yr,".rds"))
        ayp <- ayp[ayp$ins_plan_cd %in% c(4:6),]
        ayp <- ayp[!ayp$base_rate %in% c(0,NA,NaN,Inf,-Inf),];setDT(ayp)
        ayp <- ayp[,.(base_rate = mean(base_rate,na.rm=T)), 
                   by = c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","ins_plan_cd","cov_lvl")];gc()
        ayp <- as.data.frame(ayp)
        ayp$cov_lvl <- paste0("ayp",round(ayp$cov_lvl*100))
        ayp <- ayp %>%  tidyr::spread(cov_lvl, base_rate) 
        ayp$ins_plan_cd <- ayp$ins_plan_cd + 27
        ayp <- dplyr::full_join(adm[adm$ins_plan_cd %in% 31:33,],ayp)
        ayp <- ayp[!ayp$base_rate %in% c(0,NA,NaN,Inf,-Inf),]
        
        # Increase SCO’s coverage level to 88%
        sco88 <- ayp
        sco88$base_rate_ayp <- sco88$base_rate + (sco88$ayp90 - sco88$ayp85)*((88-86)/(90-85))
        
        # Increase the coverage level of SCO to 90%.
        sco90 <- ayp
        sco90$base_rate_ayp <- sco90$base_rate + (sco90$ayp90 - sco90$ayp85)*((90-86)/(90-85))
        rm(ayp);gc()
        
        if(crop_yr>=2021){
          eco90 <- adm[(adm$ins_plan_cd %in% 87:89 & round(adm$cov_lvl*100) %in% 90),
                       c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","ins_plan_cd","base_rate")]
          names(eco90)[names(eco90) %in% "base_rate"] <- "eco90"
          eco90$ins_plan_cd <- eco90$ins_plan_cd - 56
          
          sco85 <- adm[(adm$ins_plan_cd %in% 31:33 & round(adm$cov_lvl*100) %in% 85),
                       c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","ins_plan_cd","base_rate")]
          names(sco85)[names(sco85) %in% "base_rate"] <- "sco85"
          
          # Increase SCO’s coverage level to 88%
          sco88 <- dplyr::full_join(sco88,eco90)
          sco88 <- sco88[!sco88$base_rate %in% c(0,NA,NaN,Inf,-Inf),]
          sco88 <- dplyr::full_join(sco88,sco85)
          sco88 <- sco88[!sco88$base_rate %in% c(0,NA,NaN,Inf,-Inf),]
          sco88$base_rate_eco <- sco88$base_rate + (sco88$eco90 - sco88$sco85)*((88-86)/(90-85))
          # sco88 <- dplyr::inner_join(sco88,sco88[1,c(c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","ins_plan_cd"))])
          # sco88 <- sco88[order(sco88$cov_lvl),]
          
          # Increase the coverage level of SCO to 90%.
          sco90 <- dplyr::full_join(sco90,eco90)
          sco90 <- sco90[!sco90$base_rate %in% c(0,NA,NaN,Inf,-Inf),]
          sco90 <- dplyr::full_join(sco90,sco85)
          sco90 <- sco90[!sco90$base_rate %in% c(0,NA,NaN,Inf,-Inf),]
          sco90$base_rate_eco <- sco90$base_rate + (sco90$eco90 - sco90$sco85)*((88-86)/(90-85))
          # sco90 <- dplyr::inner_join(sco90,sco90[1,c(c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","ins_plan_cd"))])
          # sco90 <- sco90[order(sco90$cov_lvl),]
          # sco90
          rm(eco90,sco85);gc()
        }
        
        sco88$ins_plan_cd <- sco88$ins_plan_cd + 10
        sco90$ins_plan_cd <- sco90$ins_plan_cd + 20
        sco88$base_rate <- rowMeans(sco88[names(sco88)[grepl("base_rate_",names(sco88))]],na.rm=T)
        sco90$base_rate <- rowMeans(sco90[names(sco90)[grepl("base_rate_",names(sco90))]],na.rm=T)
        adm <- rbind(adm,sco88[names(adm)],sco90[names(adm)])
        
        rm(sco90,sco88);gc()
        
        adm <- adm[!adm$base_rate %in% c(0,NA,NaN,Inf,-Inf),]
        
        #table(adm$ins_plan_cd)
        
        return(adm)}), fill = TRUE))
plan(sequential)
saveRDS(data,file ="data/rma_supplemental_premium_rates.rds")