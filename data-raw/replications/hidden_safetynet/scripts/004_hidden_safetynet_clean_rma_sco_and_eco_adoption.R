
rm(list=ls(all=TRUE));gc()

source("scripts/000_hidden_safetynet_helpers.R")

study_env <- setup_environment()

rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
us_sf <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)
cty_sf <- urbnmapr::get_urbn_map(map = "counties", sf = TRUE)
cty_sf$county_fips <- as.character(cty_sf$county_fips)

rma_adm <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2015:2024,
      function(cy){
        # cy <- 2016
        rma_adm <- readRDS(paste0(Dr.FCIP,"rmaActuarialDataMaster/Archive/",cy,"/",cy,"_A00030_InsuranceOffer_YTD.rds"))
        rma_adm <- rma_adm[rma_adm$Insurance.Plan.Code %in% c(87:89,31:33,1:3,90),]
        names(rma_adm) <- fcip_names(names(rma_adm))
        setDT(rma_adm)
        if(cy >= 2021){
          rma_adm <- rbind(rma_adm[ins_plan_cd %in% c(1:3,90),.(avail = 1,plan = "avail_aph")  , by = c("crop_yr","state_cd","county_cd","crop_cd")],
                           rma_adm[ins_plan_cd %in% 31:33,.(avail = 1,plan = "avail_sco")  , by = c("crop_yr","state_cd","county_cd","crop_cd")],
                           rma_adm[ins_plan_cd %in% 87:89,.(avail = 1,plan = "avail_eco90"), by = c("crop_yr","state_cd","county_cd","crop_cd")],
                           rma_adm[ins_plan_cd %in% 87:89,.(avail = 1,plan = "avail_eco95"), by = c("crop_yr","state_cd","county_cd","crop_cd")])
        }else{
          rma_adm <- rbind(rma_adm[ins_plan_cd %in% c(1:3,90),.(avail = 1,plan = "avail_aph")  , by = c("crop_yr","state_cd","county_cd","crop_cd")],
                           rma_adm[ins_plan_cd %in% 31:33,.(avail = 1,plan = "avail_sco")  , by = c("crop_yr","state_cd","county_cd","crop_cd")])
        }
        
        rma_adm <- as.data.frame(rma_adm) %>% tidyr::spread(plan, avail)
        
        return(rma_adm)}), fill = TRUE))

rma_adm$county_fips <- paste0(stringr::str_pad(rma_adm$state_cd,pad="0",2),stringr::str_pad(rma_adm$county_cd,pad="0",3))

worklist <- unique(rma_adm[c("crop_yr","crop_cd")])
shell <- as.data.frame(
  data.table::rbindlist(
    lapply(
      1:nrow(worklist),
      function(i){
        return(data.frame(worklist[i,],as.data.frame(cty_sf)[c("county_name","county_fips")]))}), fill = TRUE))
data <- dplyr::full_join(rma_adm,shell[c("county_fips","crop_yr","crop_cd")],by=c("county_fips","crop_yr","crop_cd"))
rm(rma_adm,shell,worklist)

rma_sob <- readRDS("data/rma_sob.rds")
setDT(rma_sob)
rma_sob[, sco   := comm_amt*sco]
rma_sob[, eco90 := comm_amt*eco90]
rma_sob[, eco95 := comm_amt*eco95]
rma_sob <- rma_sob[crop_yr >= 2015,.(comm_amt = sum(comm_amt,na.rm=T),
                                     sco = sum(sco,na.rm=T),
                                     eco90 = sum(eco90,na.rm=T),
                                     eco95 = sum(eco95,na.rm=T)), 
                   by = c("crop_yr","state_cd","county_cd","crop_cd")]

rma_sob <- as.data.frame(rma_sob)

data <- dplyr::full_join(data,rma_sob,by=c("crop_yr","state_cd","county_cd","crop_cd"))
rm(rma_sob)

data <- data %>% tidyr::gather(plan, value, c("avail_aph","avail_sco","avail_eco90","avail_eco95","comm_amt","sco","eco90","eco95"))
data <- data %>% tidyr::spread(plan, value)
data <- data %>% tidyr::gather(plan, value, c("avail_aph","avail_sco","avail_eco90","avail_eco95","comm_amt","sco","eco90","eco95"))
data$value <- ifelse(data$value %in% c(0,NA,NaN,Inf,-Inf),0,data$value)
data <- data %>% tidyr::spread(plan, value)

# data$avail_aph <- ifelse(data$avail_aph %in% c(0,NA,NaN,Inf,-Inf),0,data$avail_aph)
# data$avail_sco <- ifelse(data$avail_sco %in% c(0,NA,NaN,Inf,-Inf),0,data$avail_sco)
# data$avail_eco90 <- ifelse(data$avail_eco90 %in% c(0,NA,NaN,Inf,-Inf),0,data$avail_eco90)
# data$avail_eco95 <- ifelse(data$avail_eco95 %in% c(0,NA,NaN,Inf,-Inf),0,data$avail_eco95)
# data$sco <- ifelse(data$sco %in% c(0,NA,NaN,Inf,-Inf),0,data$sco)
# data$eco90 <- ifelse(data$eco90 %in% c(0,NA,NaN,Inf,-Inf),0,data$eco90)
# data$eco95 <- ifelse(data$eco95 %in% c(0,NA,NaN,Inf,-Inf),0,data$eco95)
# data$comm_amt <- ifelse(data$comm_amt %in% c(0,NA,NaN,Inf,-Inf),0,data$comm_amt)

saveRDS(data,"data/supplemental_offering_adoption.rds")

