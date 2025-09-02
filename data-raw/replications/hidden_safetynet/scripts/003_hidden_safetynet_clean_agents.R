
rm(list=ls(all=TRUE));gc()

source("scripts/000_hidden_safetynet_helpers.R")

study_env <- setup_environment()


dir.create("data/agentdata")
plan(list(tweak(multisession, workers = 4)))
future_lapply(
  year_beg:year_end,
  function(crop_yr){
    tryCatch({ 
      # crop_yr <- 2015
      dir.create(paste0(dir_sim,crop_yr,"/"))
      
      agentdata <- as.data.frame(readRDS(paste0(Dr.FCIP,"rmaMultiverse/Output/yield_price_sim/sim_rma/sim_rma_",crop_yr,".rds")))
      agentdata <- unique(agentdata[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl",
                                      "yield_cal", "yield_rate", "yield_approved","Projected_Price")]);gc();gc()
      
      supp <- readRDS("data/rma_supplemental_premium_rates.rds")
      agentdata <- dplyr::inner_join(
        agentdata,unique(supp[supp$crop_yr %in% crop_yr,c("state_cd","county_cd","crop_cd","typ_cd","pract_cd")]),
        by =c("state_cd","county_cd","crop_cd","typ_cd","pract_cd"));rm(supp);gc();gc()
      
      agentdata <- dplyr::inner_join(readRDS("data/rma_sob.rds"),agentdata)
      agentdata$premium_rate <- round(agentdata$total_prem/agentdata$liability_amt,8)
      agentdata$Subsidy_Percent <- round(agentdata$subsidy/agentdata$total_prem,3)
      agentdata <- agentdata[c("crop_yr","state_cd","county_cd","crop_cd","typ_cd","pract_cd","struct_cd","ins_plan_cd","cov_lvl",
                               "yield_cal", "yield_rate", "yield_approved","Projected_Price", 
                               "premium_rate","Subsidy_Percent","comm_amt","sco", "eco90", "eco95")]
      agentdata$planted_acres  <- agentdata$comm_amt
      agentdata$price_election <- 1
      agentdata$insured_share  <- 1
      agentdata$dmage_are_rate <- 1
      
      saveRDS(agentdata,file =paste0("data/agentdata/agentdata",crop_yr,".rds"))
      rm(agentdata);gc();gc()
      return(crop_yr)
    }, error = function(e){return(NULL)})
  })
plan(sequential)
