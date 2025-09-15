
rm(list=ls(all=TRUE))
source("data-raw/scripts/environment_setup.R")

devtools::document()

current_year <- 2025

# Summary of Business
sob <- data.table::rbindlist(
  lapply(
    c(1999:current_year),  
    function(year){
      sob <- as.data.table(
        get_sob_data(year = year,comm_cat = "B",
                     group_by = c("delivery_type","cov_lvl","insurance_plan","state"),force=T))
      sob$commodity_year <- as.numeric(as.character(sob$commodity_year))
      sob$insurance_plan_code <- as.numeric(as.character(sob$insurance_plan_code))
      sob$coverage_level_percent <- as.numeric(as.character(sob$cov_level_percent))
      sob$state_code <- as.numeric(as.character(sob$state_code))
      return(sob)
    }),fill = TRUE)

# AO Subsidy rate
aoSubsidy <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/ice/ice_ao_expense_subsidy_percent.rds",
  aoSubsidy, mode = "wb", quiet = TRUE)
aoSubsidy <- readRDS(aoSubsidy)
aoSubsidy <- sob[aoSubsidy, on = intersect(names(aoSubsidy),names(sob)), nomatch = 0]
aoSubsidy <- aoSubsidy[
  ! grepl("CAT",aoSubsidy$delivery_type_code),.(AO_sob = sum(ao_expense_subsidy_percent*total_prem,na.rm=T)), 
  by = c("commodity_year")]

# Reinsurance Report    
reinsurance <- as.data.frame(rfcip::stateSRA)
reinsurance <- reinsurance[!reinsurance$fund_name %in% c("Total (All Funds)"),]
reinsurance$commodity_year <- as.numeric(as.character(reinsurance$reinsurance_year))
reinsurance$fund_name <- ifelse(reinsurance$fund_name %in% c("Assigned Risk","Other Assigned Risk"),"ARF","CF")
reinsurance <- reinsurance |> tidyr::spread(value_type, dollars)
reinsurance <- as.data.table(reinsurance)[, lapply(.SD, function(x) sum(as.numeric(as.character(x)), na.rm = TRUE)),
             by = c("commodity_year","state","fund_abb"), 
             .SDcols = c("gross_liability","gross_premium","gross_indemnity",
                         "retained_liability", "retained_premium","retained_indemnity",
                         "net_gain_loss")]

# Crop year government cost of federal crop insurance 
budget <-  as.data.frame(
  data.table::rbindlist(
    lapply(
      c(2008,2009,2011:current_year),  
      function(year){
        tryCatch({ 
          cost <- as.data.frame(
            readxl::read_excel(
              paste0(farmpolicylab,"rmaFCIPdata/fcipCost/Archive/fcipBudget/Crop year government cost of federal crop insurance.xlsx"), 
              sheet = paste0(year), skip = 2))
          cost <- cost[,1:7]
          names(cost) <- c("commodity_year","Total_Premium","Underwritings","Indemnities","Premium_subsidy","Delivery_costs","Direct_costs")
          cost$year <- year
          cost <- cost[!cost$commodity_year %in% c(NA,"Total"),]
          return(cost)
        }, error = function(e){return(NULL)})
      }),fill = TRUE))

for(i in 1:ncol(budget)){
  budget[,i] <- as.numeric(as.character(budget[,i]))
}
budget$RepAge <- budget$year-budget$commodity_year
budget <-within(budget, {Max_RepAge = ave(RepAge,commodity_year,FUN=max)} )
budget <- budget[budget$RepAge == budget$Max_RepAge,]
for(i in 2:ncol(budget)){
  budget[,i] <- budget[,i]*1000000
}
budget <- budget[c("commodity_year","Total_Premium","Underwritings","Indemnities","Premium_subsidy","Delivery_costs","Direct_costs")]
budget <- budget[complete.cases(budget),]


# Final data  

setDT(budget)
budget <- as.data.frame(
  budget[, .(
    total_prem_bud     = -sum(Total_Premium, na.rm = TRUE),
    subsidy_bud        = sum(Premium_subsidy, na.rm = TRUE),
    indemnity_bud      = sum(Indemnities, na.rm = TRUE),
    Underwritings_bud  = sum(Underwritings, na.rm = TRUE),
    AO_bud   = sum(Delivery_costs, na.rm = TRUE),
    DC_bud   = sum(Direct_costs, na.rm = TRUE)),
    by = c("commodity_year")])

setDT(sob)
sob <- as.data.frame(
  sob[, .(
    liabilities_sob  = sum(liabilities, na.rm = TRUE),
    total_prem_sob   = sum(total_prem, na.rm = TRUE),
    subsidy_sob      = sum(subsidy, na.rm = TRUE),
    indemnity_sob    = sum(indemnity, na.rm = TRUE)),
    by = c("commodity_year")])

setDT(reinsurance)
reinsurance <- as.data.frame(
  reinsurance[, .(
    liabilities_rin    = sum(gross_liability, na.rm = TRUE),
    total_prem_rin     = sum(gross_premium, na.rm = TRUE),
    indemnity_rin      = sum(gross_indemnity, na.rm = TRUE),
    Underwritings_rin  = sum(net_gain_loss, na.rm = TRUE)),
    by = c("commodity_year")])


rma_appropriations <- as.data.frame(readxl::read_excel(paste0(
  farmpolicylab,"rmaFCIPdata/fcipCost/Archive/RMA_Appropriations.xlsx")))
rma_appropriations$commodity_year <- as.numeric(rma_appropriations$fiscal_year)
rma_appropriations <- rma_appropriations[!rma_appropriations$commodity_year %in% NA,c("commodity_year","RMA_Appropriations")]


fcip_cost <- as.data.frame(dplyr::full_join(aoSubsidy,dplyr::full_join(sob,reinsurance)))
fcip_cost <- dplyr::full_join(fcip_cost,budget)
fcip_cost <- dplyr::full_join(rma_appropriations,fcip_cost)

fcip_cost <- fcip_cost[order(fcip_cost$commodity_year),]

fcip_cost$costA <- ifelse(fcip_cost$total_prem_bud %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$total_prem_rin,fcip_cost$total_prem_bud)
fcip_cost$costA <- ifelse(fcip_cost$costA %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$total_prem_sob,fcip_cost$costA)

fcip_cost$costB <- ifelse(fcip_cost$subsidy_bud %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$subsidy_sob,fcip_cost$subsidy_bud)

fcip_cost$costC <- fcip_cost$costA-fcip_cost$costB

fcip_cost$costD <- ifelse(fcip_cost$indemnity_bud %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$indemnity_rin,fcip_cost$indemnity_bud)
fcip_cost$costD <- ifelse(fcip_cost$costD %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$indemnity_sob,fcip_cost$costD)

fcip_cost$NWR <- ifelse(fcip_cost$Underwritings_bud %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$Underwritings_rin,fcip_cost$Underwritings_bud)

fcip_cost$costE <- abs(ifelse(fcip_cost$NWR > 0,fcip_cost$NWR,0)) # UWG
fcip_cost$costF <- abs(ifelse(fcip_cost$NWR < 0,fcip_cost$NWR,0)) # UWL

fcip_cost$costG <- ifelse(fcip_cost$AO_bud %in% c(NA,Inf,-Inf,NaN,0),fcip_cost$AO_sob,fcip_cost$AO_bud)
fcip_cost$costJ <- fcip_cost$costC-fcip_cost$costD-fcip_cost$costE+fcip_cost$costF-fcip_cost$costG

saveRDS(fcip_cost,file="data-raw/supplementary_files/fcip_direct_cost_items.rds")



