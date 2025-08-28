
source("data-raw/scripts/environment_setup.R")

devtools::document()

df <- get_ice_data( years = 2011:as.numeric(format(Sys.Date(),"%Y")),
                    ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                    selected_ice = "D00042_IceYieldType")
df <- unique(df[, c("reinsurance_year","yield_type_code","valid_pty_non_summary_flag","yield_type_description"), with = FALSE])
df <- df[complete.cases(df)]

df[,actual_or_assigned_yield  := grepl("ACTUAL YIELD", toupper(yield_type_description)) &
     !grepl("TRANSFERRED", toupper(yield_type_description))]
df[grepl(paste(
    toupper(c(
      "FOR PISTACHIOS, ACTUAL ORGANIC CERTIFIED YIELD IN A CONVENTIONAL DATABASE. USED WHEN A PRODUCER IS RETURNING TO A CONVENTIONAL",
      "FOR CATEGORY C APH CROPS, USED TO IDENTIFY COMMINGLED PRODUCTION WHERE SEPARATE ACRES ARE AVAILABLE",
      "THE SIMPLE AVERAGE YIELD OF ALL ACTUAL AND ASSIGNED YIELDS",
      "ASSIGNED YIELD FOR PECANS ONLY",
      "FCIC RS ASSIGNED YIELD",
      "V T Yield [(]simple average[])],OR,Determined Irrigated Yields for added Irrigated Practice.",
      "Adjacent County harvested shellfish when insured does not have minimum record requirements",
      "TEMPORARY YIELD FOR DELAYED CLAIMS",
      "PRORATED",
      "FOR PISTACHIOS, THE ACTUAL CONVENTIONAL YIELD REDUCED BY 20% FOR USE IN A PRODUCERS 4 YEAR DATABASE",
      "A SPECIAL YIELD ENTERED",
      "Actual Certified Organic Yield","Transitional Yield used to identify and replace excessive yields",
      "Short rated acreage, zero production, with the actual acres short rated reported on the production report and included in APH database",
      "All acreage within APH is damaged due to UUF or third party damage","Actual Transitional Organic Yield",
      "For Category C APH crops, used to identify commingled production from transitional organic acreage where separate acres are available to separate",
      "Personal Transition Yield ",
      "For Category C APH crops, used to identify commingled production from certified organic acreage where separate acres are available to",
      "Replicated Annual Yield",
      "Limited Actual for Weaned Calf. T-Yield for initial reported year multiplied by 1.25",
      "T-Yield procedure to address the historical T-Yields not changing for organics",
      "Dedicated Processing SWEET POTATOES [(0156)] ONLY  Processing Yield, factored",
      "Organic AIP determined yield when the insured is transitioning a perennial commodity from conventional to organic",
      "For pistachios, producer failed to provide production reports for the most recent consecutive year"))
    , collapse = "|"), 
  toupper(yield_type_description)),actual_or_assigned_yield  := TRUE]

df[grepl(toupper("Processing SWEET POTATOES"), toupper(yield_type_description)) &
     grepl(toupper("ONLY  Processing Yield, factored"), toupper(yield_type_description)),
   actual_or_assigned_yield := TRUE]

df[grepl(toupper("Applicable for Avocados"), toupper(yield_type_description)) &
     grepl(toupper("Blueberries|Table Grapes"), toupper(yield_type_description)),
   actual_or_assigned_yield := TRUE]


df[,transitional_amount_rate := as.numeric(gsub("%","",gsub(".*?([0-9]+%).*", "\\1", yield_type_description)))/100]
df[transitional_amount_rate < 0.60 | actual_or_assigned_yield %in% TRUE,transitional_amount_rate := NA]
df[,transitional_amount := ! transitional_amount_rate %in% NA]

df[,new_producer            := actual_or_assigned_yield %in% FALSE & transitional_amount %in% FALSE & grepl("NEW PRODUCER",toupper(yield_type_description))]
df[,added_land              := actual_or_assigned_yield %in% FALSE & transitional_amount %in% FALSE & grepl("TRANSFERRED|PREVIOUS PRODUCER",toupper(yield_type_description))]
df[,previous_producer_yield := actual_or_assigned_yield %in% FALSE & transitional_amount %in% FALSE & grepl("ADDED LAND",toupper(yield_type_description))]
df[,assigned_pp35_yield     := actual_or_assigned_yield %in% FALSE & transitional_amount %in% FALSE & grepl(" PP |35%",toupper(yield_type_description))]

df[,beginning_farmer_rancher := grepl("BEGINNNG FARMER",toupper(yield_type_description))]
df[,veteran_farmer_rancher  := grepl("VETERAN FARMER",toupper(yield_type_description))]

df[,zero_harvested_acres    := actual_or_assigned_yield %in% FALSE & transitional_amount %in% FALSE & 
     grepl(toupper("unharvested and destroyed|unharvested and put to another use|Unharvested acreage|Zero Acres Planted"),
           toupper(yield_type_description))]

lagged_cols <- c("actual_or_assigned_yield","transitional_amount_rate","transitional_amount",
                 "new_producer","added_land","previous_producer_yield","assigned_pp35_yield","beginning_farmer_rancher",
                 "veteran_farmer_rancher","zero_harvested_acres")

setnames(df,old = lagged_cols,new = paste0(lagged_cols,"_flag"))

table(df$yield_type_code,df$reinsurance_year)


# 
# df_check <- df[
#   actual_or_assigned_yield %in% FALSE  & 
#   transitional_amount_flag %in% FALSE & 
#     new_producer %in% FALSE &
#     added_land %in% FALSE &
#     assigned_pp35_yield %in% FALSE &
#     previous_producer_yield %in% FALSE &
#     beginnng_farmer_rancher %in% FALSE &
#     veteran_farmer_rancher %in% FALSE & 
#     zero_harvested_acres %in% FALSE]
# 
# 
# 
# 
# 
# 
# 
# unique(df_check$yield_type_description)
# unique(df_check$yield_type_code)
# 
# 
# keeps <- c(
#   df$yield_type_description[grepl("NEW PRODUCER",toupper(df$yield_type_description))],
#   df$yield_type_description[grepl("ADDED LAND",toupper(df$yield_type_description))],
#   df$yield_type_description[grepl("TRANSFERRED|PREVIOUS PRODUCER|BEGIN|VETERAN",toupper(df$yield_type_description))])
# 
# unique(df$yield_type_description[grepl(" PP |35%",toupper(df$yield_type_description))])
# 
# 
# unique(
#   df$yield_type_description[
#     !is.na(df$yield_type_description) &
#       grepl("ACTUAL YIELD", toupper(df$yield_type_description)) &
#       !grepl("TRANSFERRED", toupper(df$yield_type_description))
#   ]
# )
# 
# 
# unique(ice_yield_type_code$yield_type_description[! ice_yield_type_code$yield_type_description %in% keeps])
# 
# ice_yield_type_code$yield_type_description[grepl("PREV",toupper(ice_yield_type_code$yield_type_description))]
# 
# ice_yield_type_code <- readRDS("data/ice_yield_type_code.rds")[reinsurance_year %in% 2023]
# ice_yield_type_code <- ice_yield_type_code[yield_type_code %in% df_aph$yield_type_code]










