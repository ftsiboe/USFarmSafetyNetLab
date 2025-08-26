
source("data-raw/work_environment_setup.R")
library(rfcip)
devtools::document()

# 1) Retrieve ADM data for each year from 2011 to current, binding into one data.frame
adm <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2011:as.numeric(format(Sys.Date(), "%Y")),
      function(year) {
        tryCatch({ 
          df <- get_adm_data(year = year,dataset = "A00460_InsurancePlan")
          df$commodity_year <- year
          return(df)
        }, error = function(e){return(NULL)})
      }), fill = TRUE))

adm <- unique(adm[c("reinsurance_year","insurance_plan_code","insurance_plan_abbreviation","insurance_plan_name")])
adm$reinsurance_year <- as.numeric(as.character(adm$reinsurance_year))
adm$insurance_plan_code <- as.numeric(as.character(adm$insurance_plan_code))
names(adm) <- c("crop_yr","ins_plan_cd","ins_plan_ab","ins_plan")

# 2) Prepend archived 1995–2010 cross-reference data
adm <- rbind(unique(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/1995-2010 clean/CROSS_REFERENCE_DATA_1997_2010.rds"))[names(adm)]),adm)
adm <- unique(adm)
adm <- adm[c("crop_yr","ins_plan_cd","ins_plan_ab","ins_plan")]
names(adm) <- c("crop_yr","ins_plan_cd","ins_plan_ab_adm","ins_plan_adm")
adm <- adm[!adm$ins_plan_cd %in% NA,]
adm$ins_plan_ab_adm <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(adm$ins_plan_ab_adm))), which = c("both"))))
adm$ins_plan_adm <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(adm$ins_plan_adm))), which = c("both"))))

# 3) Read SOB coverage and subsidy data, select key columns
sobcov <-as.data.frame(readRDS(paste0(dir_data_release,"/sob/sobcov_all.rds")))[
  c("commodity_year","insurance_plan_code","insurance_plan_abbreviation")];gc()

sobtpu <-as.data.frame(readRDS(paste0(dir_data_release,"/sob/sobtpu_all.rds")))[
  c("commodity_year","insurance_plan_code","insurance_plan_abbreviation")];gc()

names(sobtpu) <- c("crop_yr","ins_plan_cd","ins_plan_ab")
names(sobcov) <- c("crop_yr","ins_plan_cd","ins_plan_ab")

sob <- unique(rbind(sobcov,sobtpu))

sob$ins_plan_ab <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(sob$ins_plan_ab))), which = c("both"))))

sob$crop_yr <- as.numeric(as.character(sob$crop_yr))
sob$ins_plan_cd <- as.numeric(as.character(sob$ins_plan_cd))
sob$ins_plan_ab <- ifelse(sob$ins_plan_ab %in% c("NULL",""),NA,sob$ins_plan_ab)
sob <- unique(sob[!sob$ins_plan_cd %in% NA,])

# 4) Impute missing abbreviations by matching code→abbr where sd=0 or mode
sob <- sob[sob$crop_yr<min(adm$crop_yr,na.rm=T),]
sobui <- doBy::summaryBy(ins_plan_cd~ins_plan_ab,data=sob,FUN=c(mean,sd),na.rm=T)
sobui <- sobui[sobui$ins_plan_cd.sd %in% c(NA,0),]
sob$ins_plan_ab <- ifelse(
  sob$ins_plan_ab %in% NA,
  as.character(factor(sob$ins_plan_cd,levels = sobui$ins_plan_cd.mean,labels = sobui$ins_plan_ab)),
  sob$ins_plan_ab)
sob <- unique(sob[!sob$ins_plan_cd %in% NA,])

mode <- function(x,na.rm = T) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}
sobui <- doBy::summaryBy(ins_plan_ab~ins_plan_cd,data=sob[!sob$ins_plan_ab %in% NA,],FUN=mode)
sob$ins_plan_ab <- ifelse(sob$ins_plan_ab %in% NA,
                          as.character(factor(sob$ins_plan_cd,levels = sobui$ins_plan_cd,labels = sobui$ins_plan_ab.mode)),
                          sob$ins_plan_ab)
sob <- unique(sob[!sob$ins_plan_cd %in% NA,])

sob[order(sob$ins_plan_cd),]

# 5) Merge SOB and ADM metadata, then build full plan names
data <- dplyr::full_join(sob,adm,by=c("crop_yr","ins_plan_cd"))
data$ins_plan <- tools::toTitleCase(stringr::str_to_sentence(data$ins_plan_adm))

# Manual recoding of plan names for clarity
data$ins_plan <- ifelse(data$ins_plan    %in% "Aph Price Component","Actual Production History - Price Component",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan    %in% "Indexed Aph","Actual Production History - Indexed",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan    %in% "Aph","Actual Production History",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan_ab %in% "APH","Actual Production History",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan_ab %in% "CRC","Crop Revenue Coverage",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan_ab %in% "DOL","Fixed Dollar Amount of Insurance",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan_ab %in% "GRP","Group Risk Protection",data$ins_plan)  
data$ins_plan <- ifelse(data$ins_plan_ab %in% "IP","Income Protection",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan_ab %in% "PNT","Peanuts",data$ins_plan)  
data$ins_plan <- ifelse(data$ins_plan_ab %in% "PP","PP",data$ins_plan) #!!!!!!!!!!!!!!!!
data$ins_plan <- ifelse(data$ins_plan_ab %in% "TDO","Tree Based Dollar Amount Of Insurance",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan_ab %in% "TGP","Tobacco (Guaranteed Production)",data$ins_plan)  
data$ins_plan <- ifelse(data$ins_plan_ab %in% "TQ","Tobacco (Quota)",data$ins_plan) 
data$ins_plan <- ifelse(data$ins_plan_ab %in% "YDO","Yield Based Dollar Amount Of Insurance",data$ins_plan)
data$ins_plan <- ifelse(data$ins_plan %in% "Fixed Dollar","Fixed Dollar Amount of Insurance",data$ins_plan)

# Further string substitutions to tidy names
data$ins_plan <- gsub("Enhanced Cov Opt","Enhanced Coverage Option",data$ins_plan)
data$ins_plan <- gsub("Supp Cov Opt","Supplemental Coverage Option",data$ins_plan)
data$ins_plan <- gsub("Stacked Inc Prot Plan","Stacked Income Protection Plan",data$ins_plan)
data$ins_plan <- gsub("Post Apl Cvge Endt","Post-Application Coverage Endorsement",data$ins_plan)
data$ins_plan <- gsub("Margin Cov Opt","Margin Coverage Option",data$ins_plan)

data$ins_plan <- gsub("- Yield Prot","- Yield Protection",data$ins_plan)
data$ins_plan <- gsub("- Rev Prot","- Revenue Protection",data$ins_plan)
data$ins_plan <- gsub("Harv Price Excl","Harvest Price Exclusion",data$ins_plan)
data$ins_plan <- gsub("Revenue Protection w Harvest Price Exclusion","Revenue Protection with Harvest Price Exclusion",data$ins_plan)
data$ins_plan <- gsub("Harvest Rev Option","Harvest Revenue Option",data$ins_plan)

data$ins_plan <- gsub("Post-Application Coverage Endorsement Rev Prot","Post-Application Coverage Endorsement - Revenue Protection",data$ins_plan)
data$ins_plan <- gsub("Post-Application Coverage Endorsement Rev Prot wi","Post-Application Coverage Endorsement - Revenue Protection with",data$ins_plan)
data$ins_plan <- gsub("Post-Application Coverage Endorsement Yield Prot","Post-Application Coverage Endorsement - Yield Protection",data$ins_plan)

# Inspect unique results & drop any remaining NAs
unique(data$ins_plan)
unique(data[data$ins_plan %in% NA,])

data$ins_plan_ab_adm <- ifelse(data$ins_plan_ab_adm %in% NA,data$ins_plan_ab,data$ins_plan_ab_adm)  
data <- unique(data[c("crop_yr","ins_plan_cd","ins_plan","ins_plan_ab_adm")])

# 6) Classify as Basic vs Endorsement
data$policy <- ifelse(grepl(paste(
  "Supplemental Coverage Option",
  "Enhanced Coverage Option",
  "Post-Application Coverage",
  "Fire Insurance Protection",
  "Hurricane Insurance Protection",
  "Stacked Income Protection",
  "Margin Coverage Option",
  sep = "|"),data$ins_plan) ,"Endorsement",NA)

data$policy <- ifelse(grepl(paste(
  "Margin Protection",
  "Stacked Income Protection",
  sep = "|"),data$ins_plan) ,"Basic or Endorsement",data$policy)

data$policy <- ifelse(data$policy %in% NA,"Basic",data$policy)

unique(data[data$policy %in% NA,"ins_plan"])

# 6) Classify Trigger level and outcome protected
Yield <- c("PACE-YP","STAX-YP","SCO-YP","ECO-YP","APH","YP","IAPH","PRH-Y","GRP","YDO","AYP",
           "RAINF","VEGAT","RI","API","VI","PRF","HIP-WI","PACE-YP","APHPC","TGP","TQ")

Revenue <- c("PACE-RP","PACE-RPHPE","STAX-RP","STAX-RPHPE","ECO-RP","ECO-RPHPE","SCO-RPHPE","SCO-RP","AGR","AGR-L","ARC",
             "AGRLT","ARH","LGM","PRV","RP","RPHPE","WFRP","PRH-P","PRH-R","IIP","IP","RA","CRC",
             "GRIP","GRIP-HRO","LRP","ARP","ARP - HPE","MP","MP-HPO","DRP","PACE-RP","PACE-RPHPE")

Individual <- c("APH","YP","IAPH","PRH-Y","AGR","AGR-L","AGRLT","ARH","LGM","PRV","RP","RPHPE","WFRP",
                "PRH-P","PRH-R","IIP","IP","RA","TQ","TGP","CRC","PNT","ARC","GS","G","APHPC","TGP","TQ")

Group <- c("SCO-YP","SCO-RP","SCO-RPHPE",
           "ECO-YP","ECO-RP","ECO-RPHPE",
           "PACE-YP","PACE-RP","PACE-RPHPE",
           "STAX-RP","STAX-RPHPE",
           "GRP","AYP","GRIP","GRIP-HRO","LRP","RI",
           "VI","ARP","ARP - HPE","MP","MP-HPO","DRP","HIP-WI","RAINF","VEGAT")

Noclassyet <- c("PACE-YP","PACE-RP","PACE-RPHPE","YDO","DOL","DO","PNT","TDO","TGP","TQ","FD","AQDOL","AQU")

data$protect <- NA
data$triger <- NA

for( vr in c("ins_plan","ins_plan_ab_adm")){
  data[,vr] <- gsub(" \u0096 ","",iconv(data[,vr], from = "ISO-8859-1", to = "UTF-8"))
  
  data$protect <- ifelse(data$protect %in% NA & data[,vr] %in% Yield,"Yield",data$protect)
  data$protect <- ifelse(data$protect %in% NA & data[,vr] %in% Revenue,"Revenue",data$protect)
  
  data$protect <- ifelse(data$protect %in% NA & grepl("YIELD",toupper(data[,vr])),"Yield",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("REVENUE",toupper(data[,vr])),"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("PEANUT",toupper(data[,vr])),"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("INCOME",toupper(data[,vr])),"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("INDEX",toupper(data[,vr])),"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("DOLLAR",toupper(data[,vr])),"Dollar",data$protect)
  
  data$triger  <- ifelse(data$triger  %in% NA & data[,vr] %in% Individual,"Individual",data$triger)
  data$triger  <- ifelse(data$triger  %in% NA & data[,vr] %in% Group,"Group",data$triger)
  data$triger  <- ifelse(data$triger  %in% NA & grepl("GROUP",toupper(data[,vr])),"Group",data$triger)
  data$triger  <- ifelse(data$triger  %in% NA & grepl("INDEX",toupper(data[,vr])),"Group",data$triger)
  data$triger  <- ifelse(data$triger  %in% NA & grepl("DOLLAR",toupper(data[,vr])),"Group",data$triger)
}

rma_groupings <- as.data.frame(readxl::read_excel(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/rma_providing_plan_groupings.xlsx")))
names(rma_groupings)[names(rma_groupings) %in% "ins_plan_type"] <-"ins_plan_type_rma"
names(rma_groupings)[names(rma_groupings) %in% "ins_plan_ab"] <-"ins_plan_ab_adm"
data <- dplyr::full_join(data,rma_groupings,by="ins_plan_ab_adm")
data <- data[!data$crop_yr %in% NA,]

data <- data[c("crop_yr","ins_plan_cd","ins_plan","ins_plan_ab_adm","policy","protect","triger","ins_plan_type_rma")]
names(data) <- c("commodity_year","insurance_plan_code","insurance_plan_name","insurance_plan_abbreviation",
                 "fcip_policy_type","outcome_protected","triger_level","insurance_plan_type_rma")

data$insurance_plan_name <- tools::toTitleCase(stringr::str_to_sentence(toupper(data$insurance_plan_name)))

data <- data.table::as.data.table(unique(data))
data <- harmonize_codes_and_names(data)

