
rm(list=ls(all=TRUE));gc()
adm <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2011:2026,
      function(year) {
        tryCatch({ 
          df <- rmaADM::get_adm_data(year = year,dataset = "A00460_InsurancePlan")
          df$commodity_year <- year
          return(df)
        }, error = function(e){return(NULL)})
      }), fill = TRUE))

adm <- unique(adm[c("reinsurance_year","insurance_plan_code","insurance_plan_abbreviation","insurance_plan_name")])
adm$reinsurance_year <- as.numeric(as.character(adm$reinsurance_year))
adm$insurance_plan_code <- as.numeric(as.character(adm$insurance_plan_code))
names(adm) <- c("crop_yr","ins_plan_cd","ins_plan_ab","ins_plan")

adm <- rbind(unique(readRDS("data-raw/datastash/CROSS_REFERENCE_DATA_1997_2010.rds")[names(adm)]),adm)

adm <- unique(adm)
adm <- adm[c("crop_yr","ins_plan_cd","ins_plan_ab","ins_plan")]
names(adm) <- c("crop_yr","ins_plan_cd","ins_plan_ab_adm","ins_plan_adm")
adm <- adm[!adm$ins_plan_cd %in% NA,]

adm$ins_plan_ab_adm <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(adm$ins_plan_ab_adm))), which = c("both"))))
adm$ins_plan_adm <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(adm$ins_plan_adm))), which = c("both"))))

sob<-as.data.frame(
  data.table::rbindlist(
    lapply(
      c("data-raw/data_release/sob/sobcov_all.rds",
        "data-raw/data_release/sob/sobtpu_all.rds"),
      function(file){
        NAMES <- c("insurance_plan_code","insurance_plan_name_abbreviation","commodity_year")
        data <- as.data.frame(readRDS(file))
        return(unique(data[NAMES[NAMES %in% names(data)]]))
      }), fill = TRUE));gc()

sobcov <-as.data.frame(readRDS("data-raw/data_release/sob/sobcov_all.rds"))[
  c("commodity_year","insurance_plan_code","insurance_plan_name_abbreviation")];gc()

sobtpu <-as.data.frame(readRDS("data-raw/data_release/sob/sobtpu_all.rds"))[
  c("commodity_year","insurance_plan_code","insurance_plan_abbreviation")];gc()

names(sobtpu) <- c("crop_yr","ins_plan_cd","ins_plan_ab")
names(sobcov) <- c("crop_yr","ins_plan_cd","ins_plan_ab")

sob <- unique(rbind(sobcov,sobtpu))

sob$ins_plan_ab <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(sob$ins_plan_ab))), which = c("both"))))

sob$crop_yr <- as.numeric(as.character(sob$crop_yr))
sob$ins_plan_cd <- as.numeric(as.character(sob$ins_plan_cd))
sob$ins_plan_ab <- ifelse(sob$ins_plan_ab %in% c("NULL",""),NA,sob$ins_plan_ab)
sob <- unique(sob[!sob$ins_plan_cd %in% NA,])

sob <- sob[sob$crop_yr<min(adm$crop_yr,na.rm=T),]
sobui <- doBy::summaryBy(ins_plan_cd~ins_plan_ab,data=sob,FUN=c(mean,sd),na.rm=T)
sobui <- sobui[sobui$ins_plan_cd.sd %in% c(NA,0),]
sob$ins_plan_ab <- ifelse(sob$ins_plan_ab %in% NA,
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

data <- dplyr::full_join(sob,adm,by=c("crop_yr","ins_plan_cd"))

data$ins_plan_adm <- stringr::str_to_sentence(data$ins_plan_adm)

Yield <- c("STAX-YP","SCO-YP","ECO-YP","APH","YP","IAPH","PRH-Y","GRP","YDO","AYP","RAINF","VEGAT","RI","API","VI","PRF","HIP-WI","PACE-YP")

Revenue <- c("STAX-RP","STAX-RPHPE","ECO-RP","ECO-RPHPE","SCO-RPHPE","SCO-RP","AGR","AGR-L","ARC",
             "AGRLT","ARH","LGM","PRV","RP","RPHPE","WFRP","PRH-P","PRH-R","IIP","IP","RA","CRC",
             "GRIP","GRIP-HRO","LRP","ARP","ARP - HPE","MP","MP-HPO","DRP","PACE-RP","PACE-RPHPE")

Individual <- c("APH","YP","IAPH","PRH-Y","AGR","AGR-L","AGRLT","ARH","LGM","PRV","RP","RPHPE","WFRP",
                "PRH-P","PRH-R","IIP","IP","RA","TQ","TGP","CRC","PNT","ARC")

Group <- c("ECO-RP","ECO-RPHPE","SCO-RPHPE","SCO-RP",
           "STAX-RP","STAX-RPHPE",
           "SCO-YP","ECO-YP","GRP","AYP","GRIP","GRIP-HRO","LRP","RI",
           "VI","ARP","ARP - HPE","MP","MP-HPO","DRP","HIP-WI","RAINF","VEGAT")

Noclassyet <- c("PACE-YP","PACE-RP","PACE-RPHPE","YDO","DOL","DO","PNT","TDO","TGP","TQ","FD","AQDOL","AQU")

data$protect <- NA
data$triger <- NA

for( vr in c("ins_plan_adm","ins_plan_ab_adm","ins_plan_ab")){
  data[,vr] <- gsub(" \u0096 ","",iconv(data[,vr], from = "ISO-8859-1", to = "UTF-8"))
  data$protect <- ifelse(data$protect %in% NA & data[,vr] %in% Yield,"Yield",data$protect)
  data$protect <- ifelse(data$protect %in% NA & data[,vr] %in% Revenue,"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("YIELD",toupper(data[,vr])),"Yield",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("REVENUE",toupper(data[,vr])),"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("INCOME",toupper(data[,vr])),"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl(" REV",toupper(data[,vr])),"Revenue",data$protect)
  data$protect <- ifelse(data$protect %in% NA & grepl("INDEX",toupper(data[,vr])),"Revenue",data$protect)
  data$triger  <- ifelse(data$triger  %in% NA & data[,vr] %in% Individual,"Individual",data$triger)
  data$triger  <- ifelse(data$triger  %in% NA & data[,vr] %in% Group,"Group",data$triger)
  data$triger  <- ifelse(data$triger  %in% NA & grepl("INDEX",toupper(data[,vr])),"Group",data$triger)
}

# data[(data$protect %in% NA & data$triger %in% NA),]

data$ins_plan_ab <- ifelse(data$ins_plan_ab_adm %in% NA,data$ins_plan_ab,data$ins_plan_ab_adm)
names(data) <- gsub("ins_plan_adm","ins_plan",names(data))
data <- data[complete.cases(data[c("crop_yr","ins_plan_cd")]),names(data)[! names(data) %in% "ins_plan_ab_adm"]]

data$ins_plan <- gsub(" \u0096 ","",iconv(data$ins_plan, from = "ISO-8859-1", to = "UTF-8"))

# table(data[data$ins_plan %in% NA,"ins_plan_ab"])

# data[data$ins_plan_ab %in% "PP","ins_plan"]

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

# Harmonize COMBO products
# These three plans of insurance are similar but not identical. Some differences are:
# • CRC bases the insurance guarantee on the higher of the base price or the harvest period price.
# • IP and standard RA guarantees are determined using the base price, with no adjustment in coverage 
#   if the price increases between the times when the base and harvest prices are established.
# • RA offers up-side price protection like that of CRC as an option but IP does not.
# • IP limits unit formats to basic units, which include all interest in a crop in a county held under identical ownership.
# • RA is unique in offering coverage on whole farm units, which integrates the coverage on from two to three crops.
# check <- data[data$ins_plan_ab %in% c("YP","APH","IP","RP-HPE","RPHPE","CRC","RP","RA"),]
# 
# table(check$crop_yr,check$ins_plan_ab)
# table(check$ins_plan,check$ins_plan_ab)

data$ins_plan_cd_rc <- ifelse(data$ins_plan_cd %in% c(1,90),1, data$ins_plan_cd)     # recode APH[90] as YP[1]
data$ins_plan_cd_rc <- ifelse(data$ins_plan_cd %in% c(44,2),2, data$ins_plan_cd_rc)  # recode CRC[44] as RP[2]
data$ins_plan_cd_rc <- ifelse(data$ins_plan_cd %in% c(25,42,3),3, data$ins_plan_cd_rc)  # recode IP[42]  as RP-HPE[3]

data$ins_plan_ab_rc <- ifelse(data$ins_plan_cd %in% c(1,90),"YP or APH", data$ins_plan_ab) 
data$ins_plan_ab_rc <- ifelse(data$ins_plan_cd %in% c(44,2),"RA or CRC", data$ins_plan_ab_rc) 
data$ins_plan_ab_rc <- ifelse(data$ins_plan_cd %in% c(25,42,3),"RP-HPE or IP", data$ins_plan_ab_rc) 

data$ins_plan_rc <- ifelse(data$ins_plan_cd %in% c(1,90),"Yield Protection or APH", data$ins_plan) 
data$ins_plan_rc <- ifelse(data$ins_plan_cd %in% c(44,2),"Revenue Protection or Crop Revenue Coverage", data$ins_plan_rc) 
data$ins_plan_rc <- ifelse(data$ins_plan_cd %in% c(25,42,3),"Revenue Prot with Harvest Price Exclusion or Income Protection", data$ins_plan_rc) 

data$policy <- NA
data$policy <- ifelse(data$ins_plan %in% "Peanuts"                         ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Tobacco (Guaranteed Production)" ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Tobacco (Quota)"                 ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "PP"                              ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Grower Yield Certification"      ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Avocado Revenue Coverage"        ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Indexed Income Protection"       ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Grower Yield Certification Span" ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Indexed APH"                     ,"Basic",data$policy)  #!!! Not sure
data$policy <- ifelse(data$ins_plan %in% "Pecan Revenue"                   ,"Basic",data$policy)  #!!! Not sure

data$policy <- ifelse(data$ins_plan %in% "Dairy Revenue Protection"                          ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Livestock Risk Protection"                         ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Livestock Gross Margin"                            ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Fixed Dollar Amount of Insurance"                  ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Yield Based Dollar Amount Of Insurance"            ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Tree Based Dollar Amount Of Insurance"             ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Dollar Amount Of Insurance"                        ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Aquaculture Dollar"                                ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Actual Production History"                         ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Yield Protection"                                  ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Revenue Protection"                                ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Revenue Prot with Harvest Price Exclusion"         ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Group Risk Income Protection - Harvest Rev Option" ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Area Yield Protection"                             ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Area Revenue Protection"                           ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Area Revenue Protection - Harvest Price Exclusion" ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Revenue Assurance"                                 ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Income Protection"                                 ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Group Risk Income Protection"                      ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Crop Revenue Coverage"                             ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Group Risk Protection"                             ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Rainfall Index"                                    ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Vegetation Index"                                  ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Whole Farm Revenue Protection"                     ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Adjusted Gross Revenue - Lite"                     ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Adjusted Gross Revenue"                            ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Production Revenue History - Yield"                ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Production Revenue History - Plus"                 ,"Basic",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Production Revenue History - Revenue"              ,"Basic",data$policy)

data$policy <- ifelse(data$ins_plan %in% "Supp Cov Opt - Yield Prot"                        ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Supp Cov Opt - Rev Prot"                          ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Supp Cov Opt - Rev Prot with Harv Price Excl"     ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Enhanced Cov Opt - Yield Prot"                    ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Enhanced Cov Opt - Rev Prot"                      ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Enhanced Cov Opt - Rev Prot with Harv Price Excl" ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Post Apl Cvge EndtYield Prot"                     ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Post Apl Cvge EndtRev Prot"                       ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Post Apl Cvge EndtRev Prot w Harv Price Excl"     ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Actual Revenue History"                           ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Hurricane Insurance Protection - Wind Index"      ,"Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Stacked Inc Prot Plan - Rev Prot"                    ,"Basic or Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Stacked Inc Prot Plan - Rev Prot w Harv Price Excl"  ,"Basic or Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Margin Protection"                                   ,"Basic or Endorsement",data$policy)
data$policy <- ifelse(data$ins_plan %in% "Margin Protection with Harvest Price Option"         ,"Basic or Endorsement",data$policy)

saveRDS(data,"data-raw/datastash/recodes_ins_plan.rds")



sobcov <- readRDS("data-raw/data_release/sob/sobcov_all.rds")[
  , lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
  by = c("insurance_plan_name_abbreviation","commodity_year","insurance_plan_code"), 
  .SDcols = c("liability_amount","indemnity_amount","total_premium_amount","subsidy_amount")];gc()

names(sobcov)[names(sobcov) %in% "insurance_plan_name_abbreviation"] <- "insurance_plan_abbreviation"

sobtpu <- readRDS("data-raw/data_release/sob/sobtpu_all.rds")[
  , lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
  by = c("insurance_plan_abbreviation","commodity_year","insurance_plan_code"), 
  .SDcols = c("liability_amount","indemnity_amount","total_premium_amount","subsidy_amount")];gc()

ins_plan <- rbind(sobtpu,sobcov[!commodity_year %in% unique(sobtpu$commodity_year),])

ins_plan$ins_plan_cd <- ins_plan$insurance_plan_code  
ins_plan$crop_yr <- ins_plan$commodity_year

recodes <- readRDS("data-raw/datastash/recodes_ins_plan.rds")
recodes$new_name <- recodes$ins_plan

data <- dplyr::full_join(ins_plan,recodes[c("ins_plan_cd","crop_yr","new_name","policy")],by=c("ins_plan_cd","crop_yr"))
data$new_name <- ifelse(data$new_name %in% c(NA,Inf,-Inf,NaN,"","NULL"),data$ins_plan_ab,data$new_name)
data$new_name <- ifelse(data$new_name %in% c(NA,Inf,-Inf,NaN,"","NULL"),"Unclassified",data$new_name)
data$policy <- ifelse(data$policy %in% c(NA,Inf,-Inf,NaN,""),"Basic",data$policy)

data$policy <- as.integer(factor(data$policy,levels = c("Basic","Endorsement","Basic or Endorsement"),
                                 labels = 1:3))

data <- doBy::summaryBy(list(c("liability_amt","indem_amt","total_prem","subsidy"),
                             c("policy","new_name","crop_yr")),FUN=sum,data=data,na.rm=T,keep.names=T)

data <- dplyr::full_join(data,doBy::summaryBy(list(c("liability_amt"),"crop_yr"),FUN=sum,data=data,na.rm=T),by="crop_yr")

for(vbl in c("liability_amt")){
  data[,vbl] <- data[,vbl]/data[,paste0(vbl,".sum")]
  data[,vbl] <- ifelse(data[,vbl] %in% c(NA,Inf,-Inf,NaN,""),NA,data[,vbl])
}


data <- data[names(data)[!grepl(".sum",names(data))]]

data <- doBy::summaryBy(crop_yr+liability_amt~new_name+policy,FUN=c(max,min,mean,sd),data=data,na.rm=T)

data <- data[order(data$policy,-data$liability_amt.mean),]
data <- data[c("policy","new_name","crop_yr.min","crop_yr.max","liability_amt.mean","liability_amt.sd")]
data <- data[!data$new_name %in% c("Aquaculture Dollar","Livestock Risk Protection","Livestock Gross Margin",
                                   "Dairy Revenue Protection","Unclassified","PP"),]
data$new_name <- gsub("Enhanced Cov Opt","Enhanced Coverage Option",data$new_name)
data$new_name <- gsub("Supp Cov Opt","Supplemental Coverage Option",data$new_name)
data$new_name <- gsub("Stacked Inc Prot Plan","Stacked Income Protection Plan",data$new_name)

data$new_name <- gsub("- Yield Prot","- Yield Protection",data$new_name)
data$new_name <- gsub("- Rev Prot","- Revenue Protection",data$new_name)
data$new_name <- gsub("Harv Price Excl","Harvest Price Exclusion",data$new_name)
data$new_name <- gsub("Post Apl Cvge EndtRev Prot","Post-Application Coverage Endorsement- Revenue Protection",data$new_name)
data$new_name <- gsub("Post Apl Cvge EndtRev Prot w","Post-Application Coverage Endorsement- Revenue Protection with",data$new_name)
data$new_name <- gsub("Post Apl Cvge EndtYield Prot","Post-Application Coverage Endorsement - Yield Protection",data$new_name)
data$new_name <- gsub("- Harvest Rev Option","- Harvest Revenue Option",data$new_name)
data$new_name <- gsub("Indexed APH","Indexed Actual Production History",data$new_name)
data$new_name <- gsub(" w "," with ",data$new_name)
data$avail <- paste0(data$crop_yr.min,"-",data$crop_yr.max)
data$share <- round(data$liability_amt.mean*100,4)
data <- data[c("policy","new_name","avail","share")]
data


c("data-raw/data_release/sob/sobcov_all.rds",
  "data-raw/data_release/sob/sobtpu_all.rds")
