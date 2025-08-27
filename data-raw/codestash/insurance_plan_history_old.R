
rm(list=ls(all=TRUE));gc();devtools::document()
source("data-raw/codestash/work_station_setup.R")

year_beg <- 1989
year_end <- as.numeric(format(Sys.Date(),"%Y"))
adm <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2011:year_end,
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

adm <- rbind(unique(readRDS(paste0(Dr.FCIP,"rmaActuarialDataMaster/Archive/1995-2010 clean/CROSS_REFERENCE_DATA_1997_2010.rds"))[names(adm)]),adm)
adm <- unique(adm)
adm <- adm[c("crop_yr","ins_plan_cd","ins_plan_ab","ins_plan")]
names(adm) <- c("crop_yr","ins_plan_cd","ins_plan_ab_adm","ins_plan_adm")
adm <- adm[!adm$ins_plan_cd %in% NA,]

adm$ins_plan_ab_adm <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(adm$ins_plan_ab_adm))), which = c("both"))))
adm$ins_plan_adm <- as.character(toupper(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(adm$ins_plan_adm))), which = c("both"))))


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
data$ins_plan <- tools::toTitleCase(stringr::str_to_sentence(data$ins_plan_adm))


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

unique(data$ins_plan)

unique(data[data$ins_plan %in% NA,])

data <- unique(data[c("crop_yr","ins_plan_cd","ins_plan")])

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

recodes <- data
recodes$new_name <- recodes$ins_plan

sobcov <- readRDS("data-raw/data_release/sob/sobcov_all.rds")[
  , lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
  by = c("commodity_year","insurance_plan_code"), 
  .SDcols = c("liability_amount","indemnity_amount","total_premium_amount","subsidy_amount")];gc()

sobtpu <- readRDS("data-raw/data_release/sob/sobtpu_all.rds")[
  , lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
  by = c("commodity_year","insurance_plan_code"), 
  .SDcols = c("liability_amount","indemnity_amount","total_premium_amount","subsidy_amount")];gc()

ins_plan <- rbind(sobtpu,sobcov[!commodity_year %in% unique(sobtpu$commodity_year),])
ins_plan$crop_yr <- ins_plan$commodity_year
ins_plan$ins_plan_cd <- ins_plan$insurance_plan_code

data <- dplyr::full_join(ins_plan,recodes[c("ins_plan_cd","crop_yr","new_name","policy")],by=c("ins_plan_cd","crop_yr"))
data$new_name <- ifelse(data$new_name %in% c(NA,Inf,-Inf,NaN,"","NULL"),"Unclassified",data$new_name)
data$policy <- ifelse(data$policy %in% c(NA,Inf,-Inf,NaN,""),"Basic",data$policy)

data$policy <- as.integer(factor(data$policy,levels = c("Basic","Endorsement","Basic or Endorsement"),
                                 labels = 1:3))

data <- doBy::summaryBy(list(c("liability_amount","indemnity_amount","total_premium_amount","subsidy_amount"),
                             c("policy","new_name","crop_yr")),FUN=sum,data=data,na.rm=T,keep.names=T)

data <- as.data.frame(dplyr::full_join(data,doBy::summaryBy(list(c("liability_amount"),"crop_yr"),FUN=sum,data=data,na.rm=T),by="crop_yr"))

for(vbl in c("liability_amount")){
  data[,vbl] <- data[,vbl]/data[,paste0(vbl,".sum")]
  data[,vbl] <- ifelse(data[,vbl] %in% c(NA,Inf,-Inf,NaN,""),NA,data[,vbl])
}

data <- data[names(data)[!grepl(".sum",names(data))]]

data.max <- doBy::summaryBy(crop_yr~new_name+policy,FUN=c(max,min),data=data,na.rm=T)
data.max <- data.max[order(-data.max$policy,-data.max$crop_yr.max,-data.max$crop_yr.min),]

data.max$y<-1:nrow(data.max)

data <- dplyr::inner_join(data,data.max,by=c("new_name","policy"))
data$y <- factor(data$y,levels = data.max$y,data.max$new_name)
data$liability_amount <- data$liability_amount*100
data$liability_amount <- ifelse(data$liability_amount %in% NA,0,data$liability_amount)
data$liability_amount <- ifelse(data$liability_amount >99,99,data$liability_amount)
data$rank <- cut(data$liability_amount, breaks=c(0,5,7.5,10,15,20,50,75,100), right = FALSE)
unique(data$rank)


study.plans <- c("Actual Production History","Yield Protection","Revenue Protection",
                 "Revenue Prot with Harvest Price Exclusion",
                 "Area Revenue Protection","Area Yield Protection",
                 "Area Revenue Protection - Harvest Price Exclusion",
                 "Margin Protection",
                 "Margin Protection with Harvest Price Option")

study.plans.labels <- ifelse(levels(data$y) %in% study.plans, yes = "bold", no = "plain")

data <- data[!data$y %in% c("Unclassified","PP"),]
data <- data[data$crop_yr>=year_beg,]
data$type<-"(A) Crop year offering and share of crop year liability in percentage"

lr <- doBy::summaryBy(list(c("indemnity_amount","total_premium_amount"),c("y","policy")),FUN=sum,data=data[data$crop_yr<=(year_end-1),],na.rm=T)
lr$lr <- (lr$indemnity_amount.sum/lr$total_premium_amount.sum)
lr$crop_yr <- lr$lr*3

lr <- lr[!lr$crop_yr %in% c(NA,0),]
lr$rank    <- "LR"
lr$type<-paste0("(B) Cumulative\nLoss Ratio (LR)\n(",year_beg,"-",year_end-1,")")

lr <- dplyr::inner_join(lr,unique(data[c("y","crop_yr.max","crop_yr.min")]))
lr <- lr[!lr$crop_yr.max - lr$crop_yr.min < 5,]
data.fig <- rbind(data[c("type","crop_yr","y","policy","rank")],lr[c("type","crop_yr","y","policy","rank")])


data.fig$policy <- factor(data.fig$policy,levels = 1:3,labels = c("(i) Basic provision","(ii) Endorsement","(i) or (ii)"))

library(ggplot2)
fig <- ggplot(data=data.fig, aes(x=crop_yr, y, fill = type, fill = color,shape=rank, group=crop_yr)) +
  geom_point(size = 2.5) +
  geom_vline(data=data.frame(type=paste0("(B) Cumulative\nLoss Ratio (LR)\n(",year_beg,"-",year_end-1,")"),x=3), 
             aes(xintercept=x),lwd=0.5, lty=2,color = "purple") +
  geom_vline(data=data.frame(type=paste0("(B) Cumulative\nLoss Ratio (LR)\n(",year_beg,"-",year_end-1,")"),
                             x=c(min(round(lr$crop_yr)),max(round(lr$crop_yr))+2)), 
             aes(xintercept=x),lwd=0.5, lty=2,color = "white") +
  scale_x_continuous(breaks = c(seq(1940,year_end,1),seq(0,10,1)),labels =c( seq(1940,year_end,1),sprintf("%.1f", round(seq(0,10,1)/3,1)))) +
  facet_grid(policy~type,scales="free",space = "free") +
  scale_fill_manual("",values = c("thistle","purple"),na.value = "white" ) +
  scale_shape_manual("",values = c(21:25,3,4,8,10,11)) +
  guides(color="none",fill="none",shape=guide_legend(nrow=1)) + 
  labs(title="",x="", y ="Insurance plan",caption = "") +
  ers_theme() + 
  theme_bw() + 
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=10),
        legend.title= element_blank(),
        axis.title.y= element_text(size=13),
        axis.title.x= element_blank(),
        axis.text.x = element_text(size=10,color="black",angle = 90,vjust = 0.5),
        axis.text.y = element_text(size=11),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))

write.csv(data.fig,"data-raw/figures/insurance_plan_history.csv")
ggsave("data-raw/figures/insurance_plan_history.png", fig,dpi = 600,width = 15, height = 12)









