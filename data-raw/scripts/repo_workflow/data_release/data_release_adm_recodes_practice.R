
source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

sob <- as.data.frame(readRDS(paste0(dir_data_release,"/sob/sobtpu_all.rds")))
sob <- unique(sob[c("commodity_year","commodity_code","practice_code","practice_name")])
names(sob) <- c("crop_yr","crop_cd","pract_cd","pract")

sob$crop_yr  <- as.numeric(as.character(sob$crop_yr))
sob$pract_cd <- as.numeric(as.character(sob$pract_cd))
sob$crop_cd  <- as.numeric(as.character(sob$crop_cd))
sob$pract    <- toupper(sob$pract)

adm <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2011:as.numeric(format(Sys.Date(), "%Y")),
      function(year) {
        tryCatch({ 
          df <- get_adm_data(year = year,dataset = "A00510_Practice", force=TRUE)
          df$commodity_year <- year
          return(df)
        }, error = function(e){return(NULL)})
      }), fill = TRUE))

adm <- unique(adm[c("commodity_year","commodity_code","practice_code","practice_name","practice_abbreviation")])
names(adm) <- c("crop_yr","crop_cd","pract_cd","pract","pract_ab")

adm$crop_yr  <- as.numeric(as.character(adm$crop_yr))
adm$pract_cd <- as.numeric(as.character(adm$pract_cd))
adm$crop_cd  <- as.numeric(as.character(adm$crop_cd))

adm <- rbind(unique(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/1995-2010 clean/CROSS_REFERENCE_DATA_1997_2010.rds"))[names(adm)]),adm)
adm <- adm[c("crop_yr","crop_cd","pract_cd","pract","pract_ab")]
names(adm) <- c("crop_yr","crop_cd","pract_cd","pract_adm","pract_ab")
adm$pract_adm <- toupper(adm$pract_adm)

data <- dplyr::full_join(adm,sob,by=c("crop_yr","crop_cd","pract_cd"))
data <- unique(data)

data$non_irrg_rc <- 0
for(iri in c("NON-IRR","NON IRRIGATED","NON-IRRIGATED")){
  data$non_irrg_rc <- ifelse(grepl(iri,data$pract),1,data$non_irrg_rc)
  data$non_irrg_rc <- ifelse(grepl(iri,data$pract_adm),1,data$non_irrg_rc)
}

data$irrg_rc <- 0
for(iri in c("IRR","ORGANIC(CERTIFIED) IRR.","ORGANIC(TRANSITIONAL) IRR.","IRRIGATED","FAC (IRRIGATED)")){
  data$irrg_rc <- ifelse(grepl(iri,data$pract) & data$non_irrg_rc %in% 0,1, data$irrg_rc)
  data$irrg_rc <- ifelse(grepl(iri,data$pract_adm) & data$non_irrg_rc %in% 0,1, data$irrg_rc)
}

data$irrgate_rc <- ifelse(data$non_irrg_rc %in% 1,"NON-IRR","Other")
data$irrgate_rc <- ifelse(data$irrg_rc %in% 1,"IRR",data$irrgate_rc)
data$irrgate_rc <- ifelse(data$irrgate_rc %in% NA ,"NO PRACTICE SPECIFED",data$irrgate_rc)

# unique(data[data$irrgate_rc %in% "Other","pract"])

data$organic_rc <- ifelse(grepl("ORGANIC",data$pract)  | grepl("ORGANIC",data$pract_adm),"ORGANIC",NA)
data$organic_rc <- ifelse(grepl("(OC)",data$pract)     | grepl("(OC)",data$pract_adm),"ORGANIC",data$organic_rc)
data$organic_rc <- ifelse(grepl("(OT)",data$pract)     | grepl("(OT)",data$pract_adm),"ORGANIC",data$organic_rc)

data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("(OC)",data$pract_adm),"ORGANIC (CERTIFIED)",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("CERTIFIED",data$pract_adm),"ORGANIC (CERTIFIED)",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("(OC)",data$pract),"ORGANIC (CERTIFIED)",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("CERTIFIED",data$pract),"ORGANIC (CERTIFIED)",data$organic_rc)

data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("(OT)",data$pract_adm),"ORGANIC (TRANSITIONAL)",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("TRANSITIONAL",data$pract_adm),"ORGANIC (TRANSITIONAL)",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("(OT)",data$pract),"ORGANIC (CERTIFIED)",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% "ORGANIC" & grepl("TRANSITIONAL",data$pract),"ORGANIC (TRANSITIONAL)",data$organic_rc)

data$organic_rc <- ifelse(data$organic_rc %in% NA & data$pract     %in% "NO PRACTICE SPECIFED","NO PRACTICE SPECIFED",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% NA & data$pract_adm %in% "NO PRACTICE SPECIFED","NO PRACTICE SPECIFED",data$organic_rc)
data$organic_rc <- ifelse(data$organic_rc %in% NA ,"NO PRACTICE SPECIFED",data$organic_rc)

# unique(data[data$organic_rc %in% NA,"pract"])

# unique(data[grepl("CONTI",data$pract),"pract"])

data$fallow_rc <- ifelse(grepl("CONTI",data$pract)  | grepl("CONTI",data$pract_adm),"CONTINUOUS CROPPING",NA)
data$fallow_rc <- ifelse(grepl("FALLOW",data$pract)  | grepl("FALLOW",data$pract_adm),"FALLOW",data$fallow_rc )

data <- unique(data[c("crop_yr","crop_cd","pract_cd","pract","irrgate_rc","organic_rc","fallow_rc")])
names(data) <- c("commodity_year","commodity_code","practice_code","practice_name","irrigation_recode","organic_recode","fallow_recode")

data <- as.data.table(data[complete.cases(data[c("commodity_year","commodity_code","practice_code")]),])
