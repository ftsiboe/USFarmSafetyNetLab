
# clear all output
library("rvest")
library(tidyverse)
# load libraries
# library(future.apply)
# library(magrittr)
# 
# 
# library("tidyr")
# library(dplyr)
# library(data.table)
# library(parallel)
# library(readxl)

source("data-raw/scripts/environment_setup.R")

# set up a multisession
#plan(multisession)


dir.create(paste0(dir_fastscratch,"/fsa_acreage/trash"), recursive = TRUE)
dir.create(paste0(dir_fastscratch,"/fsa_acreage/raw"), recursive = TRUE)

for(dir in c("fsa_acreage")){
  if (!dir.exists(paste0(dir_data_release,"/",dir))){
    dir.create(paste0(dir_data_release,"/",dir), recursive = TRUE)
  }
}

# define years to get data for
start_year <- 2009
end_year   <- as.numeric(substr(Sys.Date(),1,4)) # automatically set to current year

# custom function to extract release date
# get release dates of data to be referenced by the following function
# scrape release dates from FSA site to get day of release

# define url
URL <- "https://www.fsa.usda.gov/news-room/efoia/electronic-reading-room/frequently-requested-information/crop-acreage-data"

# read page 
pg <- rvest::read_html(URL)

# store cleaned release dates for later use
title  <- rvest::html_nodes(pg, "a") |> html_text()
refs   <- html_attr(rvest::html_nodes(pg, "a"),"href")
acreage_urls <- cbind.data.frame(title,refs)
colnames(acreage_urls) <- c("raw_string","url")
acreage_urls$day <- NA
acreage_urls$month <- NA
acreage_urls$year <- NA
acreage_urls$date <- as.Date(NA)
acreage_urls <- acreage_urls[grepl(".zip",acreage_urls$url),]

# clean dates
for(k in seq_len(length(acreage_urls$raw_string))){
  as_of_loc <- str_locate(acreage_urls$raw_string[k], "as of")[2]
  as_of_loc_underscore <- str_locate(acreage_urls$raw_string[k], "as_of")[2]
  for_loc <- str_locate(acreage_urls$raw_string[k], "for")[2]
  acre_data_loc <- str_locate(tolower(acreage_urls$raw_string[k]), "acreage data")[1]
  
  # only keep everything past "as of"
  if(!is.na(as_of_loc)){
    date <- substr(acreage_urls$raw_string[k],as_of_loc+1, nchar(acreage_urls$raw_string[k]))
  } else if(!is.na(for_loc)) {
    date <- substr(acreage_urls$raw_string[k],for_loc+1, nchar(acreage_urls$raw_string[k]))
  } else if(!is.na(acre_data_loc)){
    date <- substr(acreage_urls$raw_string[k],1,acre_data_loc-1)
    date <- gsub("Oct. ","October ", date)
    date <- gsub("Crop","", date)
  } else if(!is.na(as_of_loc_underscore)){
    date <- gsub(".zip","",substr(acreage_urls$raw_string[k],as_of_loc_underscore+2, nchar(acreage_urls$raw_string[k])))
  } 
  
  
  # trim white space and replace comma
  date <- gsub(" ","-",trimws(gsub(",","",date)))
  
  # convert to date
  date_temp <- as.Date(date, "%B-%d-%Y")
  if(is.na(date_temp)){
    dash_loc <- str_locate(date, "-")[1]
    date_temp <- paste0(substr(date,1,dash_loc),"01-",substr(date,dash_loc+1,nchar(date)))
    date_temp <- as.Date(date_temp, "%B-%d-%Y")
  } 
  if(is.na(date_temp)){
    dash_loc <- str_locate(date, "-")[1]
    date_temp <- paste0(substr(date,1,dash_loc),"01-",substr(date,dash_loc+1,nchar(date)))
    date_temp <- as.Date(date_temp, "%Y-%d-%B")
  } 
  if(is.na(date_temp)){
    date_temp <- as.Date(date, "%m%d%y")
  }
  date <- date_temp
  
  # separate date into components
  acreage_urls$day[k]   <- as.numeric(format(date,"%d"))
  acreage_urls$month[k] <- as.numeric(format(date,"%m"))
  acreage_urls$year[k]  <- as.numeric(format(date,"%Y"))
  acreage_urls$date[k]  <- date
  
  rd <- acreage_urls
  
}

acreage_urls$year <- ifelse(acreage_urls$year %in% NA,as.numeric(gsub("[^0-9]","",acreage_urls$raw_string)),acreage_urls$year)
acreage_urls$date <- as.Date(ifelse(acreage_urls$date %in% NA,as.Date(paste0(acreage_urls$year,"-01-01"), "%Y-%m-%d"),acreage_urls$date))
acreage_urls$day   <- as.numeric(format(acreage_urls$date,"%d"))
acreage_urls$month <- as.numeric(format(acreage_urls$date,"%m"))
acreage_urls$year  <- as.numeric(format(acreage_urls$date,"%Y"))

# remove missing observations
acreage_urls <- acreage_urls |> na.omit()

# add prefix to urls
acreage_urls$url <- paste0("https://www.fsa.usda.gov",acreage_urls$url)

# remove urls that aren't in the range of start_year to end_year
acreage_urls_in_range <- acreage_urls |> filter(year >= start_year, year <= end_year)


# download the raw data using the urls identified above
{
  # loop over each element of the above list and download the data
  lapply(
    1:nrow(acreage_urls_in_range),
    function(i){
      if(! paste0("raw_fsa_acreage_data_",acreage_urls_in_range$year[i],"_",format(acreage_urls_in_range$date[i],"%y%m%d"),".zip") %in% 
         list.files(path=paste0(dir_data_release,"/fsa_acreage"))){
        download.file(paste0(acreage_urls_in_range$url[i]),
                      destfile=paste0(dir_data_release,"/fsa_acreage/raw_fsa_acreage_data_",acreage_urls_in_range$year[i],
                                      "_",format(acreage_urls_in_range$date[i],"%y%m%d"),".zip"),mode="wb")
      } 
    })
  
  # clear the Trash folder
  file.remove(list.files(paste0(dir_fastscratch,"/fsa_acreage/trash"), full.names = T))
  
  # extract the zip files
  acreage_urls_in_range$file_name <- NA
  for(i in 1:nrow(acreage_urls_in_range)){
    tryCatch({
    zip_return <-  unzip(paste0(dir_fastscratch,"/fsa_acreage/raw_fsa_acreage_data_",acreage_urls_in_range$year[i],
                                "_",format(acreage_urls_in_range$date[i],"%y%m%d"),".zip"),
                         exdir=paste0(dir_fastscratch,"/fsa_acreage/trash"),overwrite=T)
    
    acreage_urls_in_range$file_name[i] <- zip_return
    
    print(i)
  }, error=function(e){})
    
  }

  # download the intended use codes ----------------------------------------------
  download.file("https://www.fsa.usda.gov/sites/default/files/documents/intended_use_codes.pdf",
                destfile=paste0(dir_data_release,"/fsa_acreage/","intended_use_codes.pdf"),mode="wb")
  
}










# 
# # clean the data that was downloaded above
# {
#   Fxn.headers <- function(file){
#     
#     year <- as.numeric(substr(gsub("./data-raw/fsaCropAcreage/Trash/|FINAL_|Revised_","",file),1,4))
#     if(year %in% c(2009:2011)){
#       sheet <- "Sheet1"
#     } else {
#       sheet <- "county_data"
#     }
#     # files <- list.files(paste0(Dr.FSA,"Trash"),
#     #                     pattern = paste0(year,"_fsa_acres"),
#     # full.names = T)
#     acres <- as.data.frame(readxl::read_excel(file, sheet = sheet))
#     acres <- data.frame(year=year,old=names(acres))
#     return(acres)
#   }
#   
#   # I try to avoid parallelization for reproducible code because you have to
#   # start considering system architecture. For example, mclapply does not allow
#   # more than one core to be used for windows systems. Thus, I usually write loops
#   # unless the code is impractically slow when run in series.
#   Headers <- list()
#   index <- 1
#   for(f in list.files(paste0(Dr.FSA,"Trash"),full.names = T) ){
#     Headers[[index]] <- Fxn.headers(f)
#     index <- index + 1
#   }
#   
#   # combine and covert to a data frame
#   headers_combined <- data.frame(rep(NA,nrow(Headers[[1]])))
#   for(k in 1:length(Headers)){
#     temp <- Headers[[k]]
#     year <- unique(temp$year)
#     names(temp) <- c("year",year)
#     temp <- temp[,-1]
#     headers_combined[,k] <- temp
#     colnames(headers_combined)[k] <- year
#   }
#   
#   
#   # States <- rgdal::readOGR(dsn =Dr.PLY,layer = "USA_States")@data
#   # 
#   # States$state_cd <- States$STATEFP
#   # States$state <- States$NAME
#   # States$state_ab  <- States$STUSPS 
#   # 
#   # Counties <- rgdal::readOGR(dsn = Dr.PLY,layer = "USA_Counties")@data
#   # 
#   # Counties$county_cd <- Counties$COUNTYFP
#   # Counties$county <- Counties$NAME
#   # Counties$state_cd <- Counties$STATEFP
#   
#   i <- unique(acreage_urls_in_range$date)[8]
#   for(i in unique(acreage_urls_in_range$date)){
#     
#     
#     meta_data <- acreage_urls_in_range[which(acreage_urls_in_range$date == i),]
#     if(nrow(meta_data) > 1){
#       stop("meta_data has more than 1 row")
#     }
#     
#     year <- meta_data$year
#     if(meta_data$month == 1){
#       year <- year - 1 # adjust to previous year if month is january
#     }
#     
#     if(year %in% c(2009:2011)){
#       sheet <- "Sheet1"
#     } else {
#       sheet <- "county_data"
#     }
#     
#     acres <- as.data.frame(readxl::read_excel(meta_data$file_name, sheet = sheet))
#     
#     if(acres[1,1] == "state_fsa_code"){
#       acres <- acres[-1,]
#     }
#     
#     names(acres) <- gsub("state_fsa_code","State Code",names(acres))
#     names(acres) <- gsub("county_fsa_code","County Code",names(acres))
#     names(acres) <- tolower(gsub(" ","_",names(acres)))
#     names(acres) <- gsub("crop_codes","crop_code",names(acres))
#     names(acres) <- gsub("state_county_code","fips",names(acres))
#     
#     
#     acres$state_cd <- stringr::str_pad(as.numeric(as.character(acres$state_code)),2,pad="0")
#     acres$county_cd <- stringr::str_pad(as.numeric(as.character(acres$county_code)),3,pad="0")
#     
#     acres$crop_yr <- year
#     
#     acres$release_date <- meta_data$date
#     acres$release_month <- meta_data$month
#     acres$release_year <- meta_data$year
#     acres$release_day <- meta_data$day
#     
#     # enforce column data types
#     for(c in c("state_code","county_code","crop_code","fips","planted_acres",
#                "volunteer_acres","failed_acres","prevented_acres",
#                "not_planted_acres",
#                "planted_and_failed_acres","state_cd","county_cd","crop_yr",
#                "release_month","release_year","release_day")){
#       acres[,c] <- as.numeric(acres[,c])
#     }
#     acres$release_date <- as.Date(acres$release_date)
#     
#     
#     saveRDS(acres,paste0(Dr.FSA,"Output","/fsa_acres_",gsub("-","_",meta_data$date),".rds"))
#     
#     
#     print(year)
#   }
#   
# }
# 
# 
# # combine into a single fsaCropAcreage dataset
# # I know there are more efficient ways to do this, but I'm trying to make
# # debugging easier in the future when something goes wrong. 
# fsaCropAcreage <- NULL
# # for(f in start_year:end_year){
# #   if(is.null(fsaCropAcreage)){
# #     fsaCropAcreage <- readRDS(paste0("./data-raw/fsaCropAcreage/Output/fsa_acres_",f,".rds"))
# #   } else {
# #     fsaCropAcreage <- rbind.data.frame(fsaCropAcreage, readRDS(paste0("./data-raw/fsaCropAcreage/Output/fsa_acres_",f,".rds")))
# #   }
# #   print(f)
# # }
# 
# fsaCropAcreage <- list.files(paste0(Dr.FSA,"Output"),
#                              pattern = ".rds", 
#                              full.names = T) |>
#   map(readRDS) |> 
#   bind_rows()
# 
# 
# # apply any final cleaning operations
# 
# # enforce common column naming conventions
# colnames(fsaCropAcreage) <- gsub("_code","_cd",colnames(fsaCropAcreage))
# colnames(fsaCropAcreage) <- gsub("_year","_yr",colnames(fsaCropAcreage))
# 
# # remove duplicated column names
# fsaCropAcreage <- fsaCropAcreage[,!duplicated(colnames(fsaCropAcreage))]
# 
# # convert columns to numeric where appropriate
# for(k in seq_len(ncol(fsaCropAcreage))){
#   
#   if(!"Date" %in% class(fsaCropAcreage[,k])){
#     
#     # check to see how many NA values are in the column
#     old_na_count <- sum(is.na(fsaCropAcreage[,k]))
#     
#     # convert the column to numeric
#     temp <- as.numeric(fsaCropAcreage[,k])
#     
#     # check to see how many NA values there are after conversion to numeric
#     new_na_count <- sum(is.na(temp))
#     
#     test <-  data.frame(is.na(fsaCropAcreage[,k]),is.na(temp))
#     
#     # check to ensure the new_na_count is not greater than the old_na_count
#     # which represents a loss of information during convertion. If there was 
#     # no loss of information, proceed with storing the converted column in the 
#     # fsaCropAcreage data frame
#     if(new_na_count == old_na_count){
#       fsaCropAcreage[,k] <- temp
#     }
#     
#   }
# }
# 
# 
# # get rid of seperate release day, monthy, and year columns (they are too confusing in conjuncture with crop year column)
# #fsaCropAcreage$release_yr[which(fsaCropAcreage$release_month == 1)] <- fsaCropAcreage$release_yr[which(fsaCropAcreage$release_month == 1)] - 1 
# #fsaCropAcreage <- fsaCropAcreage |> select(-release_month, -release_yr, -release_day)
# 
# 
# # offset dates by one day (to correct them)
# #saCropAcreage$release_date <- fsaCropAcreage$release_date + lubridate::days(1)
# 
# # increase date by one year for january releases
# #fsaCropAcreage$release_month <- lubridate::month(fsaCropAcreage$release_date)
# 
# #fsaCropAcreage$release_date # increase by one month if release month is january
# 
# 
# # create a current release column
# dates <- distinct(fsaCropAcreage |> select(crop_yr, release_date)) |>
#   group_by(crop_yr) |>
#   summarize(max_date = max(release_date))
# 
# fsaCropAcreage$current_release <- F
# fsaCropAcreage$current_release[which(fsaCropAcreage$release_date %in% dates$max_date)] <- T
# 
# 
# 
# # convert the data to a tibble
# fsaCropAcreage <- dplyr::as_tibble(fsaCropAcreage)
# 
# 
# # use the aggregated file in the package data folder
# usethis::use_data(fsaCropAcreage, overwrite = TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #   
# #   
# #   file_names <- dir(paste0(Dr.FSA,"Output"), full.names = T) # where you have your files
# # fsaCropAcreage <- do.call(rbind,lapply(file_names,readRDS))
# # 
# #   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #   # define a function that cleans the data
# #   Fxn.Clean <- function(year){
# #     # year <- 2021
# #     if(year %in% c(2009:2011)){
# #       sheet <- "Sheet1"
# #     } else {
# #       sheet <- "county_data"
# #     }
# #     
# #     acres <- as.data.frame(readxl::read_excel(list.files(paste0(Dr.FSA,"Trash"),pattern = paste0(year,"_fsa_acres"),full.names = T), sheet = sheet))
# #     #headers <- Headers[Headers$old %in% names(acres) & Headers$year %in% year,]
# #     #headers <- Headers[Headers$year %in% year,]
# #     
# #     #acres <- acres[headers$old]
# #     #names(acres) <- headers$new
# #     #acres$year <- year
# #     
# #     names(acres) <- gsub("state_fsa_code","State Code",names(acres))
# #     names(acres) <- gsub("county_fsa_code","County Code",names(acres))
# #     names(acres) <- tolower(gsub(" ","_",names(acres)))
# #     
# #     acres$state_cd <- stringr::str_pad(as.numeric(as.character(acres$state_code)),2,pad="0")
# #     acres$county_cd <- stringr::str_pad(as.numeric(as.character(acres$county_code)),3,pad="0")
# #     #acres$Intended_Use <-toupper(acres$Intended_Use)
# #     #acres$Intended_Use_cd <- acres$Intended_Use
# #     
# #     # acres <- dplyr::full_join(Intended,acres,by="Intended_Use_cd")
# #     # acres <- acres[!acres$year %in% NA,]
# #     # acres$Intended_Use_nm <- ifelse(acres$Intended_Use_nm %in% c(NA,""),acres$Intended_Use,acres$Intended_Use_nm)
# #     
# #     #acres <- dplyr::full_join(acres,States[c("state_cd","state","state_ab")],by="state_cd")
# #     #acres <- acres[!acres$year %in% NA,]
# #     
# #     #acres <- dplyr::full_join(acres,Counties[c("state_cd","county_cd","county")],by=c("state_cd","county_cd"))
# #     #acres <- acres[!acres$year %in% NA,]
# #     
# #     #acres$use_cd <- ifelse(grepl("GRAIN",toupper(acres$Intended_Use_cd)),16,NA)
# #     #acres$use_cd <- ifelse(grepl("SILAGE",toupper(acres$Intended_Use_cd)) &  acres$use_cd %in% NA,26,acres$use_cd)
# #     #acres$use_cd <- ifelse(grepl("SEED",toupper(acres$Intended_Use_cd)) &  acres$use_cd %in% NA,9,acres$use_cd)
# #     
# #     # acres$use_cd <- ifelse(grepl("GRAIN",toupper(acres$Intended_Use_nm))  &  acres$use_cd %in% NA,16,acres$use_cd)
# #     # acres$use_cd <- ifelse(grepl("SILAGE",toupper(acres$Intended_Use_nm)) &  acres$use_cd %in% NA,26,acres$use_cd)
# #     # acres$use_cd <- ifelse(grepl("SEED",toupper(acres$Intended_Use_nm)) &  acres$use_cd %in% NA,9,acres$use_cd)
# #     
# #     #tryCatch({ 
# #     #  acres$irrg_cd <- ifelse(toupper(acres$Irrigation_Practice) %in% c("I","IRRIGATED"),2,NA)
# #     #  acres$irrg_cd <- ifelse(toupper(acres$Irrigation_Practice) %in% c("N","NON-IRRIGATED") &  acres$irrg_cd %in% NA,3,acres$irrg_cd)
# #     #}, error=function(e){})
# #     
# #     #headers <- c("year","state_cd","state","state_ab","county_cd","county",
# #     #             "crop_cd","crop","Irrigation_Practice","irrg_cd","type_cd","Intended_Use_nm","Intended_Use_cd","use_cd",
# #     #             "Planted_Acres","Not_Planted_Acres","Prevented_Acres","Failed_Acres","Volunteer_Acres")
# #     
# #     #for(col in headers){
# #     #  if(!col %in% names(acres)) acres[,col] <- NA
# #     #}
# #     
# #     #for(col in c("year","state_cd","county_cd","crop_cd","irrg_cd","type_cd","use_cd",
# #     #             "Planted_Acres","Not_Planted_Acres","Prevented_Acres","Failed_Acres","Volunteer_Acres")){
# #     #  acres[,col] <- as.numeric(as.character(acres[,col]))
# #     #}
# #     
# #     #headers <- headers[headers %in% names(acres)]
# #     
# #     #acres <- acres[complete.cases(acres[c("state_cd","county_cd","crop_cd")]),headers]
# #     
# #     # write.csv(acres,paste0(Dr.FSA,"Output","/fsa_acres_",year,".csv"),na = "")
# #     saveRDS(acres,paste0(Dr.FSA,"Output","/fsa_acres_",year,".rds"))
# #     
# #     return(year)
# #   }
# #   
# #   # loop through each year and apply cleaning function
# #   
# #   future_lapply(start_year:max(acreage_urls$year),Fxn.Clean)
# #   
# #   # delete the files stored in Trash
# #   unlink(paste0(Dr.FSA,"Trash"),recursive=T)
# # }  
# 
# # # combine all individual files into one aggregated file
# # file_names <- dir(paste0(Dr.FSA,"Output"), full.names = T) # where you have your files
# # fsaCropAcreage <- do.call(rbind,lapply(file_names,readRDS))
# # 
# # # any final cleaning
# # row.names(fsaCropAcreage) <- 1:nrow(fsaCropAcreage)
# # 
# # # convert the data to a tibble
# # fsaCropAcreage <- dplyr::as_tibble(fsaCropAcreage)
# # 
# # # use the aggregated file in the package data folder
# # usethis::use_data(fsaCropAcreage, overwrite = TRUE)
# # 
# # 
# # 
# 
# 
# 
# 
