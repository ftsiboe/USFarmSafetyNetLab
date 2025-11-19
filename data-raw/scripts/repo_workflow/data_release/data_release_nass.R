
rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

source("data-raw/scripts/repo_workflow/environment_setup.R")

# unlink(list.files(paste0(dir_data_release,"/nass"), full.names = TRUE, recursive = TRUE))

devtools::document()

# source("R/helper_nass.R")

Keep.List<-c("Keep.List",ls())

# Download database
downloaded_nass_large_datasets(
  large_datasets = c(
    paste0("census", c(2022, 2017, 2012, 2007, 2002)),"economics","crops"),
  dir_dest = "./data-raw/fastscratch/nass/")

# crop land acres - Census               
rm(list= ls()[!(ls() %in% c(Keep.List))])
dir_ag_census <- "./data-raw/fastscratch/nass/"
data_census <- data.table::rbindlist(
  lapply(
    list.files(dir_ag_census,pattern = "qs.census",full.names = T), 
    function(census_file){ 
      tryCatch({ 
        # census_file  <- list.files(dir_ag_census,pattern = "qs.census",full.names = T)[5]
        
        data <-  as.data.frame(data.table::fread(census_file))
       
        data <- data[data$DOMAIN_DESC %in% c("TOTAL"),]
        data <- data[data$AGG_LEVEL_DESC %in% c("COUNTY"),]
        data <- data[data$DOMAINCAT_DESC %in% c("NOT SPECIFIED"),]
        data <- data[data$REFERENCE_PERIOD_DESC %in% c("YEAR"),]
        data <- data[
          data$SHORT_DESC %in% 
            c("AG LAND, OWNED, IN FARMS - ACRES",
              "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES",
              "AG LAND, CROPLAND - ACRES",
              "AG LAND, PASTURELAND - ACRES",
              "AG LAND, CROPLAND, PASTURED ONLY - ACRES") ,]
       
        names(data) <- tolower(names(data))
        
        data$variable <- ifelse(data$short_desc %in% "AG LAND, OWNED, IN FARMS - ACRES","ag_land_owned",NA)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES","ag_land_rented",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, CROPLAND - ACRES","crop_land",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, PASTURELAND - ACRES","pasture_land",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, CROPLAND, PASTURED ONLY - ACRES","crop_and_pasture_land",data$variable)
  
        data$value <- as.numeric(gsub(",","",as.character(data$value)))
        data <- data[! data$value %in% c(0,NA,Inf,-Inf,NaN),]

        data$state_code <- data$state_fips_code
        data$county_code <- data$county_code
        data$census_year <- data$year

        data <- doBy::summaryBy(value~variable+census_year+state_code+county_code
                               ,FUN=sum,na.rm=T,data=data,keep.names = T)
     
        data <- data |> tidyr::spread(variable, value)
        
        data$ag_land <- rowSums(data[c("ag_land_rented","ag_land_owned")],na.rm=TRUE)
        
        gc()
        
        return(data)
      }, error = function(e){return(NULL)})
    }),fill = T)
data_census[, data_source := "USDA NASS Quick Stats"]
saveRDS(data_census,file=paste0(dir_data_release,"/nass/nass_census_ag_land_acres.rds")) 


# BRF Census               
rm(list= ls()[!(ls() %in% c(Keep.List))])
source("data-raw/scripts/repo_workflow/data_release/data_release_nass_census_bfr.R")
brf_census[, data_source := "USDA NASS Quick Stats"]
saveRDS(brf_census,file=paste0(dir_data_release,"/nass/nass_census_state_beginning_farmer_and_rancher_data.rds")) 

# INDEX FOR PRICE RECEIVED, 2011       
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- process_nass_dataset(
  dir_source  = paste0(dir_fastscratch,"/nass"),
  large_dataset = "economics",
  nassqs_params = list(short_desc = "COMMODITY TOTALS - INDEX FOR PRICE RECEIVED, 2011",
                       freq_desc = "ANNUAL"))
df <- df[freq_desc %in% "ANNUAL",
         .(index_for_price_recived = mean(value, na.rm = TRUE)),
         by = c("commodity_year")]
df[, data_source := "USDA NASS Quick Stats"]
saveRDS(df,file=paste0(dir_data_release,"/nass/nass_index_for_price_recived.rds"))

# nass us ag price index monthly       
rm(list= ls()[!(ls() %in% c(Keep.List))])
datalist <- c("https://www.nass.usda.gov/Charts_and_Maps/graphics/data/received.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/fruit_veg.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/grains_oilseeds.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/livestock.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/pricetb.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/priceca.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/pricecn.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/pricect.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/pricehg.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/pricemk.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/pricesb.txt",
              "https://www.nass.usda.gov/Charts_and_Maps/graphics/data/pricewh.txt")

data <- as.data.frame(
  data.table::rbindlist(
    lapply(
      1:length(datalist),
      function(i){
        #i <- 1
        DONE <-NULL
        tryCatch({ 
          data        <- as.data.frame(readr::read_delim(datalist[i], delim = ":", escape_double = FALSE, trim_ws = TRUE, skip = 3))
          names(data) <- paste0("c",1:ncol(data))
          data        <- tidyr::separate(data,"c1",into=c("x1","x2"),sep=" ",remove = F)
          data        <- tidyr::separate(data,"c2",into=paste0("data",1:10),sep="\\s+",remove = F)
          data        <- data[,colSums(is.na(data))<nrow(data)]
          data$year   <- as.numeric(as.character(data$x1))
          data$month  <- ifelse(data$year %in% NA,data$x1,data$x2)
          data$year   <- zoo::na.locf(data$year,na.rm = F)
          data        <- data[c("year","month",names(data)[grepl("data",names(data))])]
          names       <- as.data.frame(readr::read_delim(datalist[i], delim = ":", escape_double = FALSE, trim_ws = TRUE))[1,1]
          names       <- stringr::str_split(names,"and")[[1]]
          names       <- c(stringr::str_split(names[1],",")[[1]],names[2])
          names       <- names[!names %in% c(" ",NA)]
          names       <- trimws(gsub("\\s+", " ", gsub("[\r\n]", "", names)), which = c("both"))
          names(data) <- c("year","month",names)
          data <- data %>%  tidyr::gather(comm, index, 3:ncol(data))
          data$index <- as.numeric(as.character(data$index))
          DONE <- data
          
        }, error=function(e){})
        return(DONE)
      }), fill = TRUE))

data$Date  <- as.Date(paste0(data$year,data$month,"01"),format = "%Y%B%d")
data$Lab   <- format(data$Date,"%Y \n%b")

head(data)

data <- data[complete.cases(data),]
labs <- unique(data[data$month %in% c("January","May","September"),c("Date","Lab")])
data$commX <- ifelse(data$comm %in% "Agricultural",1,NA)
data$commX <- ifelse(data$comm %in% "Crop",2,data$commX)
data$commX <- ifelse(data$comm %in% "Livestock Production",3,data$commX)
data$commX <- ifelse(data$commX %in% NA,0,data$commX)
data$commX <- factor(data$commX,levels = 0:3,labels = c("Commodity specific","Agricultural","Crop","Livestock") )

data <- as.data.table(data)
data[, data_source := "USDA NASS: https://www.nass.usda.gov/Charts_and_Maps/graphics/data"]
saveRDS(data,file=paste0(dir_data_release,"/nass/nass_us_ag_price_index_monthly.rds"))

# Get Marketing Year Average Price          
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <-  get_marketing_year_avg_price(
  dir_source = paste0(dir_fastscratch,"/nass"),
  agg_level_desc = c("NATIONAL","STATE"),
  short_desc = c(
    "OATS - PRICE RECEIVED, MEASURED IN $ / BU",
    "RYE - PRICE RECEIVED, MEASURED IN $ / BU",
    "TOBACCO - PRICE RECEIVED, MEASURED IN $ / LB",
    "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
    "FLAXSEED - PRICE RECEIVED, MEASURED IN $ / BU",
    "BARLEY - PRICE RECEIVED, MEASURED IN $ / BU",
    "BEANS, DRY EDIBLE, INCL CHICKPEAS - PRICE RECEIVED, MEASURED IN $ / CWT",
    "HAY - PRICE RECEIVED, MEASURED IN $ / TON",
    "WHEAT - PRICE RECEIVED, MEASURED IN $ / BU",
    "COTTON - PRICE RECEIVED, MEASURED IN $ / LB",
    "SORGHUM, GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
    "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
    "SUGARBEETS - PRICE RECEIVED, MEASURED IN $ / TON",
    "PEAS, DRY EDIBLE - PRICE RECEIVED, MEASURED IN $ / CWT",
    "SUNFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT",
    "RICE - PRICE RECEIVED, MEASURED IN $ / CWT",
    "PEANUTS - PRICE RECEIVED, MEASURED IN $ / LB",
    "CANOLA - PRICE RECEIVED, MEASURED IN $ / CWT",
    "MAPLE SYRUP - PRICE RECEIVED, MEASURED IN $ / GALLON",
    "RICE, LONG GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
    "MILLET, PROSO - PRICE RECEIVED, MEASURED IN $ / BU",
    "SUGARCANE - PRICE RECEIVED, MEASURED IN $ / TON",
    "SAFFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT"))

df[, data_source := "USDA NASS Quick Stats"]
saveRDS(df,file=paste0(dir_data_release,"/nass/nass_marketing_year_avg_price.rds"))

# State rental rates                   
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_state_rental_rates()
df[, data_source := "Output from get_state_rental_rates() function"]
saveRDS(df,file=paste0(dir_data_release,"/nass/nass_state_rental_rates.rds"))

# Aggregate NASS Production Data              
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_nass_production_data(
  dir_source      = "./data-raw/fastscratch/nass/",
  source_desc     = "SURVEY",
  agg_level_desc  = c("NATIONAL","STATE","COUNTY"))
df[, data_source := "USDA NASS Quick Stats"]
saveRDS(df,file=paste0(dir_data_release,"/nass/nass_production_data.rds"))



# Upload the assets
piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/nass"), full.names = TRUE, recursive = TRUE),
  repo  = "ftsiboe/USFarmSafetyNetLab",
  tag   = "nass_extracts",
  overwrite = TRUE
)

