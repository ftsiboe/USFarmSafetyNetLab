#----------------------------------------------------
# Initialize environment                          ####
rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

source("data-raw/scripts/repo_workflow/environment_setup.R")

# unlink(list.files(paste0(dir_data_release,"/nass"), full.names = TRUE, recursive = TRUE))

devtools::document()

# source("R/helper_nass.R")

Keep.List<-c("Keep.List",ls())
#----------------------------------------------------
# Download database                               ####
downloaded_nass_large_datasets(
  large_datasets = c(
    paste0("census", c(2022, 2017, 2012, 2007, 2002)),"economics","crops","animals_products"),
  dir_dest = "./data-raw/fastscratch/nass/")
#----------------------------------------------------
# Animal Inventory                                ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
animal_inventory <- process_nass_dataset(
  dir_source    = paste0(dir_fastscratch,"/nass"),
  large_dataset = "animals_products",
  nassqs_params = list(
    freq_desc = "POINT IN TIME" , 
    agg_level_desc = "COUNTY" ,
    domain_desc = "TOTAL" ,
    country_name = "UNITED STATES" , 
    util_practice_desc = "ALL UTILIZATION PRACTICES" ,
    prodn_practice_desc = "ALL PRODUCTION PRACTICES" ,
    short_desc = c("CATTLE, INCL CALVES - INVENTORY",
                   "SHEEP, INCL LAMBS - INVENTORY",
                   "GOATS - INVENTORY"),
    reference_period_desc = "FIRST OF JAN" 
    
  )
)
animal_inventory <- animal_inventory[
  , .(value = sum(value, na.rm = TRUE)),
  by = c("commodity_year","state_code","county_code","commodity_name")]

animal_inventory[,commodity_name := tolower(commodity_name)]
animal_inventory <- animal_inventory |> tidyr::spread(commodity_name, value)
animal_inventory <- as.data.table(animal_inventory)

contiguousCounty <- readRDS(file.path(dir_data_release,"adm","fcip_contiguous_county.rds"))
setnames(contiguousCounty,
         old = c("state_code", "county_code", "contiguous_state_code", "contiguous_county_code"),
         new = c("original_state_code", "original_county_code", "state_code", "county_code"))
contiguousCounty <- merge(
  animal_inventory, contiguousCounty, 
  by = intersect(names(animal_inventory), names(contiguousCounty)), all.x = TRUE, allow.cartesian = TRUE)
contiguousCounty <- contiguousCounty[
  , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
  by = c("commodity_year","original_state_code","original_county_code"),
  .SDcols = c("cattle","goats","sheep")]
setnames(contiguousCounty,
         old = c("original_state_code", "original_county_code","cattle","goats","sheep"),
         new = c("state_code", "county_code","contiguous_cattle","contiguous_goats","contiguous_sheep"))
animal_inventory <- merge(
  animal_inventory, contiguousCounty, 
  by = intersect(names(animal_inventory), names(contiguousCounty)), all.x = TRUE)

animal_inventory[contiguous_cattle %in% c(NA,Inf,NaN,-Inf), contiguous_cattle := 0]
animal_inventory[contiguous_goats %in% c(NA,Inf,NaN,-Inf), contiguous_goats := 0]
animal_inventory[contiguous_sheep %in% c(NA,Inf,NaN,-Inf), contiguous_sheep := 0]
animal_inventory[cattle %in% c(NA,Inf,NaN,-Inf), cattle := 0]
animal_inventory[goats %in% c(NA,Inf,NaN,-Inf), goats := 0]
animal_inventory[sheep %in% c(NA,Inf,NaN,-Inf), sheep := 0]

animal_inventory[, data_source := "USDA NASS Quick Stats - Animal inventory as of first of Jan"]
saveRDS(animal_inventory,file=file.path(dir_data_release,"nass/nassSurveyAnimalInventory.rds"))
#----------------------------------------------------
# Census - Ag land                                ####    
rm(list= ls()[!(ls() %in% c(Keep.List))])
data_census <- data.table::rbindlist(
  lapply(
    list.files(paste0(dir_fastscratch,"/nass"),pattern = "qs.census",full.names = T), 
    function(census_file){ 
      tryCatch({ 
        # census_file  <- list.files(paste0(dir_fastscratch,"/nass"),pattern = "qs.census",full.names = T)[4]
        
        data <-  as.data.frame(data.table::fread(census_file))
 
        data <- data[data$DOMAIN_DESC %in% c("TOTAL"),]
        data <- data[data$AGG_LEVEL_DESC %in% c("COUNTY"),]
        data <- data[data$DOMAINCAT_DESC %in% c("NOT SPECIFIED"),]
        # data <- data[data$UNIT_DESC %in% c("ACRES"),]
        # 
        # data <- data[data$COMMODITY_DESC %in% c("AG LAND"),]
        
        # unique(data$SHORT_DESC)
        
         data <- rbind(
          data[ data$REFERENCE_PERIOD_DESC %in% "YEAR" & 
            data$SHORT_DESC %in% 
              c("AG LAND, OWNED, IN FARMS - ACRES",
                "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES",
                "AG LAND, CROPLAND - ACRES",
                "AG LAND, PASTURELAND - ACRES",
                "AG LAND, CROPLAND, PASTURED ONLY - ACRES",
                "HAY, (EXCL ALFALFA) - ACRES HARVESTED",
                "HAY, ALFALFA - ACRES HARVESTED",
                "HAY - ACRES HARVESTED") ,],
          data[data$REFERENCE_PERIOD_DESC %in% "END OF DEC" & 
            data$SHORT_DESC %in% c("AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE",
                                   "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $"),]
        )

        names(data) <- tolower(names(data))
        data$variable <- ifelse(data$short_desc %in% "AG LAND, OWNED, IN FARMS - ACRES","ag_land_owned",NA)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES","ag_land_rented",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, CROPLAND - ACRES","crop_land",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, PASTURELAND - ACRES","pasture_land",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, CROPLAND, PASTURED ONLY - ACRES","crop_and_pasture_land",data$variable)
        data$variable <- ifelse(data$short_desc %in% "HAY, (EXCL ALFALFA) - ACRES HARVESTED","hay_other",data$variable)
        data$variable <- ifelse(data$short_desc %in% "HAY, ALFALFA - ACRES HARVESTED","hay_alfalfa",data$variable)
        data$variable <- ifelse(data$short_desc %in% "HAY - ACRES HARVESTED","hay_all",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE","ag_land_value_per_acre",data$variable)
        data$variable <- ifelse(data$short_desc %in% "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $","ag_land_value",data$variable)
        
        data$value <- as.numeric(gsub(",","",as.character(data$value)))
        data <- data[! data$value %in% c(0,NA,Inf,-Inf,NaN),]

        data$state_code <- data$state_fips_code
        data$county_code <- data$county_code
        data$census_year <- data$year

        data <- doBy::summaryBy(value~variable+census_year+state_code+county_code
                               ,FUN=sum,na.rm=T,data=data,keep.names = T)
     
        data <- data |> tidyr::spread(variable, value)
   
        data$ag_land <- data$ag_land_value/data$ag_land_value_per_acre
        
        gc()
        
        return(data)
      }, error = function(e){return(NULL)})
    }),fill = T)
data_census[, data_source := "USDA NASS Quick Stats"]
saveRDS(data_census,file=file.path(dir_data_release,"nass/agCensusAcres.rds")) 
#----------------------------------------------------
# Census - BRF                                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
brf_census <- data.table::rbindlist(
  lapply(
    list.files(paste0(dir_fastscratch,"/nass"),pattern = "qs.census",full.names = T), 
    function(census_file){ 
      tryCatch({ 
        data <-  as.data.frame(data.table::fread(census_file))
        
        bfr <- data[data$DOMAIN_DESC %in% c("PRODUCERS, ON ANY OPERATION","TOTAL"),]
        bfr <- bfr[bfr$DOMAINCAT_DESC %in% c("PRODUCERS, ON ANY OPERATION: (LESS THAN 11 YEARS)","NOT SPECIFIED"),]
        bfr <- bfr[bfr$AGG_LEVEL_DESC %in% c("STATE","NATIONAL"),]
        
        bfr <- bfr[bfr$SHORT_DESC %in% c(
          "AG LAND, OWNED, IN FARMS - NUMBER OF OPERATIONS",
          "AG LAND, OWNED, IN FARMS - ACRES",
          "AG LAND, RENTED FROM OTHERS, IN FARMS - NUMBER OF OPERATIONS",
          "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES",
          "GOVT PROGRAMS, FEDERAL - OPERATIONS WITH RECEIPTS",
          "GOVT PROGRAMS, FEDERAL - RECEIPTS, MEASURED IN $") ,]
        
        names(bfr) <- tolower(names(bfr))
        bfr$value <- as.numeric(gsub(",","",as.character(bfr$value)))
        bfr <- bfr[! bfr$value %in% c(0,NA,Inf,-Inf,NaN),]
        
        table(bfr$short_desc,bfr$commodity_desc)
        
        bfr <- doBy::summaryBy(value~year+state_fips_code+state_alpha+agg_level_desc+unit_desc+commodity_desc+domaincat_desc+short_desc
                               ,FUN=mean,na.rm=T,data=bfr,keep.names = T)
        
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, OWNED, IN FARMS - NUMBER OF OPERATIONS","operations_owned",NA)
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, OWNED, IN FARMS - ACRES","acres_owned",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, RENTED FROM OTHERS, IN FARMS - NUMBER OF OPERATIONS","operations_rented",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES","acres_rented",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "GOVT PROGRAMS, FEDERAL - OPERATIONS WITH RECEIPTS","govt_receipts_operations",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "GOVT PROGRAMS, FEDERAL - RECEIPTS, MEASURED IN $","govt_receipts_value",bfr$variable)
        
        bfr$variable <- paste0(ifelse(bfr$domaincat_desc %in% "NOT SPECIFIED","all","bfr"),"_",bfr$variable)
        bfr$state_code <- bfr$state_fips_code
        bfr <- doBy::summaryBy(value~year+state_code+state_alpha+variable
                               ,FUN=sum,na.rm=T,data=bfr,keep.names = T)
        bfr <- bfr |> tidyr::spread(variable, value)
        gc()
        
        return(bfr)
      }, error = function(e){return(NULL)})
    }),fill = T)
brf_census[, data_source := "USDA NASS Quick Stats"]
saveRDS(brf_census,file=file.path(dir_data_release,"nass/agCensusBFR.rds")) 
#----------------------------------------------------
# INDEX FOR PRICE RECEIVED, 2011                  ####  
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
saveRDS(df,file=paste0(dir_data_release,"/nass/nassSurveyPriceRecivedIndex.rds"))
#----------------------------------------------------
# Ag price index monthly                          ####  
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

data <- data[complete.cases(data),]
labs <- unique(data[data$month %in% c("January","May","September"),c("Date","Lab")])
data$commX <- ifelse(data$comm %in% "Agricultural",1,NA)
data$commX <- ifelse(data$comm %in% "Crop",2,data$commX)
data$commX <- ifelse(data$comm %in% "Livestock Production",3,data$commX)
data$commX <- ifelse(data$commX %in% NA,0,data$commX)
data$commX <- factor(data$commX,levels = 0:3,labels = c("Commodity specific","Agricultural","Crop","Livestock") )

data <- as.data.table(data)
data[, data_source := "USDA NASS: https://www.nass.usda.gov/Charts_and_Maps/graphics/data"]
saveRDS(data,file=paste0(dir_data_release,"/nass/nassAgPriceMonthlyIndex.rds"))
#----------------------------------------------------
# Marketing Year Average Price                    #### 
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
saveRDS(df,file=paste0(dir_data_release,"/nass/nassSurveyMYAprice.rds"))
#----------------------------------------------------
# State rental rates                              ####        
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_state_rental_rates()
df[, data_source := "Output from get_state_rental_rates() function"]
saveRDS(df,file=paste0(dir_data_release,"/nass/nassSurveyRentalRates.rds"))
#----------------------------------------------------
# Census - Insurance                              ####  
rm(list= ls()[!(ls() %in% c(Keep.List))])
data_census <- data.table::rbindlist(
  lapply(
    list.files(paste0(dir_fastscratch,"/nass"),pattern = "qs.census",full.names = T), 
    function(census_file){ 
      tryCatch({ 
        
        # census_file  <- list.files(paste0(dir_fastscratch,"/nass"),pattern = "qs.census",full.names = T)[5]
        
        data <-  data.table::fread(census_file)
        names(data) <- tolower(names(data))
        
        data <- data[short_desc %in% c(
          "AG LAND, CROPLAND - ACRES",
          "AG LAND, CROP INSURANCE - ACRES",
          "AG LAND, CROPLAND - NUMBER OF OPERATIONS",
          "AG LAND, CROP INSURANCE - NUMBER OF OPERATIONS",
          
          "INCOME, FARM-RELATED, AG TOURISM & RECREATIONAL SERVICES - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED, AG SERVICES, CUSTOMWORK & OTHER - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED, RENT, LAND & BUILDINGS - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED, OTHER - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED, GOVT PROGRAMS, STATE & LOCAL - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED, PATRONAGE DIVIDENDS & REFUNDS FROM COOPERATIVES - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED, CROP & ANIMAL INSURANCE PAYMENTS - RECEIPTS, MEASURED IN $",
          "INCOME, FARM-RELATED, FOREST PRODUCTS, (EXCL CHRISTMAS TREES & SHORT TERM WOODY CROPS & MAPLE SYRUP) - RECEIPTS, MEASURED IN $",
          
          "INCOME, FARM-RELATED, CROP & ANIMAL INSURANCE PAYMENTS - RECEIPTS, MEASURED IN $ / OPERATION",
          "INCOME, FARM-RELATED, CROP & ANIMAL INSURANCE PAYMENTS - OPERATIONS WITH RECEIPTS") ,]
        
        data[, value := as.numeric(as.character(gsub(",","",value)))]
        data <- data[!value %in% c(NA,Inf,-Inf,NaN)]
        data[,state_code  := as.numeric(as.character(data$state_fips_code))]
        data[,asd_code    := as.numeric(as.character(data$asd_code))]
        data[,county_code := as.numeric(as.character(data$county_code))]
        data <- data[toupper(domain_desc) %in% unique(data[grepl("INSURANCE",toupper(short_desc))]$domain_desc)]
        data <- data[, .(value = sum(value, na.rm = TRUE)),
                     by = c("year","agg_level_desc","domain_desc","domaincat_desc","short_desc",
                            "state_code","asd_code","county_code")]
        gc()
        
        return(data)
      }, error = function(e){return(NULL)})
    }),fill = T)
data_census[, data_source := "USDA NASS Quick Stats"]
saveRDS(data_census,file=file.path(dir_data_release,"nass/agCensusInsurance.rds")) 
#----------------------------------------------------
# Aggregate NASS Production Data                  ####       
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- get_nass_production_data(
  dir_source      = paste0(dir_fastscratch,"/nass"),
  source_desc     = "SURVEY",
  agg_level_desc  = c("NATIONAL","STATE","COUNTY"))
df[, data_source := "USDA NASS Quick Stats"]
saveRDS(df,file=paste0(dir_data_release,"/nass/nassSurveyCropProductionData.rds"))
#----------------------------------------------------
# Upload the assets                               ####

# Verify auth first (nice sanity check)
if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# Official Pasture, Rangeland, Forage Pilot Insurance Program Data
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "nass_extracts",
  name = "USDA NASS Data - Extracts",
  body = "Various items aggregated from USDA NASS"
)

piggyback::pb_upload(
  list.files(paste0(dir_data_release,"/nass"), full.names = TRUE, recursive = TRUE),
  repo  = "ftsiboe/USFarmSafetyNetLab",
  tag   = "nass_extracts",
  overwrite = TRUE
)
#----------------------------------------------------
