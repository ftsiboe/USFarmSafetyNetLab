#' Download and Process USDA NASS Historical Crop Track Records
#'
#' @description
#' Scrapes the USDA NASS historical track records web page to identify and download
#' the latest `croptrXX.zip` archive, parses its index to locate CSV tables for area
#' planted and harvested by crop, reads and reshapes each table, cleans and standardizes
#' units and variable names, applies special-case unit conversions, and returns a
#' unified time series data frame of crop-level measures (area planted, area harvested,
#' production, yield, price).
#'
#' @param url Character. URL of the USDA NASS historical track record page.
#' @param dir_source Character. Directory into which to download and extract the ZIP file.
#'   Default is `"./data-raw/fastscratch/nass/"`. Must exist or be creatable.
#'
#' @details
#' 1. **Link discovery & download**  
#'    - Reads all `<a>` hrefs from `url`, filters for links containing `"c534fn92g"`,  
#'      `"zip"`, and `"croptr"`.  
#'    - Extracts the year from each filename and selects the link with the maximum year.  
#'    - Downloads that single ZIP as `croptr.zip` into `dir_source`.  
#'
#' 2. **Index parsing**  
#'    - Opens `crop_index.htm` inside the ZIP, extracts the second HTML table as text.  
#'    - Splits on double line breaks, filters to lines mentioning "Area Planted" or  
#'      "Area Harvested", and excludes summary or non-crop entries.  
#'    - Splits each line into `Page`, `File`, `Description`, then further into `Description`  
#'      and a year range (`start`/`end`), and derives the uppercase crop name.  
#'
#' 3. **CSV reading & reshaping**  
#'    - For each row of the index:  
#'      - Reads the CSV inside the ZIP once to locate header (`h`), unit (`u`), and data (`d`) rows.  
#'      - Reads it again skipping to the data start, then loops over each data column to build  
#'        a long table with columns `crop`, `crop_yr`, `variable`, `unit`, and `value`.  
#'    - Combines all tables into one data.frame.  
#'
#' 4. **Cleaning & standardization**  
#'    - Converts `crop_yr` and `value` to numeric, drops zeros and invalids.  
#'    - Scales values when unit indicates thousands, strips formatting characters,  
#'      and normalizes variable names.  
#'    - Consolidates crop subtypes (e.g. grain/silage/grazed) under a single `crop`.  
#'
#' 5. **Variable mapping & special conversions**  
#'    - Identifies key measures via lookup lists (`Area Planted`, `Area Harvested`,  
#'      `Production`, `Yield`, `Price`) and relabels them to `"Tracks_*"`.  
#'    - Applies special-case unit conversions (cents to dollars, bales to lbs, cwt to lbs, tons to lbs,  
#'      etc.), including crop-specific rules (cotton, canola, dry beans, rice, sugarbeet,  
#'      sugarcane, sunflower).  
#'
#' 6. **Final pivot**  
#'    - Pivots the cleaned long table to wide format so each `"Tracks_*"` measure is its  
#'      own column, and returns a data.frame with columns:  
#'      `CROP`, `crop_year`, `Tracks_area_planted`, `Tracks_area_harvested`,  
#'      `Tracks_production`, `Tracks_yield`, `Tracks_price`.  
#'
#' @return A data.table in which each row corresponds to a crop-year, with standardized
#'   track record measures for area planted, area harvested, production, yield, and price.
#'
#' @import xml2 tidyr data.table readr stringr
#' @importFrom stringr str_to_sentence
#' @export
get_nass_historical_track_record_crop <- function(
    url="https://usda.library.cornell.edu/concern/publications/c534fn92g?locale=en", 
    dir_source = "./data-raw/fastscratch/nass/"){
  
  download_link <- xml2::read_html(url)
  download_link <- rvest::html_attr(rvest::html_nodes(download_link, "a"), "href")
  download_link <- download_link[grepl("c534fn92g",download_link)]
  download_link <- download_link[grepl("zip",download_link)]
  download_link <- download_link[grepl("croptr",download_link)]
  download_link <- download_link[as.numeric(gsub("[^0-9]", "", basename(download_link))) %in% max(as.numeric(gsub("[^0-9]", "", basename(download_link))))][1]
  croptr <- basename(download_link)
  download.file(download_link,destfile=paste0(dir_source,"/croptr.zip"),mode="wb")
  
  index <- xml2::read_html(unz(paste0(dir_source,"/croptr.zip"),"crop_index.htm"))  |> 
    rvest::html_node("table:nth-child(2)") |>  rvest::html_text()
  index <- map(index, ~ stringr::str_split(.x, "\\r\n\r\n") |> unlist())
  index <- purrr::reduce(index, c)
  index <- index[(grepl("Area Planted",index) | grepl("Area Harvested",index))]
  index <- index[! grepl("Principal Crops",index)]
  index <- index[! index %in% ""]
  index <- index[!grepl("All tables in the report",index)]
  index <- index[!grepl("Crop Production Historical Track Records",index)]
  index <- index[!grepl("Field Crops",index)]
  index <- index[!grepl("Comparisons",index)]
  index <- data.frame(text=index)
  index <- tidyr::separate(index,"text",into=c("Page","File","Description"),sep="\\r\n")
  index <- tidyr::separate(index,"Description",into=c("Description","period"),sep=":")
  index <- tidyr::separate(index,"period",into=c("start","end"),sep="-")
  index$end <- as.numeric(as.character(gsub("[^0-9]","",index$end)))
  index$start <- as.numeric(as.character(gsub("[^0-9]","",index$start)))
  index$crop <- gsub("Area Planted and Harvested",";",index$Description)
  index$crop <- gsub("Area Harvested",";",index$crop)
  
  index <- tidyr::separate(index,"crop",into=c("crop"),sep=";",remove = F)
  index$crop  <- toupper(as.character(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(index$crop ))), which = c("both"))))
  index
  
  data <- as.data.frame(
    data.table::rbindlist(
      lapply(
        1:nrow(index),
        function(file){
          # file<- 1
          # print(paste0("************************",file))
          data <- as.data.frame(readr::read_csv(unz(paste0(dir_source,"/croptr.zip"),index$File[file])))
          skip <- min(c(1:nrow(data))[data[,2] %in% "h"])
          data <- as.data.frame(readr::read_csv(unz(paste0(dir_source,"/croptr.zip"),index$File[file]), skip = skip))
          h <- data[data[,2] %in% "h",]
          u <- data[data[,2] %in% "u",]
          d <- data[data[,2] %in% "d",]
          data <- as.data.frame(
            data.table::rbindlist(
              lapply(
                4:ncol(data),
                function(xx){
                  # print(xx)
                  return(data.frame(crop = index$crop[file],crop_yr=d[,3],
                                    variable=paste0(h[,xx][! h[,xx] %in% NA],collapse = " "),
                                    unit= paste0(u[,xx][! u[,xx] %in% NA],collapse = " "),
                                    value=d[,xx]))
                }), fill = TRUE))
          return(data)
        }), fill = TRUE))
  
  for(variable in c("crop_yr","value")){
    data[,variable] <- as.numeric(gsub(",","",as.character(data[,variable])))
  }
  data <- data[!data$crop_yr %in% c(0,NA,Inf,-Inf,NaN),]
  data <- data[!data$value %in% c(0,NA,Inf,-Inf,NaN),]
  data$value <- ifelse(grepl("1,000 ",data$unit),data$value*1000,data$value)
  data$unit <- gsub("1,000 ","",as.character(data$unit))
  data$variable <- gsub(" 1[/]","",as.character(data$variable))
  data$variable <- gsub(" 2[/]","",as.character(data$variable))
  data$variable <- gsub(" 3[/]","",as.character(data$variable))
  data$variable <- toupper(as.character(trimws(gsub("\\s+", " ", gsub("[\r\n]", "", as.character(data$variable))), which = c("both"))))
  
  data$crop <- ifelse(data$crop %in% "CORN"    & grepl("GRAZED",data$variable),"CORN - GRAZED",data$crop)
  data$crop <- ifelse(data$crop %in% "CORN"    & grepl("GRAIN",data$variable),"CORN - GRAIN",data$crop)
  data$crop <- ifelse(data$crop %in% "CORN"    & grepl("SILAGE",data$variable),"CORN - SILAGE",data$crop)
  data$crop <- ifelse(data$crop %in% "SORGHUM" & grepl("GRAIN",data$variable),"SORGHUM - GRAIN",data$crop)
  data$crop <- ifelse(data$crop %in% "SORGHUM" & grepl("SILAGE",data$variable),"SORGHUM - SILAGE",data$crop)
  data$crop <- ifelse(data$crop %in% "SORGHUM" & grepl("GRAZED",data$variable),"SORGHUM - GRAZED",data$crop)
  
  list_planted <- c("AREA PLANTED","ACREAGE FOR ALL PURPOSES PLANTED","ACREAGE PLANTED")
  list_harvest <- c("AREA HARVESTED","HARVESTED FOR GRAIN ACRES","ACREAGE FOR ALL PURPOSES HV","HARVESTED FOR SILAGE ACRES","ACREAGE HARVESTED" ,"FORGE GRAZED HOGGED","FORAGE GRAZED HOGGED")
  list_produce <- c("PRODUCTION","HARVESTED FOR GRAIN PRODUCTION","HARVESTED FOR SILAGE PRODUCTION","PRODUCTION IN 480-POUND NWT. BALES")
  list_yield   <- c("YIELD PER ACRE","HARVESTED FOR GRAIN YIELD PER ACRE","HARVESTED FOR SILAGE YIELD PER ACRE")
  list_price   <- c("PRICE PER BUSHEL","PRICE PER CWT","HARVESTED FOR GRAIN PRICE PER BUSHEL","PRICE PER POUND","PRICE PER TON","PRICE PER TONS")
  list_value   <- c("VALUE OF PRODUCTION","HARVESTED FOR GRAIN VALUE OF PRODUCTION")
  list_sales   <- c("SOLD","VALUE OF SALES")
  
  PLANTED <- data[data$variable %in% list_planted,]
  table(PLANTED$crop,PLANTED$variable)
  PLANTED$variable <- "PLANTED"
  
  HARVESTED <- data[data$variable %in% list_harvest,]
  table(HARVESTED$crop,HARVESTED$variable)
  HARVESTED$variable <- "HARVESTED"
  
  PRODUCTION <- data[data$variable %in% list_produce,]
  table(PRODUCTION$crop,PRODUCTION$variable)
  PRODUCTION$variable <- "PRODUCTION"
  
  PRICE <- data[data$variable %in% list_price,]
  table(PRICE$crop,PRICE$variable)
  PRICE$variable <- "PRICE"
  
  VALUE <- data[data$variable %in% list_value,]
  table(VALUE$crop,VALUE$variable)
  VALUE$variable <- "VALUE"
  
  SALES <- data[data$variable %in% list_sales,]
  table(SALES$crop,SALES$variable)
  SALES$variable <- "SALES"
  
  YIELD <- data[data$variable %in% list_yield,]
  table(YIELD$crop,YIELD$variable)
  YIELD$variable <- "YIELD"
  
  final <- rbind(PRICE,PLANTED,HARVESTED,PRODUCTION,VALUE,SALES,YIELD)
  final$value <- ifelse(final$unit %in% c("(cents)"),final$value/100,final$value)
  
  final$value <- ifelse(final$unit %in% c("(cents)"),final$value/100,final$value)
  final$value <- ifelse(final$unit %in% c("(cents)"),final$value/100,final$value)
  final$value <- ifelse(final$unit %in% c("(cents)"),final$value/100,final$value)
  final$value <- ifelse(final$unit %in% c("(cents)"),final$value/100,final$value)
  final$value <- ifelse(final$unit %in% c("(cents)"),final$value/100,final$value)
  
  final$unit <- ifelse(final$unit %in% c("(cents)","(dollars)"),"$",final$unit)
  final$unit <- ifelse(final$unit %in% c("(ac)","(acres)"),"acres",final$unit)
  final$unit <- ifelse(final$unit %in% c("(bu)","(bushels)"),"bu",final$unit)
  final$unit <- ifelse(final$unit %in% c("(t)","(tons)"),"ton",final$unit)
  final$unit <- ifelse(final$unit %in% c("(bales)"),"bales",final$unit)
  final$unit <- ifelse(final$unit %in% c("(pounds)"),"lbs",final$unit)
  final$unit <- ifelse(final$unit %in% c("(cwt)"),"cwt",final$unit)
  
  final$unit <- ifelse(final$variable %in% "PRICE PER POUND",paste0(final$unit,"/lbs"),final$unit)
  final$unit <- ifelse(final$variable %in% "PRICE PER CWT",paste0(final$unit,"/cwt"),final$unit)
  final$unit <- ifelse(final$variable %in% c("PRICE PER TON","PRICE PER TONS"),paste0(final$unit,"/ton"),final$unit)
  final$unit <- ifelse(final$variable %in% c("PRICE PER BUSHEL","HARVESTED FOR GRAIN PRICE PER BUSHEL"),paste0(final$unit,"/bu"),final$unit)
  final$unit <- ifelse(final$variable %in% "YIELD",paste0(final$unit,"/acre"),final$unit)
  
  # convert COTTON PRODUCTION from bales to lbs
  # Note: 1 bale = 480 pounds (ERS Cotton and Wool Outlook: September 2023)
  final$value <- ifelse(final$crop %in% "ALL COTTON" & final$variable %in% "PRODUCTION" & final$unit %in% "bales",final$value*480,final$value)
  final$unit  <- ifelse(final$crop %in% "ALL COTTON" & final$variable %in% "PRODUCTION" & final$unit %in% "bales","lbs",final$unit)
  check <- final[final$crop %in% "ALL COTTON",]
  table(check$variable,check$unit)
  
  # convert CANOLA PRICE from $/cwt to $/lbs
  final$value <- ifelse(final$crop %in% "CANOLA" & final$variable %in% "PRICE" & final$unit %in% "$/cwt",final$value/112,final$value)
  final$unit  <- ifelse(final$crop %in% "CANOLA" & final$variable %in% "PRICE" & final$unit %in% "$/cwt","$/lbs",final$unit)
  check <- final[final$crop %in% "CANOLA",]
  table(check$variable,check$unit)
  
  # convert DRY EDIBLE BEAN PRODUCTION from cwt to lbs
  final$value <- ifelse(final$crop %in% "DRY EDIBLE BEAN" & final$variable %in% "PRODUCTION" & final$unit %in% "cwt",final$value*112,final$value)
  final$unit  <- ifelse(final$crop %in% "DRY EDIBLE BEAN" & final$variable %in% "PRODUCTION" & final$unit %in% "cwt","lbs",final$unit)
  # convert DRY EDIBLE BEAN PRICE from $/cwt to $/lbs
  final$value <- ifelse(final$crop %in% "DRY EDIBLE BEAN" & final$variable %in% "PRICE" & final$unit %in% "$/cwt",final$value/112,final$value)
  final$unit  <- ifelse(final$crop %in% "DRY EDIBLE BEAN" & final$variable %in% "PRICE" & final$unit %in% "$/cwt","$/lbs",final$unit)
  check <- final[final$crop %in% "DRY EDIBLE BEAN",]
  table(check$variable,check$unit)
  
  # convert RICE PRICE from $/cwt to $/lbs
  final$value <- ifelse(final$crop %in% "RICE" & final$variable %in% "PRICE" & final$unit %in% "$/cwt",final$value/112,final$value)
  final$unit  <- ifelse(final$crop %in% "RICE" & final$variable %in% "PRICE" & final$unit %in% "$/cwt","$/lbs",final$unit)
  # convert RICE PRODUCTION from cwt to lbs
  final$value <- ifelse(final$crop %in% "RICE" & final$variable %in% "PRODUCTION" & final$unit %in% "cwt",final$value*112,final$value)
  final$unit  <- ifelse(final$crop %in% "RICE" & final$variable %in% "PRODUCTION" & final$unit %in% "cwt","lbs",final$unit)
  check <- final[final$crop %in% "RICE",]
  table(check$variable,check$unit)
  
  # convert SUGARBEET PRICE from $/ton to $/lbs
  final$value <- ifelse(final$crop %in% "SUGARBEET" & final$variable %in% "PRICE" & final$unit %in% "$/ton",final$value/2000,final$value)
  final$unit  <- ifelse(final$crop %in% "SUGARBEET" & final$variable %in% "PRICE" & final$unit %in% "$/ton","$/lbs",final$unit)
  # convert SUGARBEET PRODUCTION from ton to lbs
  final$value <- ifelse(final$crop %in% "SUGARBEET" & final$variable %in% "PRODUCTION" & final$unit %in% "ton",final$value*2000,final$value)
  final$unit  <- ifelse(final$crop %in% "SUGARBEET" & final$variable %in% "PRODUCTION" & final$unit %in% "ton","lbs",final$unit)
  # convert SUGARBEET YIELD from ton/acre to lbs/acre
  final$value <- ifelse(final$crop %in% "SUGARBEET" & final$variable %in% "YIELD" & final$unit %in% "ton/acre",final$value*2000,final$value)
  final$unit  <- ifelse(final$crop %in% "SUGARBEET" & final$variable %in% "YIELD" & final$unit %in% "ton/acre","lbs/acre",final$unit)
  check <- final[final$crop %in% "SUGARBEET",]
  table(check$variable,check$unit)
  
  # convert SUGARCANE PRICE from $/ton to $/lbs
  final$value <- ifelse(final$crop %in% "SUGARCANE FOR SUGAR AND SEED" & final$variable %in% "PRICE" & final$unit %in% "$/ton",final$value/2000,final$value)
  final$unit  <- ifelse(final$crop %in% "SUGARCANE FOR SUGAR AND SEED" & final$variable %in% "PRICE" & final$unit %in% "$/ton","$/lbs",final$unit)
  # convert SUGARCANE PRODUCTION from ton to lbs
  final$value <- ifelse(final$crop %in% "SUGARCANE FOR SUGAR AND SEED" & final$variable %in% "PRODUCTION" & final$unit %in% "ton",final$value*2000,final$value)
  final$unit  <- ifelse(final$crop %in% "SUGARCANE FOR SUGAR AND SEED" & final$variable %in% "PRODUCTION" & final$unit %in% "ton","lbs",final$unit)
  # convert SUGARCANE PRODUCTION from ton/acre to lbs/acre
  final$value <- ifelse(final$crop %in% "SUGARCANE FOR SUGAR AND SEED" & final$variable %in% "YIELD" & final$unit %in% "ton/acre",final$value*2000,final$value)
  final$unit  <- ifelse(final$crop %in% "SUGARCANE FOR SUGAR AND SEED" & final$variable %in% "YIELD" & final$unit %in% "ton/acre","lbs/acre",final$unit)
  check <- final[final$crop %in% "SUGARCANE FOR SUGAR AND SEED",]
  table(check$variable,check$unit)
  
  # convert SUNFLOWER PRICE from $/cwt to $/lbs
  final$value <- ifelse(final$crop %in% "SUNFLOWER" & final$variable %in% "PRICE" & final$unit %in% "$/cwt",final$value/112,final$value)
  final$unit  <- ifelse(final$crop %in% "SUNFLOWER" & final$variable %in% "PRICE" & final$unit %in% "$/cwt","$/lbs",final$unit)
  check <- final[final$crop %in% "SUNFLOWER",]
  table(check$variable,check$unit)
  
  
  final$crop <- gsub("ALL ","",final$crop)
  names(final)[names(final) %in% "crop_yr"] <- "crop_year"
  final <- final[final$variable %in% c("PLANTED","HARVESTED","PRICE","PRODUCTION","YIELD"),]
  final <- final[c("crop","crop_year", "value","variable")]
  names(final) <- c("CROP","crop_year", "value","variable")
  final$variable <- ifelse(final$variable %in% "PLANTED","Tracks_area_planted",final$variable)
  final$variable <- ifelse(final$variable %in% "HARVESTED","Tracks_area_harvested",final$variable)
  final$variable <- ifelse(final$variable %in% "PRODUCTION","Tracks_production",final$variable)
  final$variable <- ifelse(final$variable %in% "YIELD","Tracks_yield",final$variable)
  final$variable <- ifelse(final$variable %in% "PRICE","Tracks_price",final$variable)
  final <- final |> tidyr::spread(variable, value)
  final$CROP <- ifelse(final$CROP %in% "SUNFLOWER","SUNFLOWERS",final$CROP)
  final$CROP <- ifelse(final$CROP %in% "SUGARBEET","SUGAR BEETS",final$CROP)
  final$CROP <- ifelse(final$CROP %in% "POTATO","POTATOES",final$CROP)
  final$CROP <- ifelse(final$CROP %in% "SOYBEAN","SOYBEANS",final$CROP)
  final$CROP <- ifelse(final$CROP %in% "PEANUT","PEANUTS",final$CROP)
  final$CROP <- ifelse(final$CROP %in% "OAT","OATS",final$CROP)
  final$CROP <- ifelse(final$CROP %in% "FLAXSEED","FLAX",final$CROP)
  final$CROP <- ifelse(final$CROP %in% "DRY EDIBLE BEAN","DRY BEANS",final$CROP)
  final$CROP <- ifelse(final$CROP %in% c("CORN - GRAIN","CORN - SILAGE"),"CORN",final$CROP)
  final$CROP <- ifelse(final$CROP %in% c("SORGHUM - GRAIN","SORGHUM - SILAGE"),"CORN",final$CROP)
  final$CROP <- ifelse(final$CROP %in% c("SUGARCANE FOR SUGAR AND SEED"),"SUGARCANE",final$CROP)
  final$CROP <- ifelse(final$CROP %in% c("SWEET POTATO"),"SWEET POTATOES",final$CROP)
  final <- final[!final$CROP %in% c( "ALFALFA HAY","CORN - GRAZED","DURUM WHEAT","FPOTATO","HAY","OTHER HAY","OTHER SPRING WHEAT",
                                     "SORGHUM - GRAZED","SPRING POTATO","SUMMER POTATO","WINTER POTATO","WINTER WHEAT"),]
  
  final$crop_year <- as.numeric(as.character(final$crop_year))
  final <- final |> dplyr::group_by(CROP,crop_year) |>
    dplyr::summarise(dplyr::across(all_of(c("Tracks_area_harvested", "Tracks_area_planted", "Tracks_price", "Tracks_production","Tracks_yield")),
                                   ~ sum(., na.rm = TRUE), .names = "{.col}")) |>
    dplyr::ungroup() |> as.data.frame(.)
  
  final <- final[c("CROP","crop_year","Tracks_area_harvested", "Tracks_area_planted", "Tracks_price", "Tracks_production","Tracks_yield")]
  names(final) <- c("commodity_name","commodity_year","tracks_area_harvested", "tracks_area_planted", "tracks_price", "tracks_production","tracks_yield")
  final$commodity_name <- stringr::str_to_sentence(final$commodity_name)
  return(as.data.table(final))
}