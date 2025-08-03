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
#' @import xml2 rvest tidyr purrr data.table readr stringr
#' @importFrom stringr str_to_sentence
#' @importFrom dplyr across group_by summarise ungroup
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
  final <- final |> group_by(CROP,crop_year) |>
    summarise(across(all_of(c("Tracks_area_harvested", "Tracks_area_planted", "Tracks_price", "Tracks_production","Tracks_yield")),
                     ~ sum(., na.rm = TRUE), .names = "{.col}")) |>
    ungroup() |> as.data.frame(.)
  
  final <- final[c("CROP","crop_year","Tracks_area_harvested", "Tracks_area_planted", "Tracks_price", "Tracks_production","Tracks_yield")]
  names(final) <- c("commodity_name","commodity_year","tracks_area_harvested", "tracks_area_planted", "tracks_price", "tracks_production","tracks_yield")
  final$commodity_name <- stringr::str_to_sentence(final$commodity_name)
  return(as.data.table(final))
}


#' Download and cache USDA NASS Quick Stats large dataset files
#'
#' @description
#' `downloaded_nass_large_datasets()` retrieves a Quick Stats file from the USDA National Agricultural Statistics Service (NASS)
#' https://www.nass.usda.gov/datasets/ page and saves it locally.  If the file is already present in the target directory, it is not re-downloaded.
#'
#' @param large_datasets `character list`
#'   The base name of the Quick Stats file to download.  For example, use `"crops"` to fetch
#'   `qs.crops_YYYYMMDD.txt.gz` or include `"census2022"` (e.g. `"census2022"`) to fetch the gzipped 2022 census version
#'   (`qs.census2022.txt.gz`). any of:
#'   "census2002","census2007","census2012","census2017","census2022",
#'   "census2007zipcode","census2017zipcode",
#'   "animals_products","crops","demographics","economics","environmental"
#' @param dir_dest `character(1)`
#'   Path to a directory where downloaded files will be stored.  Defaults to `"./data-raw/fastscratch/nass/"`.
#'
#' @return
#' Invisibly returns the normalized file large_dataset (e.g. `"qs.crops_YYYYMMDD.txt.gz"` or `"qs.censusYYYY.txt.gz"`) that was
#' downloaded or already present.
#'
#' @details
#' 1. Prepends `"qs."` to the provided `large_dataset`.  If `large_dataset` contains `"census"`, appends `".txt.gz"`,
#'    otherwise `NULL`.
#' 2. Ensures `dir_dest` exists (creates it if needed).
#' 3. Scrapes the NASS datasets page (`https://www.nass.usda.gov/datasets/`) for links ending in `.txt.gz`.
#' 4. Downloads the matching file into `dir_dest` if not already present.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom utils download.file
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 'crops' dataset if not already cached:
#' downloaded_nass_large_datasets(large_dataset = "crops")
#'
#' # Download the 2022 census version:
#' downloaded_nass_large_datasets(large_dataset = "census2022", 
#' dir_dest = "./data-raw/fastscratch/nass/")
#' }
downloaded_nass_large_datasets <- function(large_datasets, dir_dest = "./data-raw/fastscratch/nass/"){
  
  # Create target directory if needed
  if (!dir.exists(dir_dest)) {
    dir.create(dir_dest, recursive = TRUE)
  }
  
  lapply(
    large_datasets,
    function(large_dataset){
      tryCatch({
        # Normalize the file large_dataset
        if (grepl("census", large_dataset)) {
          file_name <- paste0("qs.", large_dataset,".txt.gz")
        } else {
          file_name <- paste0("qs.", large_dataset)
        }
        
        # Scrape available dataset URLs
        base_url <- "https://www.nass.usda.gov"
        dataset_page <- xml2::read_html(paste0(base_url, "/datasets/"))
        hrefs <- dataset_page |>
          rvest::html_nodes("a") |>
          rvest::html_attr("href")
        txt_links <- hrefs[grepl("\\.txt", hrefs)]
        qs_urls <- txt_links[grepl("datasets", txt_links)]
        
        # Identify the specific URL for this dataset
        matched <- qs_urls[grepl(file_name, qs_urls)]
        dest_file <- file.path(dir_dest, gsub("^/datasets/", "", matched))
        
        # Download if not already present
        if (!basename(dest_file) %in% list.files(dir_dest, pattern = file_name)) {
          
          if(!grepl("census",file_name)){
            unlink(list.files(dir_dest, pattern = file_name,full.names = TRUE))
          }
          
          download.file(
            url      = paste0(base_url, matched),
            destfile = dest_file,
            mode     = "wb"
          )
        }
      }, error=function(e){})
      return(large_dataset)})

  return(list.files(dir_dest))
}


#' Process a USDA NASS Quick Stats dataset by sector and statistic category
#'
#' @description
#' `process_nass_dataset()` downloads (if needed) and reads one or more NASS Quick Stats large datasets
#'  files for a given sector, filters the rows by the chosen statistic category plus
#' any additional Quick Stats API parameters, converts and cleans the `value` column,
#' aggregates it by taking its mean over all remaining grouping columns, and then renames
#' that aggregated column to match the requested statistic.
#'
#' @param dir_source       `character(1)`
#'   **Length 1.** Path to the directory where Quick Stats large datasets files are stored (and will be
#'   downloaded to via `get_nass_large_datasets()`).  Defaults to `"./data-raw/fastscratch/nass/"`.
#' @param large_dataset       `character(1)`
#'   The Quick Stats `large_dataset` to load (e.g. `"crops"`). one of:
#'   "census2002","census2007","census2012","census2017","census2022",
#'   "census2007zipcode","census2017zipcode",
#'   "animals_products","crops","demographics","economics","environmental"
#' @param statisticcat_desc `character(1)`
#'   **Length 1.** The Quick Stats `statisticcat_desc` to filter on (e.g. `"PRICE RECEIVED"`).
#'   After aggregation, the resulting column of mean values will be renamed to
#'   `gsub(" ", "_", statisticcat_desc)`.
#' @param nassqs_params     `list` or `NULL`
#'   A named list of additional Quick Stats API parameters to filter by (e.g.
#'   `"domain_desc"`, `"agg_level_desc"`, `"year"`, etc.).  Names must correspond to
#'   valid Quick Stats fields.  If `NULL` (the default), only `sector_desc` +
#'   `statisticcat_desc` filtering is applied.  Use
#'   `rnassqs::nassqs_params()` to list all valid parameter names.
#'
#' @return A `data.table` where:
#' * All original columns have been lowercased.
#' * Rows have been filtered by `nassqs_params`.
#' * A `value` column has been converted to numeric (commas stripped), cleaned
#'   of non-finite entries, and then aggregated by mean over the remaining columns.
#' * That aggregated column is renamed to `gsub(" ", "_", statisticcat_desc)`.
#' * Numeric code columns `state_code`, `country_code`, `asd_code`, plus
#'   `commodity_year` and `commodity_name` have been created.
#'
#' @details
#' The full set of valid Quick Stats API parameter names can be retrieved with:
#' ```r
#' rnassqs::nassqs_params()
#' ```
#' @seealso
#' * `get_nass_large_datasets()` for downloading the raw Quick Stats files
#'
#' @importFrom data.table fread setDT setnames
#' @importFrom stringr str_to_title
#' @importFrom rfcip get_crop_codes
#' @importFrom dplyr full_join
#' @export
#'
#' @examples
#' \dontrun{
#' # National annual average price received for all CROPS in 2020:
#' dt1 <- process_nass_dataset(
#'   large_dataset       = "crops",
#'   statisticcat_desc = "PRICE RECEIVED",
#'   nassqs_params = list( agg_level_desc = "NATIONAL", year = 2020 ))
#'
#' # State-level marketing-year average price for soybeans:
#' dt2 <- process_nass_dataset(
#'   large_dataset       = "crops",
#'   statisticcat_desc = "PRICE RECEIVED",
#'   nassqs_params     = list(
#'     agg_level_desc      = "STATE",
#'     short_desc          = "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
#'     reference_period_desc = "MARKETING YEAR",
#'     freq_desc           = "ANNUAL"
#'   )
#' )
#' }
process_nass_dataset <- function(
    dir_source = "./data-raw/fastscratch/nass/",
    large_dataset,
    statisticcat_desc = NULL,
    nassqs_params     = NULL){

  # validate large_dataset length
  if (length(large_dataset) != 1) {
    stop("`large_dataset` must be length 1.")
  }
  
  # validate large_dataset value
  valid_datasets <- c(
    "census2002","census2007","census2012","census2017","census2022",
    "census2007zipcode","census2017zipcode",
    "animals_products","crops","demographics","economics","environmental"
  )
  if (!large_dataset %in% valid_datasets) {
    stop(
      "`large_dataset` must be one of: ",
      paste(valid_datasets, collapse = ", ")
    )
  }
  
  #validate statisticcat_desc length (if provided)
  if (!is.null(statisticcat_desc) && length(statisticcat_desc) != 1) {
    stop("`statisticcat_desc` must be length 1 if not NULL.")
  }

  # Read & lowercase
  files <- list.files(dir_source, pattern = large_dataset, full.names = TRUE)
  df <- data.table::fread(files)
  data.table::setDT(df)
  data.table::setnames(df, old = names(df),    new = tolower(names(df)))
  data.table::setnames(df, old = "cv_%",       new = "cv")
  
  # Prepare filters
  nassqs_params <- Filter(Negate(is.null), nassqs_params)
  if (!is.null(statisticcat_desc)) {
    # ensure we filter on the requested statistic category
    nassqs_params$statisticcat_desc <- statisticcat_desc
  }

  # Apply filters
  if (!is.null(nassqs_params) && length(nassqs_params) > 0) {
    for (col in names(nassqs_params)) {
      df <- df[get(col) %in% nassqs_params[[col]]]
    }
  }
  
  # Clean & convert value
  df[, value := as.numeric(gsub(",", "", as.character(value)))]
  df <- df[is.finite(value)]
  
  # Create code & descriptor columns
  df[, state_code     := as.numeric(state_fips_code)]
  df[, county_code    := as.numeric(county_ansi)]
  df[, asd_code       := as.numeric(asd_code)]
  df[, commodity_year := as.numeric(year)]
  df[, commodity_name := commodity_desc]
  
  ## Normalize commodity names and Join RMA commodity codes
  df[grepl("SORGHUM", commodity_name) & grepl("SILAGE", util_practice_desc) , commodity_name := "SILAGE SORGHUM"]
  df[grepl("SORGHUM", commodity_name) & !grepl("SILAGE", util_practice_desc), commodity_name := "GRAIN SORGHUM"]
  df[grepl("BEANS",        commodity_name), commodity_name := "Dry Beans"]
  df[grepl("FLAXSEED",     commodity_name), commodity_name := "Flax"]
  df[grepl("SUGARBEETS",   commodity_name), commodity_name := "Sugar Beets"]
  df[grepl("PAPAYAS", commodity_name), commodity_name := "PAPAYA"]
  df[grepl("SUNFLOWER", commodity_name), commodity_name := "SUNFLOWERS"]
  df[grepl("BANANAS", commodity_name), commodity_name := "BANANA"]
  df[commodity_name %in% "TANGERINES", commodity_name := "MANDARINS/TANGERINES"]
  df[commodity_name %in% c("RASPBERRIES","BLACKBERRIES"),commodity_name := "RASPBERRY AND BLACKBERRY"]
  df[commodity_name %in% c("FRESH PLUM","PLUMS"),commodity_name := "PLUM"]
  df[commodity_name %in% c("CHICKPEAS","LENTILS","PEAS"),commodity_name := "DRY PEAS"]
  df[commodity_name %in% c("LEMONS","LIMES"),commodity_name := "LIME/LEMON"]
  df[commodity_name %in% "WILD RICE",commodity_name := "RICE"]
  # df[grepl("HAY", commodity_name), commodity_name := "FORAGE PRODUCTION"]
  
  for(xx in c("MACADAMIA","PECAN","GRAPEFRUIT","APPLE","ORANGE","PAPAYA",
              "BANANA","TANGELO","AVOCADO","COFFEE","APRICOT","NECTARINE",
              "CARAMBOLA","PEACHES","TOMATOES","MANGO","FORAGE","SWEET CORN")) {
    df[grepl(xx, commodity_name), commodity_name := xx]
  }
  
  df <- as.data.frame(df)
  df$commodity_name <- stringr::str_to_title(df$commodity_name)
  crop_codes <- as.data.frame(
    rfcip::get_crop_codes(crop = unique(df$commodity_name))
  )
  crop_codes$commodity_year <- as.integer(crop_codes$commodity_year)
  crop_codes$commodity_code <- as.integer(crop_codes$commodity_code)
  df <- dplyr::full_join(as.data.frame(df), crop_codes[c("commodity_code","commodity_name")],
                         by = "commodity_name")
  
  data.table::setDT(df)
  
  # Aggregate by all other columns
  grouping <- setdiff(names(df), c("value","cv","state_fips_code","county_ansi","year","commodity_desc"))
  df <- df[, .(value = mean(value, na.rm = TRUE)), by = grouping]
  df <- df[is.finite(value)]
  
  # Rename aggregated column if requested
  if (!is.null(statisticcat_desc)) {
    new_name <- tolower(gsub(" ", "_", statisticcat_desc))
    data.table::setnames(df, "value", new_name)
  }
  
  return(df)
}


#' Get Marketing Year Average Price for a Single Crop from USDA NASS Quick Stats
#'
#' @description
#' `get_marketing_year_avg_price()` fetches USDA NASS Quick Stats data for a specified crop
#' (`short_desc`) at one or more aggregation levels (`agg_level_desc`), computes the mean price
#' for the marketing year, joins to official RMA commodity codes, applies necessary unit conversions,
#' and returns a tidy table of marketing-year average prices.
#'
#' @param dir_source        `character(1)`
#'   Path to the directory where Quick Stats files are stored.
#'   Defaults to `"./data-raw/fastscratch/nass/"`.
#' @param agg_level_desc    `character`
#'   One or more values for the `agg_level_desc` field in the Quick Stats data.
#'   Can be `"STATE"` or/and `"NATIONAL"`.  Defaults to `"NATIONAL"`.
#' @param short_desc        `character`
#'   One or more Quick Stats `short_desc` strings identifying the crop-price series to retrieve.
#'   Defaults to `"CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU"`.
#'   **Currently, only the following set is supported:**
#'   c(
#'     "OATS - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "RYE - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "TOBACCO - PRICE RECEIVED, MEASURED IN $ / LB",
#'     "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "FLAXSEED - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "BARLEY - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "BEANS, DRY EDIBLE, INCL CHICKPEAS - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "HAY - PRICE RECEIVED, MEASURED IN $ / TON",
#'     "WHEAT - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "COTTON - PRICE RECEIVED, MEASURED IN $ / LB",
#'     "SORGHUM, GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "SUGARBEETS - PRICE RECEIVED, MEASURED IN $ / TON",
#'     "PEAS, DRY EDIBLE - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "SUNFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "RICE - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "PEANUTS - PRICE RECEIVED, MEASURED IN $ / LB",
#'     "CANOLA - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "MAPLE SYRUP - PRICE RECEIVED, MEASURED IN $ / GALLON",
#'     "RICE, LONG GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "MILLET, PROSO - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "SUGARCANE - PRICE RECEIVED, MEASURED IN $ / TON",
#'     "SAFFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT"
#'   )
#' @return A `data.table` with columns:
#'   - `commodity_year` (`integer`): the marketing year
#'   - `commodity_code` (`integer`): NASS commodity code
#'   - `state_code` (`integer`): state FIPS code (if `agg_level_desc` includes `"STATE"`)
#'   - `marketing_year_avg_price` (`numeric`): average price for the marketing year, in dollars per unit
#'   - `data_source` (`character`): always `"USDA NASS Quick Stats"`
#'
#' @seealso
#' * `process_nass_dataset()` for the underlying data fetch and filtering
#' * `get_nass_large_datasets()` for downloading the raw Quick Stats files
#'
#' @importFrom data.table setDT
#' @importFrom stringr str_to_title
#' @importFrom rfcip get_crop_codes
#' @importFrom dplyr full_join
#' @importFrom stats complete.cases
#' @export
#'
#' @examples
#' \dontrun{
#' # Default: national average for corn
#' get_marketing_year_avg_price()
#'
#' # Both state and national for wheat
#' get_marketing_year_avg_price(
#'   agg_level_desc = c("STATE", "NATIONAL"),
#'   short_desc     = "WHEAT - PRICE RECEIVED, MEASURED IN $ / BU"
#' )
#' }
get_marketing_year_avg_price <- function(
    dir_source = "./data-raw/fastscratch/nass/",
    agg_level_desc = c("NATIONAL","STATE","COUNTY"),
    short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU") {
  
  ## Fetch and filter raw data
  df <- process_nass_dataset(
    dir_source = dir_source,
    large_dataset = "crops",
    statisticcat_desc = "PRICE RECEIVED",
    nassqs_params =
      list( source_desc = "SURVEY",
            sector_desc = "CROPS",
            domain_desc = "TOTAL",
            agg_level_desc = agg_level_desc,
            reference_period_desc = "MARKETING YEAR",
            freq_desc = "ANNUAL",
            short_desc = short_desc))
  gc()
  
  ## Compute means
  STATE_df    <- df[get("agg_level_desc") == "STATE",
                    .(STATE_Mya = mean(price_received, na.rm = TRUE)),
                    by = c("commodity_code","commodity_name","state_code", "commodity_year")]
  
  NATIONAL_df <- df[get("agg_level_desc") == "NATIONAL",
                    .(NATIONAL_Mya = mean(price_received, na.rm = TRUE)),
                    by = c("commodity_code","commodity_name", "commodity_year")]
  
  ## Merge and filter
  df <- merge(NATIONAL_df, STATE_df,
              by = c("commodity_code","commodity_name", "commodity_year"),
              all = TRUE)
  df <- df[!is.na(df[["state_code"]])]; gc()
  
  ## Clean types & compute final price
  df[["marketing_year_avg_price"]] <- ifelse(
    !is.finite(df[["STATE_Mya"]]) | df[["STATE_Mya"]] == 0,
    df[["NATIONAL_Mya"]],
    df[["STATE_Mya"]])
  
  df <- df[complete.cases(df)]
  
  # Drop rows with non-finite prices
  df <- df[is.finite(df[["marketing_year_avg_price"]])]
  
  # Keep only rows with finite commodity_code and the four columns you need
  df <- df[
    is.finite(df[["commodity_code"]]),
    c("commodity_year","commodity_code","state_code","marketing_year_avg_price")]
  
  # Unit conversions
  df[commodity_code %in%  c(15,18,49,47,67,78),
     marketing_year_avg_price := marketing_year_avg_price / 100]
  
  # sorghum cwt to bu (code 51)
  df[commodity_code == 51,
     marketing_year_avg_price := (marketing_year_avg_price / 100) * 56]
  
  # sugar ton to lb (codes 38, 39)
  df[commodity_code %in% c(38,39),
     marketing_year_avg_price := marketing_year_avg_price / 2000]
  
  ## Tag and return
  df[, data_source := "USDA NASS Quick Stats"]
  return(df)
}


#' Get State Rental Rates for Cropland
#'
#' @param dir_source `character(1)`
#'   Path to the directory where Quick Stats files are stored.
#'   Defaults to `"./data-raw/fastscratch/nass/"`.
#'   
#' Approximate per-acre cost of crop production using state-level rental rates
#' retrieved from USDA NASS Quick Stats.  This function:
#' \enumerate{
#'   \item Loads and aggregates NASS asset values and cash rents by state and year.
#'   \item Joins the two series, excludes non-contiguous states/territories.
#'   \item Estimates missing rents via a panel regression on log asset values, then corrects for systematic bias.
#'   \item Interpolates any remaining missing values using a 5-nearest-neighbor spatial average, iterated twice.
#' }
#'
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{\code{NAME}}{State name}
#'     \item{\code{state_code}}{Numeric state FIPS code}
#'     \item{\code{commodity_year}}{Year of the observation}
#'     \item{\code{rent}}{Adjusted per-acre rent ($/acre)}
#'   }
#'
#' @details
#' Internally this function relies on:
#' \itemize{
#'   \item \code{process_nass_dataset()} to pull NASS Quick Stats.
#'   \item \code{plm::pdata.frame()} for panel data setup.
#'   \item \code{lm()} to fit a state-fixed-effects trend model.
#'   \item \code{spdep} to compute spatial lags (5-nearest neighbors).
#'   \item \code{doBy::summaryBy()} for error-ratio corrections.
#'   \item \code{terra} and \code{tigris} to obtain state geometries.
#' }
#'
#' @importFrom dplyr full_join inner_join
#' @importFrom data.table rbindlist as.data.table
#' @importFrom plm pdata.frame
#' @importFrom doBy summaryBy
#' @importFrom stats as.formula lm predict
#' @importFrom terra vect geom centroids
#' @importFrom tigris states
#' @importFrom sp SpatialPointsDataFrame CRS
#' @importFrom spdep knearneigh knn2nb nb2mat
#' @export
#'
#' @examples
#' \dontrun{
#' # Make sure `process_nass_dataset()` and required packages are loaded
#' df <- get_state_rental_rates()
#' head(df)
#' }
get_state_rental_rates <- function(dir_source = "./data-raw/fastscratch/nass/"){
  
  # Load and process NASS dataset to get average cropland asset value ($/acre)
  land_value <- process_nass_dataset(
    dir_source = dir_source,
    large_dataset = "economics",
    nassqs_params = list(
      short_desc = "AG LAND, CROPLAND - ASSET VALUE, MEASURED IN $ / ACRE",
      agg_level_desc = "STATE",
      freq_desc = "ANNUAL",
      domain_desc = "TOTAL",
      reference_period_desc="YEAR",
      commodity_desc="AG LAND"
    )
  )[
    , .(ag_land = mean(value, na.rm = TRUE)),
    by = c("commodity_year","state_code")
  ]
  
  # Load and process NASS dataset to get average cropland rent expense ($/acre)
  rents <- process_nass_dataset(
    dir_source = dir_source,
    large_dataset = "economics",
    nassqs_params = list(
      short_desc = "RENT, CASH, CROPLAND - EXPENSE, MEASURED IN $ / ACRE",
      agg_level_desc = "STATE",
      freq_desc = "ANNUAL",
      domain_desc = "TOTAL",
      reference_period_desc="YEAR",
      commodity_desc="RENT"
    )
  )[
    , .(land_rent = mean(value, na.rm = TRUE)),
    by = c("commodity_year","state_code")
  ]
  
  # Join asset values and rents
  df <- dplyr::full_join(land_value, rents, by = c("state_code","commodity_year"))
  
  # Exclude non-contiguous states/territories
  dfx <- df[! state_code %in% c(9,15,23,25,33,44,50,98)]
  
  # Prepare for panel regression
  dfx[,year := commodity_year]
  dfx[,state := state_code]
  dfx <- plm::pdata.frame(dfx,index = c("state","year"),drop.index = TRUE,row.names = TRUE)
  
  # Create 4 lags of ag_land
  for(lag in 1:4){dfx[, paste0("LV_",lag)] <- lag(dfx$ag_land, lag)}
  
  # Fit state-fixe--effects trend model on log asset values
  fit.rent <- lm(
    as.formula(
      paste0(
        "log(ag_land) ~ 1 + commodity_year * factor(state_code) + factor(state_code) - commodity_year + ",
        paste0(names(dfx)[grepl("LV_", names(dfx))], collapse = "+")
      )
    ),
    data = dfx
  )
  summary(fit.rent)
  
  # Predict and back-transform
  dfx$land_rent_hat <- exp(predict(fit.rent, dfx))
  
  # Compute error ratios and correct systematic bias
  dfx <- as.data.frame(dfx)
  dfx$error <- dfx$land_rent / dfx$land_rent_hat
  dfx$state_code <- as.numeric(as.character(dfx$state_code))
  dfy <- doBy::summaryBy(error ~ state_code, data = dfx, FUN = mean, na.rm = TRUE)
  dfx <- dplyr::inner_join(
    dfx,
    doBy::summaryBy(error ~ state_code, data = dfx, FUN = mean, na.rm = TRUE),
    by = "state_code"
  )
  dfx$land_rent_hat_adj <- ifelse(
    is.na(dfx$land_rent),
    dfx$land_rent_hat * dfx$error.mean,
    dfx$land_rent
  )
  
  # Clean up and select output columns
  dfx <- as.data.frame(dfx)
  dfx <- lapply(dfx, function(x) { attr(x, "index") <- NULL; x })
  dfx <- data.frame(dfx)
  dfx$state_code     <- as.numeric(as.character(dfx$state_code))
  dfx$commodity_year <- as.numeric(as.character(dfx$commodity_year))
  
  df <- dfx[c("state_code","commodity_year","land_rent_hat_adj")]
  names(df) <- c("state_code","commodity_year","rent")
  
  # Load state geometries and compute centroids
  States <- terra::vect(tigris::states(cb = TRUE))
  States$state_code <- as.numeric(States$STATEFP)
  centroids <- as.data.frame(terra::geom(terra::centroids(States)))[c("x","y")]
  
  # Build spatial neighbor matrix (5-nearest neighbors)
  nbmat <- sp::SpatialPointsDataFrame(
    cbind(centroids$x, centroids$y),
    data = as.data.frame(States),
    proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  )
  nbmat <- spdep::knearneigh(nbmat, k = 5, longlat = TRUE, use_kd_tree = TRUE)
  nbmat <- spdep::knn2nb(nbmat, row.names = States$state_code, sym = FALSE)
  nbmat <- spdep::nb2mat(nbmat, style = "B")
  
  # Two-round spatial interpolation of missing rents
  for(i in 1:2){
    df <- as.data.frame(
      data.table::rbindlist(
        lapply(
          unique(df$commodity_year),
          function(yr) {
            data <- df[df$commodity_year == yr, c("state_code","commodity_year","rent")]
            data <- data[!is.na(data$rent), ]
            data <- dplyr::full_join(as.data.frame(States), data, by = "state_code")[c("NAME","state_code","rent")]
            data$Rent.spat <- data$rent
            data$Rent.spat <- ifelse(data$Rent.spat %in% c(0,NA,Inf,-Inf,NaN), 0, data$Rent.spat)
            data$Rent.spat <- c(t(t(data$Rent.spat) %*% t(nbmat)) / rowSums(nbmat))
            data$Rent.spat <- ifelse(data$Rent.spat %in% c(0,NA,Inf,-Inf,NaN), NA, data$Rent.spat)
            data$commodity_year <- yr
            data
          }
        ),
        fill = TRUE
      )
    )
    df$Error <- df$rent / df$Rent.spat
    df <- dplyr::inner_join(
      df,
      doBy::summaryBy(Error ~ state_code, data = df, FUN = mean, na.rm = TRUE),
      by = "state_code"
    )
    df$rent <- ifelse(
      is.na(df$rent) & !df$Error.mean %in% c(0,NA,NaN,Inf,-Inf),
      df$Rent.spat * df$Error.mean,
      df$rent
    )
    df$rent <- ifelse(df$rent %in% c(0,NA,NaN,Inf,-Inf), df$Rent.spat, df$rent)
    df <- df[c("NAME","state_code","commodity_year","rent")]
  }
  
  df <- as.data.table(df)

  return(df)
}


#' Retrieve and Aggregate NASS Production Data
#'
#' @description
#' This helper function cleans, and aggregates production and area data
#' from the USDA NASS Quick Stats API (via your `process_nass_dataset()` function),
#' for specified geographic aggregation levels (national, state, county).
#' It returns a `data.table` summarizing mean production and area by the chosen levels.
#'
#' @param dir_source Character. Path to the directory where NASS Quick Stats raw QS files  
#'   are stored (default: `"./data-raw/fastscratch/nass/"`).
#' @param source_desc Character. The `source_desc` filter passed to NASS (e.g. `"SURVEY"`).
#' @param agg_level_desc Character vector. Which aggregation levels to include:  
#'   any combination of `"NATIONAL"`, `"STATE"`, and `"COUNTY"`.  
#'   Controls which code columns (`state_code`, `county_code`) are added.
#'
#' @return A `data.table` with one row per combination of:
#'   * `commodity_year`, `commodity_name`,  
#'   * chosen geographic codes (`state_code`, `county_code`),  
#'   * plus any other aggregation keys.  
#'   Columns contain summed mean values for production and area.
#'
#' @details
#' This function begins by constructing the set of grouping keys (`agg_level_list`), always
#' including the year, commodity name, aggregation descriptor, statistic category, and unit,
#' and then conditionally adding state and/or county codes if those levels are requested.
#' It then invokes `process_nass_dataset()` to fetch the raw crop data for the specified
#' source, sector, domain, country, frequency, reference period, statistic categories, and
#' aggregation levels. Once the data are loaded, any invalid or missing values are removed
#' and the mean of the remaining values is computed for each unique combination of metadata
#' columns. Four separate summaries are then generated: (1) overall production/utilization/class
#' totals, (2) breakdown by commodity class, (3) breakdown by utilization practice, and (4)
#' breakdown by production practice. These four summaries are merged back together, and any
#' rows with unwanted units (e.g., containing a dollar sign) or total rows in the commodity
#' name are filtered out. Next, area metrics are processed by selecting the first non-missing
#' sum across the four summaries and averaging it, and production metrics are handled similarly
#' after filtering out invalid unit-commodity combinations and converting cotton bale values
#' to pounds. Finally, the area and production results are bound together and summed across
#' the chosen grouping keys to produce the final `data.table`. 
#'
#' @seealso
#' * \code{\link{downloaded_nass_large_datasets}} - for downloading and caching USDA NASS Quick Stats large dataset files 
#' * \code{\link{process_nass_dataset}} - for retrieving raw NASS Quick Stats data  
#'
#' @import data.table
#' @export
get_nass_production_data <- function(
    dir_source      = "./data-raw/fastscratch/nass/",
    source_desc     = "SURVEY",
    agg_level_desc  = c("NATIONAL","STATE","COUNTY")) {
  ## 1. Build the aggregation key
  agg_level_list = c(
    "commodity_year",
    "commodity_name",
    "agg_level_desc",
    "statisticcat_desc",
    "unit_desc"
  )
  # add state or county codes if requested
  if ("STATE" %in% agg_level_desc) {
    agg_level_list = c(agg_level_list, "state_code")
  }
  if ("COUNTY" %in% agg_level_desc) {
    agg_level_list = c(agg_level_list, "county_code")
  }
  
  ## 2. Pull raw data via your custom function
  df <- process_nass_dataset(
    dir_source      = dir_source,
    large_dataset    = "crops",
    nassqs_params    = list(
      source_desc           = source_desc,
      sector_desc           = "CROPS",
      domain_desc           = "TOTAL",
      country_name          = "UNITED STATES",
      freq_desc             = "ANNUAL",
      reference_period_desc = "YEAR",
      statisticcat_desc     = c("PRODUCTION","AREA PLANTED","AREA HARVESTED","AREA BEARING"),
      agg_level_desc        = agg_level_desc));gc()

  ## 3. Clean values and compute mean by all metadata columns
  df <- df[
    !value %in% c(NA, Inf, -Inf, NaN),
    .(value = mean(value, na.rm = TRUE)),
    by = c(
      names(df)[
        !names(df) %in% c(
          "cv","value","source_desc","sector_desc","domain_desc",
          "domaincat_desc","state_ansi","state_name","asd_desc",
          "county_ansi","county_name","zip_5","watershed_code",
          "region_desc","watershed_desc","congr_district_code",
          "country_name","country_code","location_desc","begin_code",
          "end_code","week_ending","load_time","state_fips_code",
          "asd_code","freq_desc","reference_period_desc"
        )])];gc()
  
  ## 4. Summarize by practice/class/use combinations
  df_all <- df[
    prodn_practice_desc %in% "ALL PRODUCTION PRACTICES" &
      util_practice_desc  %in% "ALL UTILIZATION PRACTICES" &
      class_desc          %in% "ALL CLASSES",
    .(mn = mean(value, na.rm = TRUE), su = sum(value, na.rm = TRUE)),
    by = agg_level_list];gc()
  
  df_class <- df[
    prodn_practice_desc %in% "ALL PRODUCTION PRACTICES" &
      util_practice_desc  %in% "ALL UTILIZATION PRACTICES" &
      !class_desc        %in% "ALL CLASSES",
    .(mn_class = mean(value, na.rm = TRUE), su_class = sum(value, na.rm = TRUE)),
    by = agg_level_list];gc()
  
  df_use <- df[
    prodn_practice_desc %in% "ALL PRODUCTION PRACTICES" &
      !util_practice_desc %in% "ALL UTILIZATION PRACTICES" &
      class_desc         %in% "ALL CLASSES",
    .(mn_use = mean(value, na.rm = TRUE), su_use = sum(value, na.rm = TRUE)),
    by = agg_level_list];gc()
  
  df_practice <- df[
    !prodn_practice_desc %in% "ALL PRODUCTION PRACTICES" &
      util_practice_desc  %in% "ALL UTILIZATION PRACTICES" &
      class_desc          %in% "ALL CLASSES",
    .(mn_practice = mean(value, na.rm = TRUE), su_practice = sum(value, na.rm = TRUE)),
    by = agg_level_list]
  
  ## 5. Merge summaries and filter out unwanted rows
  df <- merge(df_all, df_class,    by = agg_level_list, all = TRUE)
  df <- merge(df,      df_use,      by = agg_level_list, all = TRUE)
  df <- merge(df,      df_practice, by = agg_level_list, all = TRUE)
  df <- df[!grepl("\\$", unit_desc) & !grepl("TOTALS", commodity_name)]
  rm(df_all, df_class, df_use, df_practice);gc()
  
  ## 6. Process area metrics
  data_area <- df[statisticcat_desc %in% c("AREA PLANTED","AREA HARVESTED","AREA BEARING") &unit_desc %in% "ACRES"]
  
  # choose first non-missing sum among su, su_class, su_use, su_practice
  data_area[, area := ifelse(su   %in% c(0,NA,Inf,-Inf,NaN), su_class,     su)]
  data_area[, area := ifelse(area %in% c(0,NA,Inf,-Inf,NaN), su_use,       area)]
  data_area[, area := ifelse(area %in% c(0,NA,Inf,-Inf,NaN), su_practice,  area)]
  data_area <- data_area[!area %in% c(0,NA,Inf,-Inf,NaN)]
  # average area back to a single value per key
  data_area <- data_area[, .(value = mean(area, na.rm = TRUE)),by = agg_level_list]
  data_area[, statisticcat_desc := paste0("nassSurvey_", gsub(" ", "_", statisticcat_desc))]
  
  ## 7. Process production metrics
  data_prod <- df[statisticcat_desc %in% "PRODUCTION"]
  data_prod[, unit_desc      := toupper(unit_desc)]
  data_prod[, commodity_name := toupper(commodity_name)]
  # drop unwanted units/commodities
  for(patt in c("PCT","BASIS","GALLONS","RUNNING BALES")) {
    data_prod <- data_prod[!grepl(patt, unit_desc)]
  }
  # drop specific commodity-unit combos known to be invalid
  invalid_pairs <- list(
    c("COTTON","TONS"), c("ALMONDS","TONS"),    c("APPLES","TONS"),
    c("ASPARAGUS","TONS"),c("BLUEBERRIES","TONS"),c("CABBAGE","TONS"),
    c("CAULIFLOWER","TONS"),c("CARROTS","TONS"), c("CELERY","TONS"),
    c("CHERRIES","TONS"),c("COFFEE","TONS"),    c("CORN","TONS"),
    c("CRANBERRIES","TONS"),c("CUCUMBERS","TONS"),c("GARLIC","TONS"),
    c("GRAPEFRUIT","TONS"),c("KIWIFRUIT","TONS"),c("LEMONS","TONS"),
    c("MACADAMIAS","TONS"),c("ORANGES","TONS"),  c("PAPAYAS","TONS"),
    c("PEACHES","LB"),     c("PECANS","TONS"),  c("PEPPERS","LB"),
    c("PISTACHIOS","TONS"),c("PUMPKINS","TONS"),c("RASPBERRIES","TONS"),
    c("SORGHUM","TONS"),    c("SPINACH","TONS"), c("SQUASH","TONS"),
    c("STRAWBERRIES","TONS"),c("SWEET CORN","CWT"),
    c("TANGELOS","TONS"),  c("TANGERINES","TONS")
  )
  for(pair in invalid_pairs) {
    data_prod <- data_prod[
      !(grepl(pair[1], commodity_name) & grepl(pair[2], unit_desc))
    ]
  }
  # choose first non-missing sum among su, su_class, su_use, su_practice
  data_prod[, production := ifelse(su %in% c(0,NA,Inf,-Inf,NaN), su_class,     su)]
  data_prod[, production := ifelse(production %in% c(0,NA,Inf,-Inf,NaN), su_use,      production)]
  data_prod[, production := ifelse(production %in% c(0,NA,Inf,-Inf,NaN), su_practice, production)]
  
  # convert cotton bales to pounds where applicable
  data_prod[grepl("COTTON", commodity_name) & grepl("480 LB BALES", unit_desc),production := production * 480]
  
  data_prod <- data_prod[!production %in% c(0,NA,Inf,-Inf,NaN)]
  
  # average production back to a single value per key
  data_prod <- data_prod[, .(value = mean(production, na.rm = TRUE)),by = agg_level_list]
  
  data_prod[, statisticcat_desc := paste0("nassSurvey_", gsub(" ", "_", statisticcat_desc))]
  
  ## 8. Combine area and production, sum final values
  df <- rbind(data_prod, data_area)[
    , .(value = sum(value, na.rm = TRUE)),
    by = c(agg_level_list[!agg_level_list %in% c("agg_level_desc","statisticcat_desc","unit_desc")])]
  
  rm(data_prod, data_area);gc()
  
  return(df)
}


#' Prepare USDA NASS Census Data for Release
#'
#' @description
#' Iterates over one or more USDA NASS census years, fetching each via
#' `process_nass_dataset()`, and produces three sets of state-level and county-level summaries for each year:  
#' 1. Agricultural land metrics by state,  
#' 2. Crop insurance totals by state and county,  
#' 3. Broad Farm Registry (BRF) census summaries by state and national level.
#'
#' @param censuses Integer vector. One or more census years to process
#'   (e.g. `c(2022, 2017, 2012, 2007, 2002)`).
#' @param dir_source Character. Path to the directory containing raw NASS Quick Stats
#'   census datasets (default: `"./data-raw/fastscratch/nass/"`).
#' @param dir_dest Character. Directory where the processed RDS files will be saved
#'   (default: `"data-raw/data_release/nass/"`).
#'
#' @details
#' For each year in `censuses`, the function:
#' First, it calls
#' `process_nass_dataset()` with the `large_dataset = paste0("census", census)` argument
#' to load the raw census data into a `data.table`, then renames `commodity_year` to
#' `census_year`. It then performs three blocks of aggregation:
#' 
#' 1. **Agricultural Land by State**  
#'    Filters for ECONOMICS-sector state-level records on cropland, pasture, woodland,
#'    and cropland share, cleans and converts the `value` field to numeric, computes
#'    the mean by `(census_year, state_code, short_desc)`, recodes `short_desc` to
#'    simple labels (`cropland`, `pasture`, `woodland`, `cropland_pct`), pivots wide
#'    with `data.table::dcast()`, coerces `census_year` and `state_code` to numeric,
#'    and saves `nass_census_agLand_state_<census>.rds`.  
#' 
#' 2. **Crop Insurance Summaries**  
#'    Subsets for both cropland and crop-insurance acreage and operation counts plus
#'    several farm-related income receipt categories, strips commas and coerces `value`
#'    to numeric, drops invalid entries, converts location codes to numeric, filters
#'    to insurance-related domains, sums `value` by all relevant grouping fields, and
#'    saves `nass_census_insurance_data_<census>.rds`.  
#' 
#' 3. **BRF Census Aggregates**  
#'    Filters for producer counts and acres (owned vs. rented), converts `value` to
#'    numeric, removes zeros and missing, first computes group-wise means and then sums
#'    across `(census_year, state_code, state_alpha, agg_level_desc, unit_desc,
#'    domaincat_desc)`, recodes `domaincat_desc` to `ALL_` or `BRF_` plus unit,
#'    pivots wide, reorders and renames columns, and saves
#'    `nass_census_brf_<census>.rds`.  
#'
#' @return Character vector of all `nass_census_*.rds` filenames written to `dir_dest`.
#'
#' @seealso
#' * \code{\link{process_nass_dataset}} for loading raw Quick Stats data  
#' 
#' @import data.table
#' @export
get_nass_census_data <- function(
    censuses = c(2022,2017,2012,2007,2002),
    dir_source = "./data-raw/fastscratch/nass/",
    dir_dest = "data-raw/data_release/nass/") {
  
  lapply(
    censuses,
    function(census){
      tryCatch({
        # read and normalize the raw census file
        df <- process_nass_dataset(dir_source = dir_source, large_dataset = paste0("census", census))
        setnames(df,old = c("commodity_year"),new = c("census_year"))
        #---------------------------------------------------
        # AG LAND STATE                                  ####
        data <- df[
          sector_desc   %in% "ECONOMICS" &
            agg_level_desc %in% "STATE"   &
            short_desc    %in% c(
              "AG LAND, CROPLAND - ACRES",
              "AG LAND, CROPLAND - AREA, MEASURED IN PCT OF AG LAND",
              "AG LAND, WOODLAND - ACRES",
              "AG LAND, PASTURELAND - ACRES"
            ) &
            unit_desc %in% c("ACRES", "PCT OF AG LAND") &
            domain_desc %in% "TOTAL",
          .(value = mean(
            as.numeric(gsub(",", "", as.character(value))),
            na.rm = TRUE
          )),
          by = .(census_year, state_code, short_desc)
        ]
        
        # recode and pivot
        data[,
             short_desc := as.character(
               factor(
                 short_desc,
                 levels = c(
                   "AG LAND, CROPLAND - ACRES",
                   "AG LAND, PASTURELAND - ACRES",
                   "AG LAND, WOODLAND - ACRES",
                   "AG LAND, CROPLAND - AREA, MEASURED IN PCT OF AG LAND"
                 ),
                 labels = c("cropland", "pasture", "woodland", "cropland_pct")
               )
             )
        ]
        id_vars <- setdiff(names(data), c("short_desc", "value"))
        data <- data.table::dcast(
          data,
          formula   = as.formula(paste(paste(id_vars, collapse = " + "), "~ short_desc")),
          value.var = "value"
        )
        data[, census_year := as.numeric(as.character(census_year))]
        data[, state_code  := as.numeric(as.character(state_code))]
        saveRDS(data, file = paste0(dir_dest, "/nass_census_agLand_state_",
                                    data[["census_year"]][1], ".rds"))
        rm(data); gc()
        
        #---------------------------------------------------
        # CROP INSURANCE                                 ####
        data <- df[
          toupper(short_desc) %in% c(
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
            "INCOME, FARM-RELATED, CROP & ANIMAL INSURANCE PAYMENTS - OPERATIONS WITH RECEIPTS"
          )
        ]
        data[, value := as.numeric(gsub(",", "", as.character(value)))]
        data <- data[!value %in% c(NA, Inf, -Inf, NaN)]
        data[, state_code := as.numeric(as.character(state_code))]
        data[, asd_cd     := as.numeric(as.character(asd_code))]
        data[, county_code := as.numeric(as.character(county_code))]
        data <- data[
          toupper(domain_desc) %in% unique(
            data[grepl("INSURANCE", toupper(short_desc))]$domain_desc
          )
        ]
        data <- data[,
                     .(value = sum(value, na.rm = TRUE)),
                     by = c(
                       "census_year", "agg_level_desc", "domain_desc", "domaincat_desc",
                       "short_desc", "state_code", "asd_cd", "county_code"
                     )
        ]
        saveRDS(data, file = paste0(dir_dest, "/nass_census_insurance_data_",
                                    data[["census_year"]][1], ".rds"))
        rm(data); gc()
        
        #---------------------------------------------------
        # BRF Census                                     ####
        bfr <- df[
          domain_desc %in% c("PRODUCERS, ON ANY OPERATION", "TOTAL") &
            agg_level_desc %in% c("STATE", "NATIONAL") &
            domaincat_desc %in% c(
              "PRODUCERS, ON ANY OPERATION: (LESS THAN 11 YEARS)",
              "NOT SPECIFIED"
            ) &
            short_desc %in% c(
              "AG LAND, OWNED, IN FARMS - NUMBER OF OPERATIONS",
              "AG LAND, OWNED, IN FARMS - ACRES",
              "AG LAND, RENTED FROM OTHERS, IN FARMS - NUMBER OF OPERATIONS",
              "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES"
            )
        ]
        
        # convert, clean, mean-then-sum, recode, pivot, reorder
        bfr[, value := as.numeric(gsub(",", "", as.character(value)))]
        bfr <- bfr[value != 0 & !is.na(value) & is.finite(value)]
        bfr <- bfr[,
                   .(value = mean(value, na.rm = TRUE)),
                   by = .(
                     census_year, state_code, state_alpha, short_desc,
                     agg_level_desc, unit_desc, domaincat_desc
                   )
        ]
        bfr <- bfr[,
                   .(value = sum(value, na.rm = TRUE)),
                   by = .(
                     census_year, state_code, state_alpha,
                     agg_level_desc, unit_desc, domaincat_desc
                   )
        ]
        bfr[, domaincat_desc := paste0(
          ifelse(domaincat_desc == "NOT SPECIFIED", "ALL", "BRF"),
          "_", unit_desc
        )]
        bfr <- data.table::dcast(
          bfr,
          census_year + state_code + state_alpha ~ domaincat_desc,
          value.var = "value"
        )
        data.table::setcolorder(
          bfr,
          c(
            "census_year", "state_code", "state_alpha",
            "ALL_ACRES", "ALL_OPERATIONS", "BRF_ACRES", "BRF_OPERATIONS"
          )
        )
        saveRDS(bfr, file = paste0(dir_dest, "/nass_census_brf_",
                                   bfr[["census_year"]][1], ".rds"))
        rm(bfr); gc()
        #---------------------------------------------------
        return(invisible(census))
      }, error = function(e){invisible(NULL)})
    })
  return(list.files(dir_dest,pattern = "nass_census_.*\\.rds$"))
}


#' Prepare and Save USDA NASS Data for Release
#'
#' @description
#' Downloads and processes multiple USDA NASS datasets-including census, economics, crops,
#' production, marketing-year average prices, and state rental rates-and saves each as
#' an RDS file in a specified directory.
#'
#' @param dir_dest Character. Target directory for saving processed NASS data files
#'   (default: `"data-raw/data_release/nass/"`).
#' @param dir_source Character. Directory where raw NASS Quick Stats CSVs are stored
#'   or will be downloaded (default: `"./data-raw/fastscratch/nass/"`).
#'
#' @details
#' This function first ensures that `dir_dest` exists, creating it if necessary. It then
#' downloads the raw NASS Quick Stats datasets for the specified census years, economics,
#' and crops into `dir_source` via `downloaded_nass_large_datasets()`. Next, it retrieves
#' the annual Index for Price Received from the economics dataset with
#' `process_nass_dataset()`, normalizes it to the current year, and saves the result as
#' `nass_survey_index_for_price_received.rds`. It then extracts production and area data at the
#' national, state, and county levels using `get_nass_production_data()`, tags the data
#' source, and writes `nass_survey_production_data.rds`. For commodity price averages, it calls
#' `get_marketing_year_avg_price()` for a predefined list of items, labels the source,
#' and saves `nass_survey_marketing_year_avg_price.rds`. Finally, it obtains annual state cropland
#' cash rent and asset-value data with `get_state_rental_rates()`, notes the bias-correction
#' and interpolation methods used, and saves `nass_survey_state_rental_rates.rds`. The function returns
#' a character vector of all filenames in `dir_dest`.
#'
#' @seealso
#' * \code{\link{downloaded_nass_large_datasets}} - download raw NASS Quick Stats CSVs  
#' * \code{\link{process_nass_dataset}} - fetch and reshape raw NASS data  
#' * \code{\link{get_nass_production_data}} - compute production and area metrics  
#' * \code{\link{get_marketing_year_avg_price}} - calculate marketing-year average prices  
#' * \code{\link{get_state_rental_rates}} - assemble state rental rate data  
#'
#' @return Character vector of filenames created in `dir_dest`.
#' @export
prep_nass_data <- function(
    dir_dest = "data-raw/data_release/nass/",
    dir_source = "./data-raw/fastscratch/nass/") {
  
  if (!dir.exists(dir_dest)) {
    dir.create(dir_dest, recursive = TRUE)
  }
  
  if (!dir.exists(dir_source)) {
    dir.create(dir_source, recursive = TRUE)
  }
  
  # Download database
  downloaded_nass_large_datasets(
    large_datasets = c(
      paste0("census", c(2022, 2017, 2012, 2007, 2002)),"economics","crops"),
    dir_dest = dir_source)
  
  # Index for price received
  df <- process_nass_dataset(
    dir_source    = dir_source,
    large_dataset  = "economics",
    nassqs_params  = list(
      short_desc = "COMMODITY TOTALS - INDEX FOR PRICE RECEIVED, 2011",
      freq_desc  = "ANNUAL"))
  
  df <- df[freq_desc %in% "ANNUAL",.(index_for_price_recived = mean(value, na.rm = TRUE)),by = c("commodity_year")]
  
  # df[, index_for_price_recived := index_for_price_recived/df[commodity_year %in% 2023][["index_for_price_recived"]]] 
  
  df[, data_source := "USDA NASS Quick Stats"]
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_index_for_price_received.rds"));rm(df); gc()
  
  # Production data
  df <- get_nass_production_data(dir_source = dir_source,source_desc = "SURVEY",agg_level_desc = c("NATIONAL", "STATE", "COUNTY"))
  df[, data_source := "USDA NASS Quick Stats"]
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_production_data.rds"));rm(df); gc()
  
  # Get Marketing Year Average Price
  df <- get_marketing_year_avg_price(
    dir_source    = dir_source,
    agg_level_desc = c("NATIONAL", "STATE"),
    short_desc     = c(
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
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_marketing_year_avg_price.rds"));rm(df); gc()
  
  # State rental rates
  df <- get_state_rental_rates(dir_source=dir_source)
  df[, data_source :=
       paste0(
         "USDA NASS Quick Stats (annual state cropland cash rent & asset value); ",
         "missing values filled by panel-regression bias correction and 5-nearest-neighbor spatial interpolation"
       )]
  
  saveRDS(df, file = paste0(dir_dest, "/nass_survey_state_rental_rates.rds"));rm(df); gc()
  
  # Census Data
  get_nass_census_data(censuses = as.numeric(gsub("[^0-9]","",list.files(dir_source,pattern = "census"))),
                       dir_source = dir_source,
                       dir_dest = dir_dest)
  
  # Historical Track Record Crop Production
  df <- get_nass_historical_track_record_crop(dir_source = dir_source)
  df[, data_source :="USDA NASS Historical Track Record Crop Production"]
  saveRDS(df, file = paste0(dir_dest, "/nass_historical_track_record_crop.rds"));rm(df); gc()
  
  return(list.files(dir_dest))
}

