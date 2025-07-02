
#' Download and cache USDA NASS Quick Stats large dataset files
#'
#' @description
#' `get_nass_large_datasets()` retrieves a Quick Stats file from the USDA National Agricultural Statistics Service (NASS)
#' https://www.nass.usda.gov/datasets/ page and saves it locally.  If the file is already present in the target directory, it is not re-downloaded.
#'
#' @param large_dataset `character(1)`
#'   The base name of the Quick Stats file to download.  For example, use `"crops"` to fetch
#'   `qs.crops_YYYYMMDD.txt.gz` or include `"census2022"` (e.g. `"census2022"`) to fetch the gzipped 2022 census version
#'   (`qs.census2022.txt.gz`). one of:
#'   "census2002","census2007","census2012","census2017","census2022",
#'   "census2007zipcode","census2017zipcode",
#'   "animals_products","crops","demographics","economics","environmental"
#' @param dir_nass_qs `character(1)`
#'   Path to a directory where downloaded files will be stored.  Defaults to `"./data-raw/nass_qs"`.
#'
#' @return
#' Invisibly returns the normalized file large_dataset (e.g. `"qs.crops_YYYYMMDD.txt.gz"` or `"qs.censusYYYY.txt.gz"`) that was
#' downloaded or already present.
#'
#' @details
#' 1. Prepends `"qs."` to the provided `large_dataset`.  If `large_dataset` contains `"census"`, appends `".txt.gz"`,
#'    otherwise `NULL`.
#' 2. Ensures `dir_nass_qs` exists (creates it if needed).
#' 3. Scrapes the NASS datasets page (`https://www.nass.usda.gov/datasets/`) for links ending in `.txt.gz`.
#' 4. Downloads the matching file into `dir_nass_qs` if not already present.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom utils download.file
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 'crops' dataset if not already cached:
#' get_nass_large_datasets(large_dataset = "crops")
#'
#' # Download the 2022 census version:
#' get_nass_large_datasets(large_dataset = "census2022", dir_nass_qs = "data-raw/fastscratch/nass_qs")
#' }
get_nass_large_datasets <- function(large_dataset, dir_nass_qs = "./data-raw/fastscratch/nass_qs"){
  # Normalize the file large_dataset
  if (grepl("census", large_dataset)) {
    file_name <- paste0("qs.", large_dataset,".txt.gz")
  } else {
    file_name <- paste0("qs.", large_dataset)
  }
  
  # Create target directory if needed
  if (!dir.exists(dir_nass_qs)) {
    dir.create(dir_nass_qs, recursive = TRUE)
  }
  
  # Scrape available dataset URLs
  base_url <- "https://www.nass.usda.gov"
  dataset_page <- xml2::read_html(paste0(base_url, "/datasets/"))
  hrefs <- dataset_page %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  txt_links <- hrefs[grepl("\\.txt", hrefs)]
  qs_urls <- txt_links[grepl("datasets", txt_links)]
  
  # Identify the specific URL for this dataset
  matched <- qs_urls[grepl(file_name, qs_urls)]
  dest_file <- file.path(dir_nass_qs, gsub("^/datasets/", "", matched))
  
  # Download if not already present
  if (!basename(dest_file) %in% list.files(dir_nass_qs, pattern = file_name)) {
    
    if(!grepl("census",file_name)){
      unlink(list.files(dir_nass_qs, pattern = file_name,full.names = TRUE))
    }
    
    download.file(
      url      = paste0(base_url, matched),
      destfile = dest_file,
      mode     = "wb"
    )
  }
  
  invisible(file_name)
}


#' Process a USDA NASS Quick Stats dataset by sector and statistic category
#'
#' @description
#' `process_nass_dataset()` downloads (if needed) and reads one or more NASS Quick Stats large datasets
#' “.txt.gz” files for a given sector, filters the rows by the chosen statistic category plus
#' any additional Quick Stats API parameters, converts and cleans the `value` column,
#' aggregates it by taking its mean over all remaining grouping columns, and then renames
#' that aggregated column to match the requested statistic.
#'
#' @param dir_nass_qs       `character(1)`
#'   **Length 1.** Path to the directory where Quick Stats large datasets files are stored (and will be
#'   downloaded to via `get_nass_large_datasets()`).  Defaults to `"./data-raw/nass_qs"`.
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
#' * All original columns have been lowercased (and `cv_%` → `cv`).
#' * Rows have been filtered by `nassqs_params`.
#' * A `value` column has been converted to numeric (commas stripped), cleaned
#'   of non‐finite entries, and then aggregated by mean over the remaining columns.
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
    dir_nass_qs = "./data-raw/nass_qs",
    large_dataset,
    statisticcat_desc = NULL,
    nassqs_params     = NULL){
  # ensure cache directory exists
  if (!dir.exists(dir_nass_qs)) {
    dir.create(dir_nass_qs, recursive = TRUE)
  }
  
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
  
  # Download any missing files
  get_nass_large_datasets(large_dataset = large_dataset, dir_nass_qs = dir_nass_qs)
  
  # Read & lowercase
  files <- list.files(dir_nass_qs, pattern = large_dataset, full.names = TRUE)
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
  df[grepl("PEAS",         commodity_name), commodity_name := "Dry Peas"]
  df[grepl("SUGARBEETS",   commodity_name), commodity_name := "Sugar Beets"]
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
  grouping <- setdiff(names(df), c("value","cv","state_fips_code","county_code","year","commodity_desc"))
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
#' and returns a tidy table of marketing‐year average prices.
#'
#' @param dir_nass_qs        `character(1)`
#'   Path to the directory where Quick Stats files are stored.
#'   Defaults to `"./data-raw/nass_qs"`.
#' @param agg_level_desc    `character`
#'   One or more values for the `agg_level_desc` field in the Quick Stats data.
#'   Can be `"STATE"` or/and `"NATIONAL"`.  Defaults to `"NATIONAL"`.
#' @param short_desc        `character`
#'   One or more Quick Stats `short_desc` strings identifying the crop–price series to retrieve.
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
    dir_nass_qs = "./data-raw/fastscratch/nass_qs",
    agg_level_desc = c("NATIONAL","STATE","COUNTY"),
    short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU") {
  
  ## Fetch and filter raw data
  df <- process_nass_dataset(
    dir_nass_qs = dir_nass_qs,
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
  
  # Drop rows with non‐finite prices
  df <- df[is.finite(df[["marketing_year_avg_price"]])]
  
  # Keep only rows with finite commodity_code and the four columns you need
  df <- df[
    is.finite(df[["commodity_code"]]),
    c("commodity_year","commodity_code","state_code","marketing_year_avg_price")]
  
  # Unit conversions
  df[commodity_code %in%  c(15,18,49,47,67,78),
     marketing_year_avg_price := marketing_year_avg_price / 100]
  
  # sorghum cwt → bu (code 51)
  df[commodity_code == 51,
     marketing_year_avg_price := (marketing_year_avg_price / 100) * 56]
  
  # sugar ton → lb (codes 38, 39)
  df[commodity_code %in% c(38,39),
     marketing_year_avg_price := marketing_year_avg_price / 2000]
  
  ## Tag and return
  df[, data_source := "USDA NASS Quick Stats"]
  return(df)
}
