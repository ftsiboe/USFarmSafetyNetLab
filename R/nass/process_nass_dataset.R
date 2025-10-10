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
#' @family USDA NASS Quick Stats
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
  
  # Aggregate by all other columns
  data.table::setDT(df)
  grouping <- setdiff(names(df), c("value","cv","state_fips_code","county_ansi","year","commodity_desc"))
  df <- df[, .(value = mean(value, na.rm = TRUE)), by = grouping]
  df <- df[is.finite(value)]
  
  # Rename aggregated column if requested
  if(!is.null(statisticcat_desc)) {
    new_name <- tolower(gsub(" ", "_", statisticcat_desc))
    data.table::setnames(df, "value", new_name)
  }
  
  return(df)
}
