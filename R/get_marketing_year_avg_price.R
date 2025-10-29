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
#'     "COTTON, COTTONSEED - PRICE RECEIVED, MEASURED IN $ / TON",
#'     "COTTON, UPLAND - PRICE RECEIVED, MEASURED IN $ / LB",
#'     "SORGHUM, GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
#'     "SUGARBEETS - PRICE RECEIVED, MEASURED IN $ / TON",
#'     "PEAS, DRY EDIBLE - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "SUNFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "RICE - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "RICE, MEDIUM-SHORT GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "RICE, LONG GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "PEANUTS - PRICE RECEIVED, MEASURED IN $ / LB",
#'     "CANOLA - PRICE RECEIVED, MEASURED IN $ / CWT",
#'     "MAPLE SYRUP - PRICE RECEIVED, MEASURED IN $ / GALLON",
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
                    by = c("commodity_name","state_code", "commodity_year")]
  
  NATIONAL_df <- df[get("agg_level_desc") == "NATIONAL",
                    .(NATIONAL_Mya = mean(price_received, na.rm = TRUE)),
                    by = c("commodity_name", "commodity_year")]
  
  ## Merge and filter
  df <- merge(NATIONAL_df, STATE_df,
              by = c("commodity_name", "commodity_year"),
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
  
  # # Keep only rows with finite commodity_name and the four columns you need
  # df <- df[
  #   is.finite(df[["commodity_name"]]),
  #   c("commodity_year","commodity_name","state_code","marketing_year_avg_price")]
  
  # Unit conversions
  df[commodity_name %in%  c("Canola","Rice","Safflower","Dry Beans","Dry Peas","Sunflowers"),
     marketing_year_avg_price := marketing_year_avg_price / 100]
  
  # sorghum cwt to bu (code 51)
  df[commodity_name == "Grain Sorghum",
     marketing_year_avg_price := (marketing_year_avg_price / 100) * 56]
  
  # sugar ton to lb (codes 38, 39)
  df[commodity_name %in% c("Sugar Beets","Sugarcane"),
     marketing_year_avg_price := marketing_year_avg_price / 2000]
  
  ## Tag and return
  df[, data_source := "USDA NASS Quick Stats"]
  return(df)
}