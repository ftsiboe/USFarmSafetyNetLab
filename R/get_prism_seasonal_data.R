#' Download and prepare PRISM seasonal weather data for a single year
#'
#' @description
#' All weather variables were first aggregated to the county level as the
#' weighted mean of all the PRISM grids within each county, using the proportion
#' of each PRISM grid related to the respective crop’s cover based on cropland
#' data layers (CDLs) from NASS CropScape (2008–2022) as the aggregation weights.
#' Subsequently, the weather variables were further aggregated over each year’s
#' growing season for each crop and county.
#'
#' This function fetches those **pre-aggregated** (county × crop × year) growing-season
#' summaries for a single year, filters to a chosen climate scenario (`SimNum`),
#' standardizes column names, optionally converts precipitation units, and
#' averages across commodity *types* within each `commodity_code` (relevant for wheat).
#'
#' @details
#' **Climate scenarios (`SimNum`):**
#' - `SimNum` shows climate scenarios of +0.0, +0.5, +1.0, +1.5, +2.0, +2.5, +3.0
#'   that are derived by adding the respective temperature increase to the observed
#'   historic value.
#' - `SimNum = 0` is the actual observed historic value.
#'
#' **Variable definitions:**
#' - `commodity_year` : commodity year
#' - `state_code` : state code
#' - `county_code` : county code
#' - `commodity_code` : commodity code
#'   - `ppt`  : precipitation (inches if `convert_precip="inch"`, mm if `"mm"`),
#'   - `tmin` : mean daily minimum temperature (°C),
#'   - `tmax` : mean daily maximum temperature (°C),
#'   - `tavg` : mean daily average temperature (°C),
#'   - `freeze`: exposure to temperatures below 0°C (°C·days; as provided),
#'   - Degree-day summaries `DD*` 
#'      - `DD1`    : degree days from 0–10°C,
#'      - `DD2_28` : degree days from 10–28°C, ... up to `DD2_35`,
#'      - `DD3_28` : degree days above 28°C, ... up to `DD3_35`.
#'
#' **Column renames (for merging with other datasets):**
#' - `crop_yr`  → `commodity_year`
#' - `state_cd` → `state_code`
#' - `county_cd`→ `county_code`
#' - `crop_cd`  → `commodity_code`
#'
#' @param year Integer; the year to download (e.g., `2020`).
#' @param simnum Integer climate-scenario code to keep (default `0`, observed).
#' @param convert_precip One of `"inch"` (default; converts mm → inches), `"mm"` (ensure millimeters), or `"none"` (leave as-is).
#' @param cache_dir Optional directory to cache the `.rds` file. Defaults to `tempdir()`.
#' @param overwrite Logical; if `TRUE`, re-download even if cached. Default `FALSE`.
#' @param quiet Logical; silence `download.file()` output. Default `TRUE`.
#'
#' @return A `data.table` keyed by `commodity_year, state_code, county_code, commodity_code`
#'   with `ppt, tmin, tmax, tavg, freeze` and all `DD*` columns.
#'
#' @import package data.table
#' @export
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- get_prism_seasonal_data(2018)
#' dt[]
#' }
get_prism_seasonal_data <- function(
    year,
    simnum = 0L,
    convert_precip = c("inch", "mm", "none"),
    cache_dir = NULL,
    overwrite = FALSE,
    quiet = TRUE
){
  stopifnot(length(year) == 1L, is.numeric(year), !is.na(year))
  if (!is.numeric(simnum) || length(simnum) != 1L || is.na(simnum)) {
    stop("`simnum` must be a single numeric/integer value.")
  }
  convert_precip <- match.arg(convert_precip)
  
  if (is.null(cache_dir)) cache_dir <- tempdir()
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  url  <- sprintf("https://people.beocat.ksu.edu/~ftsiboe/prism_seasonal/prism_seasonal_%d.rds", as.integer(year))
  dest <- file.path(cache_dir, sprintf("prism_seasonal_%d.rds", as.integer(year)))
  
  if (!file.exists(dest) || isTRUE(overwrite)) {
    tryCatch(
      utils::download.file(url, destfile = dest, mode = "wb", quiet = quiet),
      error = function(e) stop(sprintf("Failed to download year %s: %s", year, conditionMessage(e)))
    )
  }
  
  dt <- tryCatch(readRDS(dest),
                 error = function(e) stop(sprintf("Failed to read %s: %s", dest, conditionMessage(e))))
  dt <- data.table::as.data.table(dt)
  
  # Sanity checks for required columns
  req_cols <- c("SimNum","ppt","crop_yr","state_cd","county_cd","crop_cd")
  miss <- setdiff(req_cols, names(dt))
  if (length(miss)) stop("Missing expected columns: ", paste(miss, collapse = ", "))
  
  # Keep the selected climate scenario
  dt <- dt[SimNum == simnum]
  
  # Precipitation conversion (source assumed mm)
  if (convert_precip == "inch") {
    dt[, ppt := ppt / 25.4]
  } # "mm" or "none": leave as-is
  
  # Standardize column names for merging
  data.table::setnames(
    dt,
    old = c("crop_yr","state_cd","county_cd","crop_cd"),
    new = c("commodity_year","state_code","county_code","commodity_code")
  )
  
  # Identify measure columns (ppt/tmin/tmax/tavg/freeze + all DD*)
  dd_cols <- grep("^DD", names(dt), value = TRUE)
  measure_cols <- intersect(c("ppt","tmin","tmax","tavg","freeze", dd_cols), names(dt))
  if (!length(measure_cols)) stop("No measure columns found to aggregate.")
  
  # Average across commodity *types* within commodity_code (e.g., wheat classes)
  dt <- dt[,
           lapply(.SD, function(x) mean(x, na.rm = TRUE)),
           by = c("commodity_year","state_code","county_code","commodity_code"),
           .SDcols = measure_cols]
  
  data.table::setkeyv(dt, c("commodity_year","state_code","county_code","commodity_code"))
  dt[]
}
