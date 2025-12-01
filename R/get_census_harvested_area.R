#' Retrieve and Aggregate NASS Harvested Area Data
#'
#' Extracts, filters, and aggregates harvested area information for major field
#' crops from pre-processed USDA NASS datasets (e.g., Census of Agriculture).
#' Multiple crop-specific descriptors (e.g., "CORN, GRAIN - ACRES HARVESTED",
#' "CORN, SILAGE - ACRES HARVESTED") are standardized into unified commodity
#' groups, then aggregated to county level and (optionally) rolled up to state
#' and national levels across selected years.
#'
#' @param dir_source Character. Path to the root directory containing the
#'   downloaded or pre-processed NASS datasets.
#' @param census_years Numeric vector of Census of Agriculture years to include.
#'   Default: \code{c(2002, 2007, 2012, 2017, 2022)}.
#' @param aggregation_level Character vector specifying one or more levels of
#'   geographic aggregation to include. One or more of \code{"COUNTY"},
#'   \code{"STATE"}, \code{"NATIONAL"}. COUNTY data are always queried and used
#'   as the base for rollups.
#' @param map_crop_area Named list mapping standardized crop names to their
#'   corresponding NASS \code{short_desc} values used for filtering harvested
#'   area items. Defaults cover barley, corn, cotton, oats, peanuts, rice,
#'   sorghum, soybeans, and wheat.
#'
#' @details
#' For each requested census dataset, the function:
#' \enumerate{
#'   \item Calls \code{process_nass_dataset()} with filters for harvested area.
#'   \item Normalizes and coerces numeric values.
#'   \item Maps NASS descriptors to standardized \code{commodity_name}.
#'   \item Aggregates to COUNTY, then (optionally) rolls up to STATE and NATIONAL.
#' }
#' Datasets that cannot be read/processed are skipped silently.
#'
#' @return A single \code{data.table} with aggregated harvested area and columns:
#' \describe{
#'   \item{commodity_year}{Numeric; year of the commodity observation.}
#'   \item{commodity_name}{Character; standardized crop identifier.}
#'   \item{state_code}{State FIPS code (NA at NATIONAL level).}
#'   \item{county_code}{County FIPS code (NA at STATE/NATIONAL levels).}
#'   \item{value}{Total harvested area (acres).}
#'   \item{agg_level}{One of \code{"COUNTY"}, \code{"STATE"}, \code{"NATIONAL"}.}
#' }
#' @import data.table
#' @export
get_census_harvested_area <- function(
    dir_source,
    census_years      = c(2002, 2007, 2012, 2017, 2022),
    aggregation_level = c("STATE","NATIONAL","COUNTY"),
    map_crop_area = list(
      barley   = "BARLEY - ACRES HARVESTED",
      corn     = c("CORN, GRAIN - ACRES HARVESTED","CORN, SILAGE - ACRES HARVESTED"),
      cotton   = "COTTON - ACRES HARVESTED",
      oats     = "OATS - ACRES HARVESTED",
      peanuts  = "PEANUTS - ACRES HARVESTED",
      rice     = "RICE - ACRES HARVESTED",
      sorghum  = c("SORGHUM, GRAIN - ACRES HARVESTED","SORGHUM, SILAGE - ACRES HARVESTED","SORGHUM, SYRUP - ACRES HARVESTED"),
      soybeans = "SOYBEANS - ACRES HARVESTED",
      wheat    = "WHEAT - ACRES HARVESTED"
    )
){
  stopifnot(is.numeric(census_years))
  ds_names <- paste0("census", as.integer(census_years))
  
  nass_harvested_area <- lapply(ds_names, function(large_dataset){
    tryCatch({
      data <- process_nass_dataset(
        dir_source    = census_directory,
        large_dataset = large_dataset,
        nassqs_params = list(
          short_desc      = unlist(map_crop_area, use.names = FALSE),
          agg_level_desc  = aggregation_level,
          domaincat_desc  = "NOT SPECIFIED",
          domain_desc     = "TOTAL"
        )
      )
      data.table::setDT(data)
      if (!is.numeric(data$value)) data[, value := suppressWarnings(as.numeric(value))]
      
      # Map short_desc -> standardized roots; keep only recognized rows
      data[,commodity_name:= NA_character_]
      for(crop in names(map_crop_area)){
        data[short_desc %in% map_crop_area[[crop]], commodity_name := crop]
      }
      
      data <- data[!is.na(commodity_name)]
      
      res_list <- list()

      if ("COUNTY" %in% aggregation_level) {
        res_list[["COUNTY"]] <- data[
          , .(value = sum(value, na.rm = TRUE)),
          by = .(commodity_year, commodity_name, state_code, county_code)
        ][, agg_level := "COUNTY"]
      }

      if ("STATE" %in% aggregation_level) {
        res_list[["STATE"]] <- data[
          , .(value = sum(value, na.rm = TRUE)),
          by = .(commodity_year, commodity_name, state_code)
        ][, `:=`(county_code = NA_character_, agg_level = "STATE")]
      }
      
      if ("NATIONAL" %in% aggregation_level) {
        res_list[["NATIONAL"]] <- data[
          , .(value = sum(value, na.rm = TRUE)),
          by = .(commodity_year, commodity_name)
        ][, `:=`(
          state_code  = NA_character_,
          county_code = NA_character_,
          agg_level   = "NATIONAL"
        )]
      }
      
      data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)
      
      }, error = function(e) NULL)
  })
  
  nass_harvested_area <- data.table::rbindlist(nass_harvested_area, use.names = TRUE, fill = TRUE)
  
  setnames(nass_harvested_area,old = c("commodity_year","value"), new = c("census_year","harvested_area"))
  
  nass_harvested_area[]
}
