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
    by = agg_level_list]
  
  rm(data_prod, data_area);gc()
  
  return(df)
}