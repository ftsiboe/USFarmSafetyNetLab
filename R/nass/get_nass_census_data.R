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
#' @family USDA NASS Quick Stats
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