
#' Estimate FCIP Unloaded (County) Rates
#'
#' Computes the “unloaded” loss cost rates (\code{tau}) for counties based on
#' the FCIC Rate Methodology Handbook (2009), pp. 65–70.
#'
#' @param statplan A \link[data.table]{data.table} containing FCIP rate elements
#'   with at least the columns:
#'   \describe{
#'     \item{state_code, county_code}{Identifiers for each county.}
#'     \item{contiguous_state_code, contiguous_county_code}{Mapping to county group.}
#'     \item{insured_area}{Total insured acres in the county.}
#'     \item{lcr}{Loss Cost Rate for each county.}
#'     \item{commodity_code}{Crop identifier.}
#'   }
#' @param year Integer. Crop year for which rates are being estimated
#'   (currently not used but reserved for future subsetting).
#' @param crop Optional vector of commodity codes to filter by crop.
#' @param state Optional vector of state codes to restrict the analysis.
#' @param county Optional vector of county codes to restrict the analysis.
#'
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{state_code, county_code, commodity_code}{Keys identifying county and crop.}
#'     \item{tau}{Estimated FCIP county “unloaded” rate.}
#'   }
#' @import data.table
#' 
#' @details
#' 1. **Target data** is filtered to the selected state(s)/county(ies).  
#' 2. **Group data** finds contiguous‐county groupings, unions them with the target.  
#' 3. Computes group‐level statistics:
#'   - \code{c_alpha}: mean insured acres  
#'   - \code{c_u}: mean LCR  
#'   - \code{c_a}: variance of LCR  
#' 4. Computes target county statistics:
#'   - \code{c_x}: mean LCR  
#'   - \code{c_v}: variance of LCR  
#'   - \code{c_net_acre}: total insured acres  
#' 5. Applies the blending formula  
#'   \deqn{\tau = Z\,x + (1 - Z)\,u, \quad Z = P/(P + K)}  
#'   where  
#'   \eqn{P = c_{\!net\_acre}/c_\alpha,\quad K = c_v/c_a.}
#'
#' @references
#' FCIC Rate Methodology Handbook APH (2009), pp. 65–70.  
#' \url{https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf}
#'
#' @export
estimate_fcip_unloaded_rate <- function(
    statplan,
    year   = 2011,
    crop   = NULL,
    state  = NULL,
    county = NULL) {
  # Documentation link:
  # https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf
  
  # 1. Filter to the target county(ies)/state(s)
  target_data <- statplan[
    state_code %in% state & county_code %in% county
  ]
  
  # 2. Build the contiguous‐county group for each target county
  group_data <- target_data[contiguous_county, on = .(state_code, county_code), nomatch = 0
  ][, .(state_code = contiguous_state_code, county_code = contiguous_county_code)]
  group_data <- unique(group_data)
  
  # 3. Combine group members with the original target_data
  group_data  <- unique(rbind(group_data[statplan  , on = .(state_code, county_code), nomatch = 0],target_data))
  
  # 4. Compute group-level insured area mean (c_alpha), LCR mean (c_u), LCR variance (c_a)
  group_data <- group_data[, .(
    c_alpha = mean(insured_area,na.rm=T),c_a = var(lcr,na.rm=T),
    c_u = mean(lcr,na.rm=T)), by = .(commodity_code)]
  
  # 5. Compute target county LCR stats: variance (c_v), mean (c_x), net insured acres (c_net_acre)
  target_data <- target_data[, .(
    c_v = var(lcr,na.rm=T), c_x = mean(lcr,na.rm=T),
    c_net_acre = sum(insured_area,na.rm=T)), by = .(state_code,county_code,commodity_code)]
  
  # 6. Join target county stats with group stats by commodity_code
  res <- target_data[group_data, on = .(commodity_code), nomatch = 0]
  
  # 7. Calculate P, K, Z, and tau according to the handbook formula
  res[, c_P := c_net_acre/c_alpha]
  res[, c_K := c_v/c_a]
  res[, c_Z := c_P/(c_P+c_K)]
  res[, tau := c_Z*c_x + (1-c_Z)*c_u] # County Unloaded Rate (same as target rate).
  
  # 8. Filter out invalid or zero tau values
  res <- res[!tau %in% c(NA, Inf, -Inf, NaN, 0), ]
  
  # 9. Return a clean data.frame with only the columns of interest
  res <- res[, .(state_code, county_code, commodity_code, tau)]
  
  return(res)
}


#' Estimate FCIP Instrumental Variables (Unloaded Rates)
#'
#' Uses historical FCIP rate data to build instrumented unloaded‐rate variables
#' following:
#'   1. Tsiboe & Turner (2023), “Econometric identification of crop insurance participation”  
#'      _Agricultural and Resource Economics Review_, 52(3):476–497.  
#'      \url{https://doi.org/10.1017/age.2023.13}  
#'
#' @param year Integer. The target crop year for which to construct instruments.
#' @param statplan A data.table containing FCIP rate elements, including at least:
#'   \describe{
#'     \item{commodity_year}{Year of the rate observation.}
#'     \item{state_code, county_code}{County identifiers.}
#'     \item{commodity_code}{Crop identifier.}
#'     \item{insured_area, lcr, contiguous_state_code, contiguous_county_code}{Fields
#'       required by \code{estimate_fcip_unloaded_rate()}.}
#'   }
#'
#' @import data.table
#' @return A data.table with one row per county–crop for the specified \code{year},
#'   containing:
#'   \describe{
#'     \item{state_code, county_code, commodity_code}{Keys.}
#'     \item{tau_sob}{Smoothed unloaded rate (uses contiguous‐county means to fill zeros/NAs).}
#'     \item{commodity_year}{The input \code{year}, repeated.}
#'   }
#'
#' @details
#' 1. **Task list**: Identify all unique (state, county) pairs with data in the  
#'    2–21 years before \code{year}.  
#' 2. **Unloaded‐rate calculation**: For each county in \code{task_list}, call  
#'    \code{estimate_fcip_unloaded_rate()} on the same 2–21 year window to get \code{tau}.  
#'    Errors return \code{NULL} so processing continues.  
#' 3. **Contiguous‐county smoothing**:  
#'    - Build a lookup table of contiguous counties (using \code{contiguous_county}).  
#'    - For each contiguous group, compute the mean \code{tau} to get \code{tau_c}.  
#' 4. **Merge & fill**: Left‐join the raw \code{adm} and \code{contiguous_adm};  
#'    replace any zero/NA/Inf \code{tau} with the group mean \code{tau_c} into  
#'    \code{tau_sob}.  
#' 5. **Cleanup**: Drop helper columns (\code{tau}, \code{tau_c}), remove invalid rows,  
#'    add \code{commodity_year}, and return the result.
#'
#' @seealso \code{\link{estimate_fcip_unloaded_rate}}
#' @export
estimate_fcip_instruments <- function(year, statplan) {
  
  # 1. Build list of (state, county) with at least 2–21 years of data before 'year'
  task_list <- unique(statplan[commodity_year %in% (year-2):(year-21), .(state_code, county_code)])
  
  # 2. For each county, compute the unloaded rate via the helper function
  adm <- data.table::rbindlist(
    lapply(
      1:nrow(task_list),
      function(i){
        tryCatch({
          estimate_fcip_unloaded_rate(
            statplan = statplan[commodity_year %in% (year-2):(year-21)],
            year   = year,
            state  = task_list$state_code[i],
            county = task_list$county_code[i])
        }, error = function(e){return(NULL)})
      }), fill = TRUE)
  setDT(adm)
  
  # 3. Prepare contiguous county mapping for smoothing
  setDT(contiguous_county)
  contiguous_county[, state_code := contiguous_state_code]
  contiguous_county[, county_code := contiguous_county_code]
  contiguous_adm <- unique(contiguous_county, by = c("state_code", "county_code"))
  
  # 4. For each contiguous group, compute the mean tau => tau_c
  contiguous_adm <- data.table::rbindlist(
    lapply(
      1:nrow(contiguous_adm),
      function(ss){
        tryCatch({
          # ss <- 1
          data <- contiguous_adm[ss][contiguous_county, on = .(state_code, county_code), nomatch = 0][
            adm, on = .(state_code, county_code), nomatch = 0]
          
          data <- data[, .(tau_c = mean(tau, na.rm = TRUE)),by = .(state_code, county_code, commodity_code)]
          
          return(data)
        }, error = function(e){return(NULL)})
      }), fill = TRUE)
  
  # 5. Merge raw rates with contiguous‐county smoothed rates
  adm <- adm[contiguous_adm, on = intersect(names(adm), names(contiguous_adm)), nomatch = 0]
  
  # 6. Replace any invalid/zero tau with the smoothed tau_c
  adm[, tau_sob := fifelse(tau %in% c(NA, Inf, -Inf, NaN) | tau == 0, tau_c, tau)]
  
  # 7. Drop helper columns and invalid rows
  rm(contiguous_adm);gc()
  adm <- adm[, setdiff(names(adm), c("tau_c", "tau")), with = FALSE]
  adm <- adm[!tau_sob %in% c(NA, Inf, -Inf, NaN,0)]
  
  # 8. Tag with the target commodity_year and return
  adm[, commodity_year := year]
  gc()
  return(adm)
}


#' Formulate & Merge National Subsidy Rate Instrument (Yu et al., 2018)
#'
#' Downloads the historical “Summary of Business” RDS and computes
#' national subsidy‐rate instruments at specified coverage levels,
#' following Yu et al. (2018).
#'
#' @param dt sobcov
#' @param delivery_systems Character vector. Delivery systems to include;
#'                   default \code{c("RBUP","FBUP")}.
#' @param plan_codes Integer vector. Insurance plan codes to include;
#'                   default \code{c(1:3, 90, 44, 25, 42)}.
#' @param coverage_levels Numeric vector. Percent coverage levels to keep;
#'                   default \code{c(65, 75)}.
#'
#' @return A data.table with columns: commodity_year, subsidy_rate_65, subsidy_rate_75.
#'
#' @import data.table
#' @importFrom tidyr spread
#' @export
get_yu2018_instrument <- function(
    dt,
    delivery_systems  = c("RBUP", "FBUP"),
    plan_codes        = c(1:3, 90, 44, 25, 42),
    coverage_levels   = c(65, 75)) {
  # 2. Read into data.table
  dt <- data.table::as.data.table(dt)
  
  # 3. Filter to relevant delivery systems & plan codes
  dt <- dt[delivery_type %in% delivery_systems]
  dt <- dt[insurance_plan_code   %in% plan_codes]
  
  # 4. Create a label for each coverage level
  #    (round to nearest 5% then convert to a “subsidy_rate_##” string)
  dt[, coverage_level_percent := paste0("subsidy_rate_",(round((coverage_level_percent / 0.05)) * 0.05) * 100)]
  
  # 5. Keep only the coverage levels we need
  dt <- dt[coverage_level_percent %in% paste0("subsidy_rate_", coverage_levels)]
  
  # 6. Summarize total subsidy and premium by crop year & coverage level
  dt_sum <- dt[
    , .(
      subsidy_amount    = sum(subsidy_amount,    na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount, na.rm = TRUE)
    ),by = .(commodity_year, coverage_level_percent)]
  
  # 7. Convert to rate = subsidy_amount / total premium
  dt_sum[, subsidy_rate := subsidy_amount / total_premium_amount]
  
  # 8. Reshape to wide: one column per coverage level’s subsidy rate
  #    (requires tidyr)
  dt_wide <- dt_sum[, .(commodity_year, coverage_level_percent, subsidy_rate)] %>%
    tidyr::spread(coverage_level_percent, subsidy_rate)
  
  # 9. Return as data.table
  return(data.table::as.data.table(dt_wide))
}


#' Calculate key distributional moments for a numeric vector of revenues
#'
#' @param x Numeric vector of revenues (e.g. per-draw revenues)
#' @return A one-row data.frame with:
#'   - mu:      mean
#'   - md:      median
#'   - sd:      standard deviation
#'   - cv:      coefficient of variation (sd / mu)
#'   - vr:      variance
#'   - sk:      skewness
#'   - ku:      kurtosis
#'   - lapv:    mean squared loss deviations below the mean
#'   - lrpv:    lapv / probability of loss
#'   - nlapv:   lapv normalized by mu
#'   - nlrpv:   lrpv normalized by mu
#'   - lres2:   same as lapv (for clarity)
#'   - cdf:     empirical probability of loss (P(X < μ))
#' @import moments
#' @importFrom stats median var
#' @export
calculate_revenue_moments <- function(x) {
  mu    <- mean(x,     na.rm = TRUE)
  md    <- median(x,   na.rm = TRUE)
  sd    <- sd(x,       na.rm = TRUE)
  cv    <- sd / mu
  vr    <- var(x,      na.rm = TRUE)
  sk    <- moments::skewness(x, na.rm = TRUE)
  ku    <- moments::kurtosis(x, na.rm = TRUE)
  res   <- x - mu
  lres2 <- mean(ifelse(res > 0, NA, res^2), na.rm = TRUE)
  cdf   <- mean(ifelse(res < 0, 1, 0), na.rm = TRUE)
  lapv  <- lres2
  lrpv  <- lapv / cdf
  nlapv <- lapv / mu
  nlrpv <- lrpv / mu
  
  data.frame(
    mu    = mu,
    md    = md,
    sd    = sd,
    cv    = cv,
    vr    = vr,
    sk    = sk,
    ku    = ku,
    lapv  = lapv,
    lrpv  = lrpv,
    nlapv = nlapv,
    nlrpv = nlrpv,
    lres2 = lres2,
    cdf   = cdf,
    stringsAsFactors = FALSE
  )
}


#' Prepare and demean data for fixed‐effects models
#'
#' This function
#'   1. Filters to complete cases on the specified panel, time, weight, variables, and output  
#'   2. If `output` is NULL, creates a dummy output column filled with 1s  
#'   3. Drops any panel with only one observation  
#'   4. Computes within‐panel means for the output + each variable in `varlist` (`_mean_i`)  
#'   5. Computes overall sample means for the same set of variables (`_mean`)  
#'   6. Replaces each variable in `varlist` by `value − within_panel_mean + overall_mean`  
#'
#' @param data    A data.frame or data.table containing the data.
#' @param varlist Character vector of variable names to be demeaned.
#' @param panel   Character vector of column name(s) defining the panel identifier.
#' @param time    Character scalar name of the time variable.
#' @param wvar    Character scalar name of a variable to keep but _not_ demean (optional, default NULL).
#' @param output  Character scalar name of an “output” variable whose means are computed but not altered; if NULL, a dummy column named `"output"` is created (optional, default NULL).
#'
#' @return A list with components  
#'   - **data**: a data.table containing  
#'       - the original `panel`, `time`, `wvar`, `varlist`, and `output` columns  
#'       - two mean columns for each of `c(output, varlist)`:  
#'         `<name>_mean_i` (within‐panel) and `<name>_mean` (overall)  
#'   - **NFE**: the number of panels with more than one observation
#'
#' @import data.table
#' @export
fixed_effect_model_data_prep <- function(
    data, 
    varlist, 
    panel, 
    time, 
    wvar = NULL, 
    output = NULL) {
  
  # 1) ensure data.table
  dt <- as.data.table(data)
  
  # 2) if no output specified, create dummy
  if (is.null(output)) {
    output <- "output"
    dt[, output := 1]
  }
  
  # 3) keep only needed columns, drop incomplete rows
  keep_cols <- unique(c(panel, time, wvar, varlist, output))
  dt <- dt[, ..keep_cols]
  dt <- dt[complete.cases(dt)]
  
  # 4) drop panels with only one obs, count panels remaining
  dt[, obs := .N, by = panel]
  dt <- dt[obs > 1]
  NFE <- dt[, uniqueN(panel)]
  
  # 5) compute within‐panel means (_mean_i)
  mean_vars <- c(output, varlist)
  dt[, paste0(mean_vars, "_mean_i") := lapply(.SD, mean),by = panel, .SDcols = mean_vars]
  
  # 6) compute overall sample means (_mean)
  dt[, ALL := 1L]
  dt[, paste0(mean_vars, "_mean") := lapply(.SD, mean),by = ALL, .SDcols = mean_vars]
  
  # 7) demean each variable in varlist
  dt[, (varlist) := lapply(varlist, function(v)
    get(v) - get(paste0(v, "_mean_i")) + get(paste0(v, "_mean")))]
  
  # 8) select final columns, drop any leftover NAs
  final_cols <- c(
    panel, time, wvar,
    varlist, output,
    paste0(output, c("_mean_i", "_mean")))
  
  dt <- dt[, ..final_cols]
  dt <- dt[complete.cases(dt)]
  
  # 9) return
  list(data = dt,NFE  = NFE)
}

#' Panel‐based spatial smoothing estimator
#'
#' This function
#'   1. Constructs spatially‐varying treatment interactions (one variable per spatial unit)  
#'   2. Applies within‐panel/time fixed‐effects demeaning to both outcome and interactions  
#'   3. Fits an OLS model by hand (\code{lm.fit}) to recover one coefficient per spatial unit  
#'
#' @param data      A \code{data.table} or \code{data.frame} containing panel data.
#' @param output    Name of the outcome variable (character scalar).
#' @param treatment Name of the treatment variable whose spatial effects we estimate (character scalar).
#' @param time      Name of the time variable (character scalar).
#' @param panel     Name(s) of the panel identifier variable(s) (character vector).
#' @param spatialvar Name of the spatial grouping variable (e.g. county FIPS; character scalar).
#'
#' @return A \code{data.table} with columns:
#'   - \code{estimate}: the estimated spatial‐unit coefficient  
#'   - \code{county_fips}: the spatial unit identifier (5‐digit FIPS)  
#'   - \code{state_code}, \code{county_code}: parsed FIPS components  
#'
#' @details
#' Internally, we  
#'   1. Build \code{treatment_code} = \code{I(spatialvar==code) * treatment} for each spatial unit code.  
#'   2. Call \code{fixed_effect_model_data_prep()} to demean the outcome and all \code{treatment_code} variables.  
#'   3. Assemble the design matrix \code{X = [output_mean_i, treatment_*]} and response \code{y}.  
#'   4. Solve \eqn{\hat\beta = (\tilde X'\tilde X)^{-1}\tilde X'\tilde y} via \code{lm.fit}.  
#'   5. Return a row per spatial unit with its coefficient.  
#'
#' @import data.table
#' @importFrom stats lm.fit
#' @export
panel_based_spatial_smoothing_estimator <- function(
    data,
    output,
    treatment,
    time,
    panel,
    spatialvar){
  
  # 1) create treatment‐by‐spatial‐unit interaction columns
  spatialvar_list <- data[, unique(get(spatialvar))]
  data[ , paste0(treatment, '_', spatialvar_list) := 
          lapply(spatialvar_list, function(code) 
            as.integer(get(spatialvar) == code) * get(treatment)
          )
  ]
  
  # 2) demean outcome & interactions via panel/time fixed effects
  fe_prep <- fixed_effect_model_data_prep(
    data    = data,
    varlist = names(data)[grepl(paste0(treatment, '_'), names(data))],
    panel   = panel,
    time    = time,
    wvar    = spatialvar,
    output  = output
  )
  data <- fe_prep$data
  
  # 3) build response vector y and design matrix X
  y <- data[[output]]
  X <- as.matrix(data[, c(
    paste0(output, '_mean_i'),
    names(data)[grepl(paste0(treatment, '_'), names(data))]
  ), with = FALSE])
  
  # free memory
  rm(data); gc()
  
  # 4) fit OLS by hand using QR (lm.fit)
  coeffs <- lm.fit(X, y)$coefficients
  
  # 5) assemble results
  fit_df <- data.table(
    estimate     = coeffs,
    county_fips  = gsub(paste0(treatment, '_'), '', names(coeffs))
  )
  
  # keep only valid spatial units
  fit_df <- fit_df[county_fips %in% spatialvar_list]
  fit_df <- fit_df[complete.cases(fit_df)]
  
  return(fit_df)
}



#' Smooth county‐level estimates via spatial contiguity
#'
#' This function
#'   1. Loads all U.S. county geometries via **urbnmapr**
#'   2. Joins in a user‐supplied data.frame of county estimates (by 5‐digit FIPS)
#'   3. (Optionally) converts to a **terra** SpatVector for further terra ops
#'   4. Builds a contiguity neighbor list using **sf::st_touches**
#'   5. Iteratively fills any missing estimates by applying a user‐supplied function to neighbor values
#'   6. Classifies the smoothed values into `n_classes` using Jenks natural breaks 
#'
#' @param data         A data.frame or tibble containing at least two columns:
#'                       - a 5‐digit FIPS code (character)
#'                       - a numeric estimate to be smoothed
#' @param fip_col      Name of the FIPS‐code column in `fit_df` (default 'fip').
#' @param estimate_col Name of the numeric estimate column in `fit_df` (default 'estimate').
#' @param iterations   Number of smoothing passes to perform (default 5).
#' @param fun          A function to aggregate neighbor values; must take a numeric vector and return a single numeric (default mean).
#' @param n_classes    Number of categories for Jenks natural‐breaks classification (default 10).
#'
#' @return An **sf** object of all U.S. counties with these new columns:  
#'   - estimate: your original values (NA where missing)  
#'   - estimate_smooth: the same values after neighborhood‐function imputation  
#'   - estimate_cat: factor giving the Jenks‐break category  
#'
#' @import dplyr
#' @importFrom urbnmapr get_urbn_map
#' @importFrom stats setNames
#' @importFrom sf st_touches
#' @importFrom utils head tail
#' @importFrom terra vect
#' @importFrom magrittr "%>%"
#' @importFrom classInt classIntervals
#' @export
smooth_county_estimates <- function(
    data,
    fip_col       = "fip",
    estimate_col  = "estimate",
    iterations    = 5,
    fun           = mean,
    n_classes     = 10) {
  # 1) load all U.S. counties
  counties_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE)
  
  # 2) join in user estimates by FIPS
  by_arg <- stats::setNames(fip_col, "county_fips")
  map_sf <- dplyr::left_join(counties_sf, data, by = by_arg)
  
  # 3) (optional) convert to terra::SpatVector
  counties_vect <- terra::vect(map_sf)
  
  # 4) build contiguity list via sf
  nbr_list <- sf::st_touches(map_sf)
  
  # 5) iterative neighbor‐function smoothing
  sm      <- map_sf[[estimate_col]]
  missing <- which(is.na(sm))
  
  for (i in seq_len(iterations)) {
    sm_new <- sm
    sm_new[missing] <- sapply(missing, function(j) {
      ngb_vals <- sm[nbr_list[[j]]]
      ngb_vals <- ngb_vals[!is.na(ngb_vals)]
      if (length(ngb_vals) == 0) {
        NA_real_
      } else {
        fun(ngb_vals)
      }
    })
    sm <- sm_new
  }
  
  map_sf$estimate_smooth <- sm
  # 6) classify the smoothed estimates using Jenks natural breaks
  #    use unique values to avoid duplicate‐break errors
  uniq_vals <- unique(sm[!is.na(sm)])
  brks      <- unique(classInt::classIntervals(uniq_vals,
                                               n = n_classes,
                                               style = "jenks")$brks)
  map_sf <- map_sf %>%
    dplyr::mutate(
      estimate_cat = cut(
        estimate_smooth,
        breaks         = brks,
        include.lowest = TRUE,
        labels         = paste0(
          format(round(head(brks, -1), 2), nsmall = 2),
          "–",
          format(round(tail(brks, -1), 2), nsmall = 2)
        )
      )
    )
  
  map_sf
  
}
