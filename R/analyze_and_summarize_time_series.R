#' Analyze and summarize time-series data by disaggregate and outcome using data.table
#'
#' This function computes comprehensive summary statistics, trend diagnostics,
#' and both magnitude-sorted and chronological notable changes for each
#' combination of `disaggregate` level and `outcome`.
#'
#' @param dt A data.table or data.frame containing the columns:
#'   - `commodity_year` (integer)
#'   - `disaggregate` (character or factor)
#'   - `outcome` (character or factor)
#'   - `value` (numeric)
#' @param threshold Numeric. Percent threshold (e.g., 10 for 10%) used to flag
#'   `notable_growth` (>= +threshold) and `notable_decline` (<= -threshold).
#'   Defaults to 10.
#'
#' @return A named list of outcomes. Each element is a named list of one-row data.frames (one per disaggregate level) containing the following summary columns:
#'   - **mean**: Arithmetic mean of the `value` series.
#'   - **sd**: Standard deviation of the `value` series.
#'   - **median**: Median of the `value` series.
#'   - **iqr**: Interquartile range (75th-25th percentile) of the `value` series.
#'   - **cv**: Coefficient of variation (sd divided by mean).
#'   - **n_obs**: Number of non-missing observations.
#'   - **pct_missing**: Percentage of years with missing `value`.
#'   - **initial_year_value**: Value in the first `commodity_year` period.
#'   - **final_year_value**: Value in the last `commodity_year` period.
#'   - **year_with_max_value**: Year when `value` is maximized.
#'   - **max_value**: Maximum `value` observed.
#'   - **year_with_min_value**: Year when `value` is minimized.
#'   - **min_value**: Minimum `value` observed.
#'   - **cumulative_change**: Absolute change (final minus initial value).
#'   - **mean_annual_growth_rate**: Compound annual growth rate (CAGR) in percent.
#'   - **growth_initial_to_final**: Total percent growth from initial to final value.
#'   - **first_diff_sd**: Standard deviation of year-to-year percent changes.
#'   - **max_drawdown_pct**: Greatest percent drop from a peak to subsequent trough.
#'   - **trend_slope**: Slope of linear model `value ~ commodity_year`.
#'   - **trend_p_value**: P-value for slope significance in the trend model.
#'   - **trend_r_squared**: R-squared of the trend model.
#'   - **time_to_peak**: Years from start until the peak year.
#'   - **time_to_trough**: Years from start until the trough year.
#'   - **notable_growth**: Magnitude-sorted list of year: +% changes greater than or equal to threshold.
#'   - **notable_growth_chron**: Chronologically-sorted list of year: +% changes greater than or equal to threshold.
#'   - **notable_decline**: Magnitude-sorted list of year: -% changes less than or equal to -threshold.
#'   - **notable_decline_chron**: Chronologically-sorted list of year: -% changes less than or equal to -threshold.
#'   - **any_missing_years**: Logical flag for missing years in the series.
#'   - **monotonic**: Logical flag if the series strictly increases or decreases.
#'   - **rank**: Rank of the disaggregate within its outcome by descending CAGR.
#'   - **rank_among_outcomes**: Global CAGR rank across all outcome/disaggregate groups.
#' @import data.table
#' @importFrom stats IQR coef sd 
#' @export
analyze_and_summarize_time_series <- function(dt, threshold = 10) {
  dt <- data.table::as.data.table(dt)
  
  # Compute global CAGR ranks across combinations
  global_summ <- dt[order(commodity_year), .(
    init_val   = value[1],
    final_val  = value[.N],
    start_year = commodity_year[1],
    end_year   = commodity_year[.N],
    cagr       = ((value[.N] / value[1]) ^ (1 / (commodity_year[.N] - commodity_year[1])) - 1) * 100
  ), by = .(outcome, disaggregate)]
  global_summ <- global_summ[order(-cagr), rank_among_outcomes := .I]
  
  analyze_one <- function(subdt) {
    # Summarize core stats by outcome + disaggregate
    summ <- subdt[order(commodity_year), .(
      mean                    = mean(value, na.rm = TRUE),
      sd                      = sd(value,   na.rm = TRUE),
      median                  = median(value, na.rm = TRUE),
      iqr                     = IQR(value,  na.rm = TRUE),
      cv                      = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE),
      n_obs                   = sum(!is.na(value)),
      total_years             = commodity_year[.N] - commodity_year[1] + 1,
      pct_missing             = ((commodity_year[.N] - commodity_year[1] + 1) - sum(!is.na(value))) /
        (commodity_year[.N] - commodity_year[1] + 1) * 100,
      year_with_max_value     = commodity_year[which.max(value)],
      max_value               = max(value, na.rm = TRUE),
      year_with_min_value     = commodity_year[which.min(value)],
      min_value               = min(value, na.rm = TRUE),
      initial_year_value      = value[1],
      final_year_value        = value[.N],
      cumulative_change       = value[.N] - value[1],
      mean_annual_growth_rate = ((value[.N] / value[1]) ^ (1 / (commodity_year[.N] - commodity_year[1])) - 1) * 100,
      growth_initial_to_final = (value[.N] - value[1]) / value[1] * 100
    ), by = .(outcome, disaggregate)]
    
    # Rank by mean within each outcome, breaking ties by disaggregate name descending
    summ <- summ[order(-mean, -xtfrm(disaggregate))]
    summ[, rank := .I]
    # Merge in global rank
    summ <- merge(summ, global_summ, by = c("outcome", "disaggregate"), all.x = TRUE, sort = FALSE)
    
    out_list <- lapply(seq_len(nrow(summ)), function(i) {
      row <- summ[i]
      lvl <- row$disaggregate
      dt_lvl <- subdt[disaggregate == lvl][order(commodity_year)]
      dt_lvl[, pct_change := 100 * (value / shift(value) - 1)]
      
      # Volatility & drawdown
      first_diff_sd    <- dt_lvl[, sd(pct_change, na.rm = TRUE)]
      max_drawdown_pct <- max((dt_lvl[, cummax(value)] - dt_lvl$value) / dt_lvl[, cummax(value)] * 100, na.rm = TRUE)
      
      # Notable growth (magnitude)
      growth_dt <- dt_lvl[pct_change >= threshold, .(year = commodity_year, change = pct_change)]
      growth_dt <- growth_dt[order(-change)]
      notable_growth       <- if (nrow(growth_dt) > 0) paste0(
        growth_dt[, paste0(year, ": +", sprintf("%.1f", change), "%")], collapse = "; "
      ) else NA_character_
      # Chronological
      notable_growth_chron <- if (nrow(growth_dt) > 0) paste0(
        growth_dt[order(year), paste0(year, ": +", sprintf("%.1f", change), "%")], collapse = "; "
      ) else NA_character_
      
      # Notable decline (magnitude)
      decline_dt <- dt_lvl[pct_change <= -threshold, .(year = commodity_year, change = pct_change)]
      decline_dt <- decline_dt[order(-abs(change))]
      notable_decline       <- if (nrow(decline_dt) > 0) paste0(
        decline_dt[, paste0(year, ": ", sprintf("%.1f", change), "%")], collapse = "; "
      ) else NA_character_
      # Chronological
      notable_decline_chron <- if (nrow(decline_dt) > 0) paste0(
        decline_dt[order(year), paste0(year, ": ", sprintf("%.1f", change), "%")], collapse = "; "
      ) else NA_character_
      
      # Flags and trends
      any_missing_years <- row$total_years != row$n_obs
      diffs <- dt_lvl[, diff(value)]
      monotonic <- all(diffs >= 0, na.rm = TRUE) || all(diffs <= 0, na.rm = TRUE)
      tm <- lm(value ~ commodity_year, data = dt_lvl)
      trend_slope     <- coef(tm)[2]
      trend_p_value   <- summary(tm)$coefficients[2, 4]
      trend_r_squared <- summary(tm)$r.squared
      time_to_peak   <- row$year_with_max_value - dt_lvl$commodity_year[1]
      time_to_trough <- row$year_with_min_value - dt_lvl$commodity_year[1]
      
      data.frame(
        mean                    = row$mean,
        sd                      = row$sd,
        median                  = row$median,
        iqr                     = row$iqr,
        cv                      = row$cv,
        n_obs                   = row$n_obs,
        pct_missing             = row$pct_missing,
        initial_year_value      = row$initial_year_value,
        final_year_value        = row$final_year_value,
        year_with_max_value     = row$year_with_max_value,
        max_value               = row$max_value,
        year_with_min_value     = row$year_with_min_value,
        min_value               = row$min_value,
        cumulative_change       = row$cumulative_change,
        mean_annual_growth_rate = row$mean_annual_growth_rate,
        growth_initial_to_final = row$growth_initial_to_final,
        first_diff_sd           = first_diff_sd,
        max_drawdown_pct        = max_drawdown_pct,
        trend_slope             = trend_slope,
        trend_p_value           = trend_p_value,
        trend_r_squared         = trend_r_squared,
        time_to_peak            = time_to_peak,
        time_to_trough          = time_to_trough,
        notable_growth          = notable_growth,
        notable_growth_chron    = notable_growth_chron,
        notable_decline         = notable_decline,
        notable_decline_chron   = notable_decline_chron,
        any_missing_years       = any_missing_years,
        monotonic               = monotonic,
        rank                    = row$rank,
        rank_among_outcomes     = row$rank_among_outcomes,
        stringsAsFactors        = FALSE
      )
    })
    names(out_list) <- tolower(summ$disaggregate)
    out_list
  }
  
  # Apply to each outcome
  result <- lapply(unique(dt$outcome), function(o) analyze_one(dt[outcome == o]))
  names(result) <- unique(dt$outcome)
  result
}


