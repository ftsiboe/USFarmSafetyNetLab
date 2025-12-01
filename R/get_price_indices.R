#' Build a price-received deflator (PPIPR) series relative to `current_year`
#'
#' @description
#' Constructs a table used to deflate nominal FCIP monetary amounts to a common
#' base year. Returns two columns, `commodity_year` and `PPIPR`, where `PPIPR`
#' equals the year's price-received index divided by the index in `current_year`
#' (so `PPIPR(current_year) == 1`).
#'
#' @details
#' **Data sources (from `rfcipDemand`):**
#' - `nassSurveyPriceRecivedIndex` (annual; expects `commodity_year`, `index_for_price_recived`).
#' - `nassAgPriceMonthlyIndex` (monthly U.S. agricultural price index; expects
#'   `year`, `comm`, `index`).
#'
#' **Synthesizing the current year (if missing in the annual table):**
#' - Compute the arithmetic mean of the monthly index where `comm == "Agricultural"` for
#'   both `current_year` and `current_year - 1`.
#' - Multiply last year's annual `index_for_price_recived` by the ratio
#'   `mean_monthly(current_year) / mean_monthly(current_year - 1)` to derive the
#'   current-year annual index.
#' - Append this row with `data_source = "calculated"`.
#'
#' **Normalization:**
#' - Let the denominator be the (mean) `index_for_price_recived` among rows with
#'   `commodity_year == current_year` (provides stability if duplicates exist).
#' - Define `PPIPR = index_for_price_recived / denominator`.
#'
#' **Output shape:**
#' - Returns only `commodity_year` and `PPIPR`, sorted ascending by `commodity_year`.
#' - If the input annual table contains multiple rows per year, duplicates are preserved
#'   in the output (each with its own `PPIPR`). Aggregate if you require strictly one
#'   row per year (see Notes).
#'
#' @param current_year Integer scalar. The base year used for normalization.
#'   The returned `PPIPR` equals 1 for this year.
#'
#' @return A `data.table` with two columns:
#' \itemize{
#'   \item `commodity_year` - integer year.
#'   \item `PPIPR` - numeric deflator equal to the year's price-received index divided by
#'         the `current_year` index.
#' }
#'
#' @section Assumptions & Notes:
#' - Assumes both reference datasets from **rfcipDemand** are available with the
#'   specified columns (including the source's spelling `index_for_price_recived`).
#' - Monthly means are computed with `na.rm = TRUE`.
#' - If you need one row per year, post-aggregate:
#'   `dt[, .(PPIPR = mean(PPIPR, na.rm = TRUE)), by = commodity_year]`.
#'
#' @import data.table
#' @export
get_price_indices <- function(current_year = NULL){
  
  temporary_dir <- tempdir()
  piggyback::pb_download(
    file = "nassSurveyPriceRecivedIndex.rds",
    dest = temporary_dir,
    repo = "ftsiboe/USFarmSafetyNetLab",
    tag  = "nass_extracts",
    overwrite = TRUE)
  annual <- readRDS(file.path(temporary_dir,"nassSurveyPriceRecivedIndex.rds"))
  data.table::setDT(annual)
  
  piggyback::pb_download(
    file = "nassAgPriceMonthlyIndex.rds",
    dest = temporary_dir,
    repo = "ftsiboe/USFarmSafetyNetLab",
    tag  = "nass_extracts",
    overwrite = TRUE)
  monthly <- readRDS(file.path(temporary_dir,"nassAgPriceMonthlyIndex.rds"))
  data.table::setDT(monthly)
  
  if(is.null(current_year)){
    current_year <- max(annual$commodity_year,na.rm=T)
  }
  
  # Ensure expected columns exist (fail fast, clear message)
  for (col in c("commodity_year", "index_for_price_recived")) {
    if (!col %in% names(annual)) stop("`nass_index_for_price_recived` missing column: ", col)
  }
  for (col in c("year", "comm", "index")) {
    if (!col %in% names(monthly)) stop("`nass_us_ag_price_index_monthly` missing column: ", col)
  }
  
  price_indices <- data.table::copy(annual)
  
  if (! current_year %in% unique(price_indices[["commodity_year"]])) {
    # Compute monthly means for current and previous year (Agricultural only)
    m_curr <- monthly[year %in% current_year     & comm %in% "Agricultural", mean(index, na.rm = TRUE)]
    m_prev <- monthly[year %in% (current_year-1) & comm %in% "Agricultural", mean(index, na.rm = TRUE)]
    
    # Guard empty subsets / NaN / non-finite
    if (!is.finite(m_curr) || !is.finite(m_prev) || m_prev == 0) {
      stop("Cannot synthesize current_year index: monthly series missing or invalid for years ",
           current_year-1, " and/or ", current_year, ".")
    }
    
    # Find last year's annual index (aggregate if duplicates)
    last_idx <- price_indices[commodity_year %in% (current_year-1),
                              mean(index_for_price_recived, na.rm = TRUE)]
    if (!is.finite(last_idx)) {
      stop("Missing or invalid `index_for_price_recived` for year ", current_year-1, ".")
    }
    
    price_index_current <- (m_curr / m_prev) * last_idx
    
    # Append synthesized row (namespace data.table constructor)
    price_indices <- rbind(
      price_indices,
      data.table::data.table(
        commodity_year = current_year,
        index_for_price_recived = price_index_current,
        data_source = "calculated"
      ),
      use.names = TRUE, fill = TRUE
    )
  }
  
  # Compute PPIPR relative to current_year (aggregate denominator in case of duplicates)
  denom <- price_indices[commodity_year %in% current_year,
                         mean(index_for_price_recived, na.rm = TRUE)]
  if (!is.finite(denom) || denom == 0) {
    stop("Invalid denominator for PPIPR in current_year: not finite or zero.")
  }
  
  price_indices[, PPIPR := index_for_price_recived / denom]
  price_indices <- price_indices[, .(commodity_year, PPIPR)]
  price_indices <- price_indices[order(commodity_year)]
  
  price_indices
}

