#' Build a state-level **revealed** Plan of Operations from historical SRA data
#'
#' Pulls public SRA history from \code{rfcip} (both \code{nationalSRA} and
#' \code{stateSRA}), normalizes fund labels, collapses to one row per
#' \code{state}-\code{commodity_year}-\code{fund}, and returns a wide, state-year
#' table with (i) each fund's share of the state book and (ii) the average
#' premium-retention percentage retained inside each fund.
#'
#' The result is a ready-made \code{aip_state_operation}-style table that can be
#' used by simulation helpers such as \code{fund_alocation_and_retention()}.
#'
#' @details
#' Steps performed:
#' \enumerate{
#'   \item Bind the national aggregate (tagged as \code{state == "US"}) to state-level SRA records.
#'   \item Drop roll-up rows (\code{fund_abb == "Total"}).
#'   \item Map FCIC fund abbreviations to \code{arf}, \code{cf}, \code{df}:
#'         \code{CC/OC/RC/C -> cf}, \code{CD/OD/RD -> df}, \code{AR/OA/A -> arf}.
#'   \item Keep premium fields and set \code{commodity_year <- reinsurance_year}.
#'   \item Sum dollars by \code{state} \eqn{\times} \code{commodity_year} \eqn{\times}
#'         \code{fund_abb} \eqn{\times} \code{value_type}.
#'   \item Reshape wide by \code{value_type} to get \code{gross_premium} and \code{retained_premium};
#'         compute:
#'         \deqn{share = \frac{\text{gross\_premium}}{\sum_{\text{fund}} \text{gross\_premium}}}
#'         \deqn{retention = \frac{\text{retained\_premium}}{\text{gross\_premium}}}
#'   \item Average across multiple company records within the same state-fund-year.
#'   \item Reshape to a wide, state-year layout with fund-prefixed columns.
#' }
#'
#' Notes:
#' \itemize{
#'   \item Missing values are handled with \code{na.rm = TRUE} in sums/means.
#'   \item Keys in the final wide table are \code{state} and \code{commodity_year}.
#'   \item The output includes \strong{shares} and \strong{retention} plus additional
#'         fund-prefixed aggregates (e.g., gross/retained premium and indemnity).
#' }
#'
#' @return A \code{data.table} with one row per \code{state}-\code{commodity_year} and columns:
#' \itemize{
#'   \item \code{state} - two-letter postal code; \code{"US"} for the national aggregate.
#'   \item \code{commodity_year} - FCIC reinsurance year (treated as commodity year).
#'   \item Fund-share and retention columns:
#'         \code{arf_share}, \code{cf_share}, \code{df_share};
#'         \code{arf_retention}, \code{cf_retention}, \code{df_retention}.
#'   \item Additional fund-prefixed metrics (averaged over companies where applicable):
#'         \code{<fund>_gross_indemnity}, \code{<fund>_gross_liability},
#'         \code{<fund>_gross_premium}, \code{<fund>_net_gain_loss},
#'         \code{<fund>_retained_indemnity}, \code{<fund>_retained_liability},
#'         \code{<fund>_retained_premium}, where \code{<fund> \%in\% c("arf","cf","df")}.
#' }
#'
#' @section Dependencies:
#' Requires \pkg{rfcip} for \code{nationalSRA} and \code{stateSRA}; uses
#' \pkg{data.table}, \pkg{dplyr}, and \pkg{tidyr} for aggregation and reshaping.
#'
#' @examples
#' \dontrun{
#' dt <- revealed_aip_state_operation()
#' dt[state == "IA" & commodity_year >= 2018,
#'    .(IA_arf_share = arf_share, IA_cf_share = cf_share, IA_df_share = df_share)]
#' }
#'
#' @family FCIP Re-insurance Calculators
#' @import data.table
#' @export
revealed_aip_state_operation <- function(){

  state_sra <- data.table::as.data.table(rfcip::nationalSRA)
  state_sra[,state := "US"]
  state_sra <- rbind(state_sra,data.table::as.data.table(rfcip::stateSRA))

  # drop roll-ups
  state_sra <- state_sra[!fund_abb %in% "Total"]

  #  normalise fund abbreviations

  state_sra[,fund_abb := ifelse(fund_abb %in% c("CC","OC","RC","C" ),"cf",fund_abb)]
  state_sra[,fund_abb := ifelse(fund_abb %in% c("CD","OD","RD"),"df",fund_abb)]
  state_sra[,fund_abb := ifelse(fund_abb %in% c("AR","OA","A"),"arf",fund_abb)]

  # keep premium fields, sum to state-fund-year
  state_sra[,commodity_year := reinsurance_year]
  state_sra <- state_sra[
    ,
    lapply(.SD, sum, na.rm = TRUE),
    by = .(state, commodity_year, fund_abb, value_type),
    .SDcols = "dollars"]

  # reshape wide, compute shares & retention
  state_sra <- state_sra |>
    tidyr::spread(value_type, dollars) |>
    dplyr::group_by(state, commodity_year) |>
    dplyr::mutate(state_total_gross_premium =
                    sum(gross_premium, na.rm = TRUE)) |>
    data.table::as.data.table()

  state_sra[ , share     := gross_premium     / state_total_gross_premium]
  state_sra[ , retention := retained_premium  / gross_premium]

  # average across multiple companies in same state-fund-year
  state_sra <- state_sra[
    ,
    lapply(.SD, mean, na.rm = TRUE),
    by = .(state, commodity_year, fund_abb),
    .SDcols = c("share","retention","gross_indemnity","gross_liability","gross_premium",
                "net_gain_loss","retained_indemnity","retained_liability","retained_premium")]

  # final wide layout: arf_share, cf_retention
  state_sra <- state_sra |>
    tidyr::gather(
      type, value, c("share","retention","gross_indemnity","gross_liability","gross_premium",
                     "net_gain_loss","retained_indemnity","retained_liability","retained_premium"))

  state_sra <- as.data.table(state_sra)
  state_sra[,type := paste0(fund_abb,"_",type)]

  state_sra <- state_sra[
    , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
    by = c("state","commodity_year","type"),
    .SDcols = c("value")] |>
    tidyr::spread(type, value)

  state_sra <- data.table::as.data.table(state_sra)

  return(state_sra)

}


