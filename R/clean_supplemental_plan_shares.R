#' SCO share by coverage level
#'
#' @description
#' Computes the Supplemental Coverage Option (SCO) **share of insured acres** by
#' coverage level for base plans (1=YP, 2=RP, 3=RP-HPE), using SCO plan codes
#' 31-33 mapped back to 1-3.
#'
#' @param sob A `data.table` (or data.frame) of SOB records that already
#'   contain aggregated acre and dollar fields. Must include at least:
#'   `insured_acres`, `endorsed_acres`, `insurance_plan_code`,
#'   `coverage_level_percent`, `commodity_year`, `state_code`, `county_code`,
#'   `commodity_code`, `type_code`, `practice_code`.
#'
#' @details
#' - Base data are summed for plans `1:3`.
#' - SCO records are selected via plan codes `31:33` and then remapped to
#'   `1:3` (subtract 30) to align with corresponding base plans.
#' - SCO share is computed as `endorsed_acres / insured_acres` by key and
#'   coverage level.
#'
#' @return A `data.table` with unique rows by
#'   `commodity_year, state_code, county_code, commodity_code, type_code,
#'   practice_code, insurance_plan_code, coverage_level_percent` and column:
#'   `sco` (share in \[0,1\]).
#'
#' @import data.table
#' @export
clean_sco_share_by_coverage_level <- function(sob){

  # Base data aggregation
  data <- sob[
    insurance_plan_code %in% 1:3,
    .(
      insured_acres        = sum(insured_acres,   na.rm = TRUE),
      liability_amount     = sum(liability_amount,             na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount,         na.rm = TRUE),
      subsidy_amount       = sum(subsidy_amount,               na.rm = TRUE),
      indemnity_amount     = sum(indemnity_amount,             na.rm = TRUE)
    ),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "unit_structure_code", "insurance_plan_code", "coverage_type_code", "coverage_level_percent")
  ][
    is.finite(insured_acres) & insured_acres > 0
  ]

  # SCO share by coverage level
  sco_data <- sob[
    insurance_plan_code %in% c(31:33),
    .(sco = sum(endorsed_acres, na.rm = TRUE)),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "insurance_plan_code", "coverage_level_percent")
  ][
    , insurance_plan_code := insurance_plan_code - 30
  ][
    data[, .(insured_acres = sum(insured_acres, na.rm = TRUE)),
         by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
                "insurance_plan_code", "coverage_level_percent")],
    on = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "insurance_plan_code", "coverage_level_percent"),
    nomatch = 0
  ][
    , sco := fifelse(insured_acres > 0, sco/insured_acres, NA_real_)
  ]

  sco_data <- unique(
    sco_data[, c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
                 "insurance_plan_code", "coverage_level_percent", "sco"), with = FALSE]
  )

  return(sco_data)
}

#' ECO share by insurance plan
#'
#' @description
#' Computes Enhanced Coverage Option (ECO) **shares of insured acres** for
#' base plans (1=YP, 2=RP, 3=RP-HPE), using ECO plan codes 87-89 mapped back
#' to 1-3, and returns separate shares for ECO-90 and ECO-95.
#'
#' @param sob A `data.table` (or data.frame) of SOB records that already
#'   contain aggregated acre and dollar fields. Must include at least:
#'   `insured_acres`, `endorsed_acres`, `insurance_plan_code`,
#'   `coverage_level_percent`, `commodity_year`, `state_code`, `county_code`,
#'   `commodity_code`, `type_code`, `practice_code`.
#'
#' @details
#' - Base data are summed for plans `1:3`.
#' - ECO records are selected via plan codes `87:89`, remapped to `1:3`
#'   (subtract 86), and coverage levels are labeled as `"eco90"` / `"eco95"`
#'   (using `coverage_level_percent` * 100).
#' - ECO shares are computed as `eco_endorsed_acres / insured_acres` by key.
#'
#' @return A `data.table` with unique rows by
#'   `commodity_year, state_code, county_code, commodity_code, type_code,
#'   practice_code, insurance_plan_code` and columns:
#'   `eco90`, `eco95` (shares in \[0,1\]); missing combinations may be `NA`.
#'
#' @import data.table
#' @export
clean_eco_share_by_insurance_plan <- function(sob){

  # Base data aggregation
  data <- sob[
    insurance_plan_code %in% 1:3,
    .(
      insured_acres        = sum(insured_acres,   na.rm = TRUE),
      liability_amount     = sum(liability_amount,             na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount,         na.rm = TRUE),
      subsidy_amount       = sum(subsidy_amount,               na.rm = TRUE),
      indemnity_amount     = sum(indemnity_amount,             na.rm = TRUE)
    ),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "unit_structure_code", "insurance_plan_code", "coverage_type_code", "coverage_level_percent")
  ][
    is.finite(insured_acres) & insured_acres > 0
  ]

  # ECO shares
  eco_data <- sob[
    insurance_plan_code %in% c(87:89),
    .(eco = sum(endorsed_acres, na.rm = TRUE)),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "insurance_plan_code", "coverage_level_percent")
  ][
    , insurance_plan_code := insurance_plan_code - 86
  ][
    , coverage_level_percent := paste0("eco", round(coverage_level_percent * 100))
  ][
    !eco %in% c(0, NA, NaN, Inf, -Inf)
  ] |> tidyr::spread(coverage_level_percent, eco)

  eco_data <- as.data.table(eco_data)[
    data[, .(insured_acres = sum(insured_acres, na.rm = TRUE)),
         by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code", "insurance_plan_code")],
    on = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code", "insurance_plan_code"),
    nomatch = 0
  ][
    , eco90 := fifelse(insured_acres > 0, eco90/insured_acres, NA_real_)
  ][
    , eco95 := fifelse(insured_acres > 0, eco95/insured_acres, NA_real_)
  ]

  eco_data <- unique(
    eco_data[, c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
                 "insurance_plan_code", "eco90", "eco95"), with = FALSE]
  )

  return(eco_data)
}


#' Supplemental plan shares (SCO/ECO)
#'
#' @description
#' Aggregates base plan acres/dollars and (optionally) merges in
#' Supplemental Coverage Option (SCO) and Enhanced Coverage Option (ECO)
#' shares for each key.
#'
#' @param sob A `data.table` or `data.frame` of SOB records. Expected columns:
#'   `insured_acres`, `endorsed_acres`, `insurance_plan_code`,
#'   `coverage_level_percent`, `commodity_year`, `state_code`, `county_code`,
#'   `commodity_code`, `type_code`, `practice_code`, `unit_structure_code`,
#'   `coverage_type_code`, `liability_amount`, `total_premium_amount`,
#'   `subsidy_amount`, `indemnity_amount`.
#' @param get_sco_shares Logical; if `TRUE`, merge SCO shares by coverage level.
#' @param get_eco_shares Logical; if `TRUE`, merge ECO-90/95 shares by plan.
#'
#' @details
#' Assumes base plans are harmonized to 1=YP, 2=RP, 3=RP-HPE, and
#' `coverage_level_percent` is in decimals (e.g., 0.90, 0.95).
#' Relies on helpers: `clean_sco_share_by_coverage_level()` and
#' `clean_eco_share_by_insurance_plan()`.
#'
#' @return A `data.frame` with aggregated acres/dollars and columns
#'   `sco`, `eco90`, `eco95` (shares in \[0,1\] where available).
#'
#' @import data.table
#' @export
clean_supplemental_plan_shares <- function(
    sob,
    get_sco_shares = TRUE,
    get_eco_shares = TRUE){

  data <- sob[
    insurance_plan_code %in% 1:3,
    .(
      insured_acres        = sum(insured_acres,   na.rm = TRUE),
      liability_amount     = sum(liability_amount,             na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount,         na.rm = TRUE),
      subsidy_amount       = sum(subsidy_amount,               na.rm = TRUE),
      indemnity_amount     = sum(indemnity_amount,             na.rm = TRUE)
    ),
    by = c("commodity_year", "state_code", "county_code", "commodity_code", "type_code", "practice_code",
           "unit_structure_code", "insurance_plan_code", "coverage_type_code", "coverage_level_percent")
  ][
    is.finite(insured_acres) & insured_acres > 0
  ]

  if(isTRUE(get_sco_shares)){
    sco_data <- clean_sco_share_by_coverage_level(sob)
    data <- merge(data, sco_data, by = intersect(names(data), names(sco_data)), all = TRUE)
  }

  if(isTRUE(get_eco_shares)){
    eco_data <- clean_eco_share_by_insurance_plan(sob)
    data <- merge(data, eco_data, by = intersect(names(data), names(eco_data)), all = TRUE)
  }

  for (xx in c("sco","eco90","eco95")) {
    if (!xx %in% names(data)) next
    # set non-finite to NA first
    data[!is.finite(get(xx)), (xx) := NA_real_]
    # clamp to [0,1]
    data[get(xx) < 0, (xx) := 0]
    data[get(xx) > 1, (xx) := 1]
    # if you truly want NA to 0:
    data[is.na(get(xx)), (xx) := 0]
  }

  data <- data[is.finite(insured_acres) & insured_acres > 0]
  return(data)
}
