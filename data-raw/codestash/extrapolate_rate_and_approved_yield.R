#' Extrapolate Rate and Approved Yield from Base Premiums
#'
#' Given a `data.table` with FCIP insurance fields and RMA rate components, this
#' function reconstructs the underlying `rate_yield` by inverting the premium-rate
#' pipeline and computes an `approved_yield`, adding both as new columns named
#' `rate_yield` and `approved_yield`.
#'
#' @param dt data.table. Must contain numeric columns:
#' - All fields in `rfcipCalcPass::FCIP_INSURANCE_POOL` 
#' - All fields in `rfcipCalcPass::FCIP_INSURANCE_ELECTION`
#' - liability_amount_per_acre = liability_amount/total_insured_acres
#' - total_premium_amount_per_acre  = total_premium_amount/total_insured_acres 
#' - projected_price Numeric. RMA projected price
#' - harvest_price Numeric. RMA harvest price
#' - reference_amount Numeric. Current-year reference yield used to compute yield ratio.
#' - reference_rate Numeric. Rate used in base rate calculation for current year.
#' - fixed_rate Numeric. Fixed rate added after exponentiation for current year.
#' - rate_differential_factor Numeric. Differential factor applied to current base rate.
#' - unit_residual_factor Numeric. Unit residual factor applied to current base rate.
#' - exponent_value Numeric. Exponent value for current-year yield ratio.
#' - prior_year_reference_amount Numeric. Prior-year reference amount.
#' - prior_year_reference_rate Numeric. Prior-year reference rate.
#' - prior_year_fixed_rate Numeric. Prior-year fixed rate.
#' - prior_year_rate_differential_factor Numeric. Prior-year differential factor.
#' - prior_year_unit_residual_factor Numeric. Prior-year unit residual factor.
#' - prior_year_exponent_value Numeric. Exponent value for prior-year yield ratio.
#' @param farm_identifiers Character vector. Column name(s) that uniquely identify each record. If `NULL`, a temporary `producer_id` is created.
#' @param tol Numeric. Tolerance for matching inverted branches (default: 0.01, i.e. 1 cent per dollar drift).
#' @param rate_yield_to_approved_yield_factor Numeric. Factor (<1) to scale `approved_yield` when `rate_yield` inversion fails (default: 0.9), leveraging that `approved_yield` is always greater of equal to `rate_yield`.
#'
#' @family Yield/Revenue Calibration
#' @return `data.table` with new columns including `rate_yield` and `approved_yield`.
#'
#' @references
#' Tsiboe, Francis, Dylan Turner, and Jisang Yu. (2025) [Utilizing large-scale insurance data sets to calibrate sub-county level crop yields](https://onlinelibrary.wiley.com/doi/10.1111/jori.12494). Journal of Risk and Insurance, 92(1), 139-165.
#'
#' Coble, K. H., Knight, T. O., Goodwin, B. K., Miller, M. F., Rejesus, R. M., & Duffield, G. (2010). [A comprehensive review of the RMA APH and COMBO rating methodology final report.](https://www.rma.usda.gov/sites/default/files/topics/comprehensivereview.pdf)
#' 
#' @import data.table
#' @export
extrapolate_rate_and_approved_yield <- function(
    dt,
    farm_identifiers = NULL,
    tol = 0.01,
    rate_yield_to_approved_yield_factor = 0.9){
  # -- input checks
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.")
  }
  # -- add identifier if missing
  if (is.null(farm_identifiers)) {
    dt[, producer_id := .I]
    farm_identifiers <- "producer_id"
  }
  # -- required columns
  required <- c(
    "insurance_plan_code", "liability_amount_per_acre", "total_premium_amount_per_acre",
    "coverage_level_percent", "projected_price", "harvest_price",
    "reference_amount", "reference_rate", "fixed_rate",
    "rate_differential_factor", "unit_residual_factor", "exponent_value",
    "prior_year_reference_amount", "prior_year_reference_rate",
    "prior_year_fixed_rate", "prior_year_rate_differential_factor",
    "prior_year_unit_residual_factor", "prior_year_exponent_value"
  )
  miss <- setdiff(required, names(dt))
  if (length(miss)) {
    stop("Missing columns: ", paste(miss, collapse = ", "))
  }
  
  # -- helper to cup-and-cap a yield ratio
  cap <- function(r) pmin(pmax(r, 0.5), 1.5)
  
  #  -- Adjustments to gross revenue protection policies to be equivalent to their corresponding yield
  #protection policies, as is done by RMA (Coble et al., 2010).
  # https://legacy.rma.usda.gov/pubs/2009/comprehensivereview.pdf
  dt[, liability_amount_adj := fifelse(
    insurance_plan_code %in% c(2,3),
    liability_amount_per_acre * (projected_price / harvest_price),
    liability_amount_per_acre
  )]
  
  # -- compute adjusted base premium rate per acre
  dt[, base_premium_rate_adj := total_premium_amount_per_acre / liability_amount_adj]
  
  # -- compute approved yield per acre
  dt[, approved_yield := liability_amount_adj/(coverage_level_percent * projected_price)]
  
  # -- invert pipeline to get rate_yield
  # For each row, we have bpr = adjusted base premium rate per acre
  # We now reverse the calculation branch-by-branch:
  dt[, c(
    "current_base_rate",
    "prior_year_base_rate",
    "current_rate_yield",
    "prior_year_rate_yield",
    "rate_yield"
  ) := {
    bpr <- base_premium_rate_adj
    
    # 1. Current-year branch inversion:
    #   Mc = base_premium_rate_adj / (rate_differential_factor * unit_residual_factor)
    #     => isolate (base_rate_multiplier)
    #   base_rate = (Mc - fixed_rate) / reference_rate
    #     => revert the additive 'fixed_rate' then divide by reference_rate
    #   ratio_c = cap(base_rate^(1/exponent_value))
    #     => undo exponentiation, then cup-and-cap to [0.5,1.5]
    #   ry_c = ratio_c * reference_amount
    #     => back out the original rate_yield
    Mc <- base_premium_rate_adj / (rate_differential_factor * unit_residual_factor)
    br_c <- (Mc - fixed_rate) / reference_rate
    ratio_c <- cap(br_c^(1 / exponent_value))
    ry_c <- ratio_c * reference_amount
    
    # 2. Prior-year branch inversion:
    #   Mpy = base_premium_rate_adj / (prior_year_rate_differential_factor * prior_year_unit_residual_factor)
    #   base_rate_py = (Mpy - prior_year_fixed_rate) / prior_year_reference_rate
    #   ratio_py = cap(base_rate_py^(1/prior_year_exponent_value))
    #   ry_py = ratio_py * prior_year_reference_amount
    Mpy <- base_premium_rate_adj / (prior_year_rate_differential_factor * prior_year_unit_residual_factor)
    br_py <- (Mpy - prior_year_fixed_rate) / prior_year_reference_rate
    ratio_py <- cap(br_py^(1 / prior_year_exponent_value))
    ry_py <- ratio_py * prior_year_reference_amount
    
    # 3. Recompute both branch's forward premiums to see which matches observed base_premium_rate_adj:
    #   cbpr = (ratio_c^exponent_value * reference_rate + fixed_rate) * rate_differential_factor * unit_residual_factor
    #   pbpr = (ratio_py^prior_year_exponent_value * prior_year_reference_rate + prior_year_fixed_rate) * prior_year_rate_differential_factor * prior_year_unit_residual_factor * 1.2
    cbpr <- (ratio_c^exponent_value * reference_rate + fixed_rate) *
      rate_differential_factor * unit_residual_factor
    pbpr <- (ratio_py^prior_year_exponent_value * prior_year_reference_rate + prior_year_fixed_rate) *
      prior_year_rate_differential_factor * prior_year_unit_residual_factor * 1.2
    
    # 4. Safe branch selection
    cond_c   <- !is.na(bpr) && !is.na(cbpr) && abs(bpr - cbpr) < tol
    cond_py  <- !is.na(bpr) && !is.na(pbpr) && abs(bpr - pbpr) < tol
    cond_cap <- !is.na(bpr) && abs(bpr - 0.999) < tol
    
    # 5. Select the branch whose recomputed premium matches base_premium_rate_adj within tol:
    final_ry <- if (cond_c) {
      ry_c
    } else if (cond_py) {
      ry_py
    } else if (cond_cap) {
      NA_real_
    } else {
      warning(
        sprintf(
          "Branch mismatch for base_premium_rate_adj=%.8f: fallback to approved_yield*factor", bpr
        )
      )
      approved_yield * rate_yield_to_approved_yield_factor
    }
    list(cbpr, pbpr, ry_c, ry_py, final_ry)
  }, by = farm_identifiers]
  
  # fallback to approved_yield*rate_yield_to_approved_yield_factor
  dt[, rate_yield := ifelse(rate_yield %in% c(0,NA,Inf,NaN,-Inf), approved_yield * rate_yield_to_approved_yield_factor,rate_yield)]
  
  # -- ensure approved >= rate_yield
  dt[rate_yield > approved_yield, approved_yield := rate_yield]
  
  return(dt)
}