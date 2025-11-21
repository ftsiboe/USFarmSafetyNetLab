#' Compute continuous rating parameters from ADM data for a given year
#'
#' Pulls ADM datasets A01010_BaseRate (Base Rate) and
#' A01040_CoverageLevelDifferential (Coverage Level Differential), harmonizes
#' plan codes (90 to 1, 44 to 2, 25/42 to 3), aggregates base quantities, fits a
#' commodity-level regression to estimate missing coverage-level differentials,
#' and returns per-(state, county, commodity, type, practice, plan) records
#' with:
#'   - Ycr (reference amount), Expo (exponent), Rr (fixed rate), Rf (reference rate)
#'   - PF (ratio Rr / max Rr within county/commodity/type/plan)
#'   - Rd50..Rd85 (coverage differential factors; Rd65 anchored at 1)
#'   - Fu_EU / Fu_BU (enterprise/basic unit residual factors)
#'
#' @details
#' Coverage-level differentials (Rd*) are modeled, for each commodity (crop),
#' as a multivariate quadratic in coverage level (and its square) and the base
#' fixed rate Rr (and its square), including an interaction with coverage level.
#' The 65% level is excluded from the regression fit and then anchored to 1
#' (i.e., Rd65 = 1) when filling. Coverage levels considered are 50-85%.
#'
#' Rd* values are averaged across insurance plans at the county-commodity level
#' before joining back to the base table; if plan-specific differentials are
#' required, retain `insurance_plan_code` in the differential aggregation.
#'
#' @note
#' PF is computed as \code{Rr / max(Rr)} within (state, county, commodity, type, insurance_plan_code).
#'
#' For observations with missing Rd*, estimated values follow the approach
#' described by Coble et al. (2010), specifying a quadratic function of
#' coverage level and the underlying rate terms, with the 65% level as the
#' normalization point.
#'
#' @references
#' Coble, K. H., Knight, T. O., Goodwin, B. K., Miller, M. F., Rejesus, R. M.,
#' & Duffield, G. (2010). \emph{A Comprehensive Review of the RMA APH and
#' COMBO Rating Methodology: Final Report}. USDA Risk Management Agency.
#' https://www.rma.usda.gov/sites/default/files/topics/comprehensivereview.pdf
#'
#' @param year integer scalar (e.g., 2023)
#' @param harmonize_insurance_plan_code Logical; recode plans to (1,2,3). Default `TRUE`.
#'
#' @return A \code{data.table} with columns:
#'   \itemize{
#'     \item \code{state_code}, \code{county_code}, \code{commodity_code},
#'           \code{type_code}, \code{practice_code}, \code{insurance_plan_code}
#'     \item \code{Ycr}, \code{Expo}, \code{Rr}, \code{Rf}, \code{PF}
#'     \item \code{Rd50}, \code{Rd55}, \code{Rd60}, \code{Rd65},
#'           \code{Rd70}, \code{Rd75}, \code{Rd80}, \code{Rd85}
#'     \item \code{Fu_EU}, \code{Fu_BU}
#'     \item \code{commodity_year}
#'   }
#'
#' @import data.table
#' @importFrom stats lm coef
#' @export
get_continuous_rating_parameters <- function(
    year,harmonize_insurance_plan_code = TRUE) {
  
  temporary_dir <- tempdir()
  
  stopifnot(length(year) == 1L, is.numeric(year), is.finite(year))
  
  # ---- helpers
  to_numeric <- function(DT, cols) {
    keep <- intersect(cols, names(DT))
    if (length(keep)) {
      DT[, (keep) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = keep]
    }
    invisible(NULL)
  }
  
  to_char <- function(DT, cols) {
    keep <- intersect(cols, names(DT))
    if (length(keep)) {
      DT[, (keep) := lapply(.SD, as.character), .SDcols = keep]
    }
    invisible(NULL)
  }
  
  safe_max <- function(v) {
    m <- suppressWarnings(max(v, na.rm = TRUE))
    if (!is.finite(m)) NA_real_ else m
  }
  
  # safe linear model coefs: returns named numeric of length 6 (x0..x5) or NA
  safe_rd_coefs <- function(df) {
    # df must have columns Rd, x1..x5
    out <- tryCatch({
      cf <- stats::coef(stats::lm(Rd ~ x1 + x2 + x3 + x4 + x5, data = df))
      # ensure full set
      nm <- c("(Intercept)", "x1", "x2", "x3", "x4", "x5")
      cf <- cf[nm]
      stats::setNames(as.numeric(cf), c("b0","b1","b2","b3","b4","b5"))
    }, error = function(e) stats::setNames(rep(NA_real_, 6L), c("b0","b1","b2","b3","b4","b5")))
    out
  }
  
  fill_rd <- function(DT) {
    # fill RdXX via polynomial using b0..b5 and Rr when RdXX missing/invalid; Rd65 anchored at 1
    CL <- c(50,55,60,70,75,80,85)
    for (cl in CL) {
      nm <- paste0("Rd", cl)
      DT[ is.na(get(nm)) | !is.finite(get(nm)) | get(nm) == 0, (nm) := b0 + (cl/100)*b1 + (cl/100)^2*b2 + Rr*b3 + (Rr^2)*b4 + (Rr*(cl/100))*b5]
    }
    # enforce anchor at 65
    DT[is.na(Rd65) | !is.finite(Rd65) | Rd65 == 0, Rd65 := 1]
    DT
  }
  
  force_numeric_keys <- c(
    "state_code","county_code","commodity_code","type_code",
    "practice_code","insurance_plan_code","coverage_type_code","coverage_level_percent",
    "reference_rate","fixed_rate","reference_amount","exponent_value",
    "rate_differential_factor","unit_residual_factor","enterprise_unit_residual_factor"
  )
  force_character_keys <- c("unit_structure_code")
  
  # ---- load A01010 (Base Rate)
  if(year >= 2011){
    
    ADM <- data.table::as.data.table(
      rfcip::get_adm_data(year = year, dataset = "A01010_BaseRate", force = TRUE)
    )
    
  }else{
    piggyback::pb_download(
      file = paste0("base_rate_",year,".rds"),
      dest = temporary_dir,
      repo = "ftsiboe/USFarmSafetyNetLab",
      tag  = "adm_legacy",
      overwrite = TRUE)
    ADM <- readRDS(file.path(temporary_dir,paste0("base_rate_",year,".rds")))
    data.table::setDT(ADM)
  }
  
  to_numeric(ADM, force_numeric_keys)
  to_char(ADM, force_character_keys)
  
  # Harmonize plan codes (only if requested)
  # These three plans of insurance are similar but not identical. Some differences are:
  # * CRC bases the insurance guarantee on the higher of the base price or the harvest period price.
  # * IP and standard RA guarantees are determined using the base price, with no adjustment if the price increases.
  # * RA offers up-side price protection like that of CRC as an option but IP does not.
  # * IP limits unit formats to basic units, which include all interest in a crop in a county under identical ownership.
  # * RA is unique in offering coverage on whole farm units, integrating coverage from two to three crops.
  # check <- data[data$ins_plan_ab %in% c("YP","APH","IP","RP-HPE","RPHPE","CRC","RP","RA"),]
  if(isTRUE(harmonize_insurance_plan_code)){
    # APH[90] -> YP[1]
    ADM[insurance_plan_code %in% c(1L, 90L), insurance_plan_code := 1L]
    # CRC[44] -> RP[2]
    ADM[insurance_plan_code %in% c(44L, 2L), insurance_plan_code := 2L]
    # IP[42], RP-HPE[3], [25] -> RP-HPE[3]
    ADM[insurance_plan_code %in% c(25L, 42L, 3L), insurance_plan_code := 3L]
    
    ADM <- ADM[insurance_plan_code %in% 1:3]
  }
  
  ADM <- ADM[, .(
    Ycr = mean(reference_amount,  na.rm = TRUE),
    Expo = mean(exponent_value,   na.rm = TRUE),
    Rr   = mean(fixed_rate,       na.rm = TRUE),
    Rf   = mean(reference_rate,   na.rm = TRUE)
  ), by = .(state_code, county_code, commodity_code, type_code, practice_code, insurance_plan_code)]
  
  # PF: productivity factor vs. county max Rr (guard against all-NA Rr)
  ADM[, Rr_max := safe_max(Rr), by = .(state_code, county_code, commodity_code, type_code, insurance_plan_code)]
  ADM[, PF := Rr / Rr_max]
  ADM <- ADM[, .(state_code, county_code, commodity_code, type_code, practice_code,
                 insurance_plan_code, Ycr, Expo, Rr, Rf, PF)]
  
  # ---- load A01040 (Coverage Level Differential)
  if(year >= 2011){
    Rd <- data.table::as.data.table(
      rfcip::get_adm_data(year = year, dataset = "A01040_CoverageLevelDifferential", force = TRUE)
    )
  }else{
    piggyback::pb_download(
      file = paste0("coverage_level_differential_",year,".rds"),
      dest = temporary_dir,
      repo = "ftsiboe/USFarmSafetyNetLab",
      tag  = "adm_legacy",
      overwrite = TRUE)
    Rd <- readRDS(file.path(temporary_dir,paste0("coverage_level_differential_",year,".rds")))
    data.table::setDT(Rd)
  }
  
  to_numeric(Rd, force_numeric_keys)
  to_char(Rd, force_character_keys)
  
  if(isTRUE(harmonize_insurance_plan_code)){
    # APH[90] -> YP[1]
    Rd[insurance_plan_code %in% c(1L, 90L), insurance_plan_code := 1L]
    # CRC[44] -> RP[2]
    Rd[insurance_plan_code %in% c(44L, 2L), insurance_plan_code := 2L]
    # IP[42], RP-HPE[3], [25] -> RP-HPE[3]
    Rd[insurance_plan_code %in% c(25L, 42L, 3L), insurance_plan_code := 3L]
    
    Rd <- Rd[insurance_plan_code %in% 1:3]
  }
  
  Rd <- Rd[round(coverage_level_percent * 100) <= 85]
  
  # unit residual factors averaged at county/commodity (across plans)
  Fu <- Rd[, .(
    Fu_EU = mean(enterprise_unit_residual_factor, na.rm = TRUE),
    Fu_BU = mean(unit_residual_factor,           na.rm = TRUE)
  ), by = .(commodity_code, state_code, county_code)]
  
  # base rate at PF==1 for each county/commodity/plan
  ADM_base <- ADM[
    PF == 1 & is.finite(Rr),
    .(Rr = mean(Rr, na.rm = TRUE)),
    by = .(commodity_code, state_code, county_code, insurance_plan_code)]
  
  # county/commodity/plan/CL-level differentials (averaged)
  Rd <- Rd[, .(Rd = mean(rate_differential_factor, na.rm = TRUE)),
           by = .(commodity_code, state_code, county_code, insurance_plan_code, coverage_level_percent)]
  
  # attach base Rr for formula terms (x[i] ~ coverage, Rr, Rr^2, interactions)
  Rd <- ADM_base[Rd, on = intersect(names(ADM_base), names(Rd)), nomatch = 0L]
  Rd[, `:=`(
    x1 = coverage_level_percent,
    x2 = coverage_level_percent^2,
    x3 = Rr,
    x4 = Rr^2,
    x5 = Rr * coverage_level_percent
  )]
  
  # fit per-commodity regression (exclude 65% so it can be anchored at 1)
  rd_fit_data <- Rd[round(coverage_level_percent * 100) != 65]
  rd_fit <- rd_fit_data[
    , {
      df <- .SD[!is.na(Rd) & is.finite(Rd), c("Rd","x1","x2","x3","x4","x5")]
      as.list(safe_rd_coefs(df))
    },
    by = .(commodity_code)
  ]
  # replace NA/Inf/0 coefs with 0
  for (b in c("b0","b1","b2","b3","b4","b5")) {
    rd_fit[!is.finite(get(b)) | get(b) == 0, (b) := 0]
  }
  
  # wide Rd50..Rd85 per county/commodity/plan
  Rd[, coverage_level_percent := paste0("Rd", round(coverage_level_percent * 100))]
  Rd <- data.table::dcast(
    Rd,
    state_code + county_code + commodity_code + insurance_plan_code ~ coverage_level_percent,
    value.var = "Rd"
  )
  # bring back base Rr and coefficients (join on commodity only for coefs)
  Rd <- ADM_base[Rd, on = intersect(names(ADM_base), names(Rd)), nomatch = 0L]
  Rd <- Rd[rd_fit, on = "commodity_code", nomatch = 0L]
  
  # fill missing/invalid RdXX via regression; anchor 65 at 1
  Rd <- fill_rd(Rd)
  
  # average Rd* across plans (intended behavior) at county/commodity
  Rd <- Rd[, .(
    Rd50 = mean(Rd50, na.rm = TRUE),
    Rd55 = mean(Rd55, na.rm = TRUE),
    Rd60 = mean(Rd60, na.rm = TRUE),
    Rd65 = mean(Rd65, na.rm = TRUE),
    Rd70 = mean(Rd70, na.rm = TRUE),
    Rd75 = mean(Rd75, na.rm = TRUE),
    Rd80 = mean(Rd80, na.rm = TRUE),
    Rd85 = mean(Rd85, na.rm = TRUE)
  ), by = .(commodity_code, state_code, county_code)]
  
  # ---- final joins
  ADM <- ADM[Rd, on = intersect(names(ADM), names(Rd)), nomatch = 0L]
  ADM <- ADM[Fu, on = intersect(names(ADM), names(Fu)), nomatch = 0L]
  
  # order & stamp
  ADM <- ADM[, .(
    state_code, county_code, commodity_code, type_code, practice_code,
    insurance_plan_code, Ycr, Expo, Rr, Rf, PF,
    Rd50, Rd55, Rd60, Rd65, Rd70, Rd75, Rd80, Rd85, Fu_EU, Fu_BU
  )]
  ADM[, commodity_year := as.integer(year)]
  data.table::setcolorder(ADM, c(
    "state_code","county_code","commodity_code","type_code","practice_code",
    "insurance_plan_code","commodity_year",
    "Ycr","Expo","Rr","Rf","PF",
    "Rd50","Rd55","Rd60","Rd65","Rd70","Rd75","Rd80","Rd85",
    "Fu_EU","Fu_BU"
  ))
  ADM[]
}