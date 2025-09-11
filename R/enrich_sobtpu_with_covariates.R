#' Enrich FCIP table (modular)
#'
#' Convenience wrapper around \code{enrich_sobtpu()} that exposes the same
#' arguments and returns the enriched table. This function exists for
#' user-facing discoverability while deferring all work to
#' \code{enrich_sobtpu()}.
#'
#' @param sob A \code{data.frame} or \code{data.table} with FCIP-like columns.
#'   At minimum, the following columns are required by the modules you enable:
#'   \itemize{
#'     \item \strong{Election}: \code{insurance_plan_code}, \code{coverage_level_percent}, \code{unit_structure_code}
#'     \item \strong{Practice}: \code{commodity_year}, \code{commodity_code}, \code{practice_code}
#'     \item \strong{Commodity}: \code{commodity_year}, \code{commodity_code}, \code{insured_acres}
#'     \item \strong{Loss ratio}: \code{state_code}, \code{county_code}, \code{CROP}, \code{commodity_year}, \code{indemnity_amount}, \code{total_premium_amount}
#'     \item \strong{Program age}: \code{state_code}, \code{county_code}, \code{commodity_code}, \code{commodity_year}
#'     \item \strong{Loss cause}: \code{state_code}, \code{county_code}, \code{commodity_code}, \code{commodity_year}
#'     \item \strong{Location}: \code{state_code}, \code{county_code}
#'     \item \strong{Structural breaks}: \code{commodity_year}
#'     \item \strong{Subsidy bins}: \code{subsidy_amount}, \code{total_premium_amount}
#'   }
#'
#' @param election_grouping Logical. Add plan/product, coverage buckets, and unit
#'   structure columns (\code{PLAN}, \code{RPYP}, \code{COV}, \code{STRUCT}).
#'   Default \code{TRUE}.
#'
#' @param commodity_grouping Logical. Join commodity groupings, normalize
#'   \code{CROP}, and add \code{TOPCROP} (top 12 by insured acres). Default \code{TRUE}.
#'
#' @param practice_grouping Logical. Join irrigation/organic practice recodes
#'   (\code{irrigation_recode}, \code{organic_recode}). Default \code{TRUE}.
#'
#' @param loss_ratio_grouping Logical. Compute cumulative prior-years loss ratio
#'   (\code{cum_lr}) and bins (\code{CLRCAT}, \code{CLRmCAT}) by state-county-crop-year.
#'   Default \code{TRUE}.
#'
#' @param program_age_grouping Logical. Compute program age (span of prior
#'   positive-liability years) and 10/5/4-year bins
#'   (\code{program_age_by_year}, \code{program_age_by_year_10},
#'   \code{program_age_by_year_05}, \code{program_age_by_year_04}).
#'   Default \code{TRUE}.
#'
#' @param loss_cause_grouping Logical. Compute cause-of-loss shares for area and
#'   indemnity (\code{evntA_*}, \code{evntI_*}) and binary flags
#'   (\code{CAT_evntA_*}, \code{CAT_evntI_*}). Default \code{TRUE}.
#'
#' @param location_grouping Logical. Add state name/abbreviation, ERS region,
#'   and NASS CRD (\code{COUNTY}, \code{ERSReg}, \code{ERSReg_cd}, \code{CRD}).
#'   Default \code{TRUE}.
#'
#' @param structural_breaks Logical. Add farm-bill era labels
#'   (\code{period_farmbill}) and a pre/post 2012 indicator
#'   (\code{period_combo}). Default \code{TRUE}.
#'
#' @param subsidy_bins Logical. Add coarse subsidy-share bins (e.g., \code{SUB040},
#'   \code{SUB042}, ..., \code{SUB080}). Default \code{TRUE}.
#'
#' @param practice_map Optional \code{data.frame}/\code{data.table} for practice
#'   recodes. If \code{NULL}, it is downloaded. Must include:
#'   \code{commodity_year}, \code{commodity_code}, \code{practice_code},
#'   \code{irrigation_recode}, \code{organic_recode}.
#'
#' @param commodity_map Optional \code{data.frame}/\code{data.table} for
#'   commodity groupings. If \code{NULL}, it is downloaded. Must include:
#'   \code{commodity_year}, \code{commodity_code}, \code{commodity_name},
#'   \code{CROP}, \code{commodity_group}.
#'
#' @param ers_xls Optional file path to the ERS region Excel file. If
#'   \code{NULL}, it is downloaded.
#'
#' @param crd_csv Optional file path to the NASS CRD CSV. If \code{NULL}, it is
#'   downloaded.
#'
#' @param base_year_loss Integer. Base year for the loss-ratio cumulative window
#'   (prior-year sums use years \code{>= base_year_loss} and \code{< current year}).
#'   Default \code{1999L}.
#'
#' @param sobcov Optional preloaded SOBCOV aggregate used by the program-age and
#'   loss-cause modules. If \code{NULL}, it is downloaded. Must include:
#'   \code{commodity_year}, \code{commodity_code}, \code{state_code},
#'   \code{county_code}, \code{net_reported_quantity} (or \code{net_acre_qty}),
#'   and \code{liability_amount} (or \code{liability_amt}). If \code{net_acre_typ}
#'   exists, rows containing "ACRE" are retained.
#'
#' @param sobscc Optional preloaded SOBSCC aggregate used by the program-age
#'   module. If \code{NULL}, it is downloaded. Same column requirements as
#'   \code{sobcov}.
#'
#' @param colsom Optional preloaded Cause-of-Loss detail used by the loss-cause
#'   module. If \code{NULL}, it is downloaded. Must include:
#'   \code{commodity_year} (or \code{crop_yr}), \code{commodity_code} (or \code{crop_cd}),
#'   \code{state_code} (or \code{state_cd}), \code{county_code} (or \code{county_cd}),
#'   \code{damage}, \code{det_acre_qty}, \code{indem_amt}.
#'
#' @return A \code{data.table} equal to \code{sob} with the selected modules
#'   added via left joins. If a \code{liability_amount} column exists, rows with
#'   non-finite or non-positive values are filtered at the end.
#'
#' @import data.table
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @importFrom usmap fips_info
#' @export
enrich_sobtpu_with_covariates <- function(
    sob,
    election_grouping    = TRUE,
    commodity_grouping   = TRUE,
    practice_grouping    = TRUE,
    loss_ratio_grouping  = TRUE,
    program_age_grouping = TRUE,
    loss_cause_grouping  = TRUE,
    location_grouping    = TRUE,
    structural_breaks    = TRUE,
    subsidy_bins         = TRUE,
    # optional resources:
    practice_map = NULL,
    commodity_map = NULL,
    ers_xls = NULL,
    crd_csv = NULL,
    base_year_loss = 1999L,
    sobcov = NULL,
    sobscc = NULL,
    colsom = NULL
){
  DT <- data.table::copy(data.table::as.data.table(sob))

  if (isTRUE(election_grouping))    DT <- enrich_with_election_grouping(DT)
  if (isTRUE(practice_grouping))    DT <- enrich_with_practice_grouping(DT, practice_map = practice_map)
  if (isTRUE(commodity_grouping))   DT <- enrich_with_commodity_grouping(DT, commodity_map = commodity_map, top_n = 12L)
  if (isTRUE(loss_ratio_grouping))  DT <- enrich_with_loss_ratio_grouping(DT, base_year = base_year_loss)
  if (isTRUE(program_age_grouping)) DT <- enrich_with_program_age(DT, sobcov = sobcov, sobscc = sobscc)
  if (isTRUE(loss_cause_grouping))  DT <- enrich_with_loss_cause(DT, colsom = colsom, sobcov = sobcov)
  if (isTRUE(location_grouping))    DT <- enrich_with_location_grouping(DT, ers_xls = ers_xls, crd_csv = crd_csv)
  if (isTRUE(structural_breaks))    DT <- enrich_with_structural_breaks(DT)
  if (isTRUE(subsidy_bins))         DT <- enrich_with_subsidy_bins(DT)

  if ("liability_amount" %in% names(DT)) {
    DT <- DT[is.finite(liability_amount) & liability_amount > 0]
  }
  DT[]
}

# ---- utilities ----------------------------------------------------

#' @keywords internal
#' @noRd
.scale_cov_pct <- function(x) {
  # Accepts coverage in [0,1] or [0,100]; returns rounded percent (e.g., 75)
  ifelse(is.na(x), NA_real_,
         ifelse(x <= 1, round(x * 100), round(x)))
}

# ---- modules ------------------------------------------------------

#' Add plan, product, coverage buckets, and unit structure labels
#'
#' @param sob data.frame/data.table with columns:
#'   \itemize{
#'     \item \code{insurance_plan_code}
#'     \item \code{coverage_level_percent} (either 0-1 or 0-100)
#'     \item \code{unit_structure_code}
#'   }
#' @return data.table with new columns: \code{PLAN}, \code{RPYP}, \code{COV}, \code{STRUCT}
#' @import data.table
#' @export
enrich_with_election_grouping <- function(sob) {
  DT <- copy(as.data.table(sob))

  req <- c("insurance_plan_code","coverage_level_percent","unit_structure_code")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns for election grouping: ", paste(miss, collapse=", "))

  cov_pct <- .scale_cov_pct(DT[["coverage_level_percent"]])

  DT[, `:=`(
    PLAN = fcase(
      insurance_plan_code %in% c(1, 90), "Yield",
      insurance_plan_code %in% c(2, 3),  "Revenue",
      default = NA_character_
    ),
    RPYP = fcase(
      insurance_plan_code %in% c(1, 90), "YP",
      insurance_plan_code == 2,          "RP",
      insurance_plan_code == 3,          "RPHPE",
      default = NA_character_
    ),
    COV = fcase(
      cov_pct %in% c(50L, 55L), "50-55%",
      cov_pct %in% c(60L, 65L), "60-65%",
      cov_pct %in% c(70L, 75L), "70-75%",
      cov_pct %in% c(80L, 85L), "80-85%",
      default = NA_character_
    ),
    STRUCT = fcase(
      unit_structure_code == "BU",                      "BU",
      unit_structure_code %in% c("EU","WU","EP","EC"), "EU",
      unit_structure_code %in% c("OU","UA","UD"),      "OU",
      default = "OU"
    )
  )]

  DT[]
}

#' Add irrigation/organic practice recodes (left join)
#'
#' @param sob data.frame/data.table with \code{commodity_year, commodity_code, practice_code}
#' @param practice_map Optional data.frame/data.table with columns
#'   \code{commodity_year, commodity_code, practice_code, irrigation_recode, organic_recode}.
#'   If \code{NULL}, it will be downloaded from the project release.
#' @return data.table with \code{irrigation_recode}, \code{organic_recode}
#' @import data.table
#' @importFrom utils download.file
#' @export
enrich_with_practice_grouping <- function(sob, practice_map = NULL) {

  DT <- copy(as.data.table(sob))

  req <- c("commodity_year","commodity_code","practice_code")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns for practice grouping: ", paste(miss, collapse=", "))

  if (is.null(practice_map)) {
    tf <- tempfile(fileext = ".rds")
    utils::download.file(
      "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_recodes_practice.rds",
      tf, mode = "wb", quiet = TRUE)
    practice_map <- readRDS(tf)
  }
  setDT(practice_map)
  practice_map <- practice_map[, .(commodity_year, commodity_code, practice_code,
                                   irrigation_recode, organic_recode)]

  # LEFT JOIN: keep all sob rows
  DT <- practice_map[DT, on = .(commodity_year, commodity_code, practice_code)]

  DT[]
}

#' Add commodity groupings and TOPCROP tag (left join)
#'
#' @param sob data.frame/data.table with \code{commodity_year, commodity_code, insured_acres}
#' @param commodity_map Optional mapping with columns
#'   \code{commodity_year, commodity_code, commodity_name, CROP, commodity_group}.
#'   If \code{NULL}, it will be downloaded.
#' @param top_n integer. Number of crops to keep as named in \code{TOPCROP}; others -> "OTHER".
#' @return data.table with \code{commodity_name}, \code{CROP}, \code{commodity_group}, \code{TOPCROP}
#' @import data.table
#' @importFrom utils download.file
#' @export
enrich_with_commodity_grouping <- function(sob, commodity_map = NULL, top_n = 12L) {
  DT <- copy(as.data.table(sob))

  req <- c("commodity_year","commodity_code","insured_acres")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns for commodity grouping: ", paste(miss, collapse=", "))

  if (is.null(commodity_map)) {
    tf <- tempfile(fileext = ".rds")
    utils::download.file(
      "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_recodes_commodity_groupings.rds",
      tf, mode = "wb", quiet = TRUE)
    commodity_map <- readRDS(tf)
  }
  setDT(commodity_map)
  commodity_map <- commodity_map[, .(commodity_year, commodity_code, commodity_name, CROP, commodity_group)]

  DT <- commodity_map[DT, on = .(commodity_year, commodity_code)]

  DT[, CROP := fifelse(
    CROP %chin% c("ALL OTHER CROPS","ALL OTHER COMMODITIES") | is.na(CROP) | CROP == "",
    "OTHER", CROP)]
  DT[, commodity_group := fifelse(is.na(commodity_group) | commodity_group == "", "OTHER", commodity_group)]

  # Top-N crops by insured acres (excluding FORAGE)
  topcrops <- DT[CROP != "FORAGE",
                 .(insured_acres = sum(insured_acres, na.rm = TRUE)),
                 by = CROP][order(-insured_acres)]
  keep <- head(topcrops$CROP, as.integer(top_n))
  DT[, TOPCROP := fifelse(CROP %chin% keep, CROP, "OTHER")]

  DT[]
}

#' Add state names/abbreviations, ERS Region, and NASS CRD (left joins)
#'
#' @param sob data.frame/data.table with \code{state_code, county_code}
#' @param ers_xls Optional path to the ERS excel file; if \code{NULL}, it is downloaded.
#' @param crd_csv Optional path to the CRD csv file; if \code{NULL}, it is downloaded.
#' @return data.table with \code{state_abbreviation}, \code{state_name}, \code{ERSReg}, \code{ERSReg_cd}, \code{CRD}
#' @import data.table
#' @importFrom usmap fips_info
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @importFrom utils download.file
#' @export
enrich_with_location_grouping <- function(sob, ers_xls = NULL, crd_csv = NULL) {

  DT <- copy(as.data.table(sob))

  req <- c("state_code","county_code")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns for location grouping: ", paste(miss, collapse=", "))

  # State names/abbreviations
  if (!requireNamespace("usmap", quietly = TRUE))
    stop("Package 'usmap' is required for location_grouping.", call. = FALSE)
  states <- data.table::as.data.table(usmap::fips_info(unique(DT$state_code)))
  states[, fips := as.integer(as.character(fips))]
  setnames(states, old = c("abbr","fips","full"),
           new = c("state_abbreviation","state_code","state_name"))
  DT <- merge(DT, states, by = "state_code", all.x = TRUE)

  # ERS Regions
  if (is.null(ers_xls)) {
    ers_xls <- tempfile(fileext = ".xls")
    utils::download.file(
      "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/spatial_features/County-to-ERS.Resource.Region.aggregation.xls",
      ers_xls, mode = "wb", quiet = TRUE)
  }
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required to read ERS excel.", call. = FALSE)
  ERSReg <- as.data.table(readxl::read_excel(ers_xls))

  DT[, COUNTY := fifelse(!is.na(state_code) & !is.na(county_code),
                         as.integer(sprintf("%02d%03d",
                                            as.integer(state_code),
                                            as.integer(county_code))),
                         NA_integer_)]

  setnames(ERSReg,
           old = c("name","id","county_fips"),
           new = c("ERSReg","ERSReg_cd","COUNTY"),
           skip_absent = TRUE)
  ERSReg[, COUNTY := as.integer(COUNTY)]
  DT <- merge(DT, ERSReg[, .(COUNTY, ERSReg, ERSReg_cd)], by = "COUNTY", all.x = TRUE)

  # CRD (mode per county, then build "SSRR": state FIPS + CRD)
  if (is.null(crd_csv)) {
    crd_csv <- tempfile(fileext = ".csv")
    utils::download.file(
      "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/spatial_features/NASS.County.and.District.Codes.csv",
      crd_csv, mode = "wb", quiet = TRUE)
  }
  if (!requireNamespace("readr", quietly = TRUE))
    stop("Package 'readr' is required to read CRD csv.", call. = FALSE)

  crd <- as.data.table(readr::read_csv(crd_csv, skip = 2))
  setnames(crd, c("state_code","CRD","county_code","county","flag"))
  crd <- unique(crd[as.integer(as.character(flag)) == 1L])
  crd[, `:=`(
    state_code  = as.integer(as.character(state_code)),
    county_code = as.integer(as.character(county_code)),
    CRD         = as.integer(as.character(CRD))
  )]

  crd_mode <- crd[!is.na(CRD), .N, by = .(state_code, county_code, CRD)
  ][order(state_code, county_code, -N, CRD)
  ][, .SD[1L], by = .(state_code, county_code)
  ][, N := NULL][]

  crd_mode[, CRD := sprintf("%02d%02d", state_code, CRD)]

  DT <- merge(DT, crd_mode[, .(state_code, county_code, CRD)],
              by = c("state_code","county_code"), all.x = TRUE)

  DT[]
}

#' Add farm-bill era labels and a before/after 2012 combo
#'
#' @param sob data.frame/data.table with \code{commodity_year}
#' @return data.table with \code{period_farmbill}, \code{period_combo}
#' @import data.table
#' @export
enrich_with_structural_breaks <- function(sob) {

  DT <- copy(as.data.table(sob))

  if (!"commodity_year" %in% names(DT))
    stop("Missing column 'commodity_year' for structural breaks.")

  DT[, period_farmbill := fcase(
    commodity_year < 1980, 0L,
    commodity_year >= 1980 & commodity_year < 1994, 1L,
    commodity_year >= 1994 & commodity_year < 1996, 2L,
    commodity_year >= 1996 & commodity_year < 2000, 3L,
    commodity_year >= 2000 & commodity_year < 2008, 4L,
    commodity_year >= 2008 & commodity_year < 2014, 5L,
    commodity_year >= 2014 & commodity_year < 2018, 6L,
    commodity_year >= 2018, 7L,
    default = NA_integer_
  )]
  labs <- c("Pre farm bill","1980 farm bill","1994 farm bill","1996 farm bill",
            "2000 farm bill","2008 farm bill","2014 farm bill","2018 farm bill")
  DT[, `:=`(
    period_farmbill = labs[as.integer(period_farmbill) + 1L],
    period_combo    = fcase(commodity_year >= 2012, "After",
                            commodity_year <  2012, "Before",
                            default = NA_character_)
  )]

  DT[]
}

#' Add coarse subsidy bins (e.g., SUB040, SUB042, ..., SUB080)
#'
#' @param sob data.frame/data.table with \code{subsidy_amount}, \code{total_premium_amount}
#' @param lo,hi numeric lower/upper clamp for ratio; defaults 0.40-0.80
#' @param step numeric bin width; default 0.02
#' @return data.table with \code{subsidy_bins} (character labels)
#' @import data.table
#' @export
enrich_with_subsidy_bins <- function(sob, lo = 0.40, hi = 0.80, step = 0.02) {

  DT <- copy(as.data.table(sob))

  req <- c("subsidy_amount","total_premium_amount")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns for subsidy bins: ", paste(miss, collapse=", "))

  DT[, subsidy_bins := {
    den <- total_premium_amount
    ratio <- ifelse(is.na(den) | den <= 0, NA_real_, subsidy_amount / den)
    x <- pmin(pmax(ratio, lo), hi)
    b <- lo + floor((x - lo + 1e-12) / step) * step
    ifelse(is.na(b), NA_character_, sprintf("SUB%03d", round(b * 100)))
  }]

  DT[]
}

#' Add cumulative loss ratio (prior years) and LR bins
#'
#' Computes cumulative indemnity and premium over *prior* years (from `base_year`)
#' for each (state, county, crop), derives:
#'   - `cum_lr`: cumulative loss ratio up to previous year
#'   - `CLRCAT`: bins (less than or equal to 0.50, 0.50-0.65, ..., greater than 5.00)
#'   - `CLRmCAT`: bins (less than or equal to 0.50, 0.50-0.65, ..., greater than  2.20)
#' and LEFT-joins these onto `sob`.
#'
#' @param sob data.frame/data.table with:
#'   state_code, county_code, CROP, commodity_year,
#'   indemnity_amount, total_premium_amount
#' @param base_year integer. First year included in the cumulative window. Default 1999L.
#' @return data.table (enriched sob)
#' @import data.table
#' @export
enrich_with_loss_ratio_grouping <- function(sob, base_year = 1999L) {

  DT <- copy(as.data.table(sob))

  # checks
  req <- c("state_code","county_code","CROP","commodity_year",
           "indemnity_amount","total_premium_amount")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))

  base_year <- as.integer(base_year)

  # 1) annual totals per group
  yearly <- DT[, .(
    indemnity = sum(indemnity_amount, na.rm = TRUE),
    premium   = sum(total_premium_amount, na.rm = TRUE)
  ), by = .(state_code, county_code, CROP, commodity_year)]

  # 2) cumulative (prior years only), starting at base_year
  yearly2 <- yearly[commodity_year >= base_year]
  setorder(yearly2, state_code, county_code, CROP, commodity_year)
  yearly2[, `:=`(
    cum_indemnity_prev = shift(cumsum(indemnity), fill = 0),
    cum_premium_prev   = shift(cumsum(premium),   fill = 0)
  ), by = .(state_code, county_code, CROP)]

  # 3) cum LR + bins
  cumLR <- yearly2[, .(
    state_code, county_code, CROP, commodity_year,
    cum_lr = fifelse(cum_premium_prev > 0,
                     cum_indemnity_prev / cum_premium_prev, NA_real_)
  )]

  brk1 <- c(-Inf, 0.50, 0.65, 1.00, 1.60, 2.20, 5.00, Inf)
  lab1 <- c("<=0.50","0.50-0.65","0.65-1.00","1.00-1.60","1.60-2.20","2.20-5.00",">5.00")
  cumLR[, CLRCAT := cut(cum_lr, brk1, labels = lab1, right = TRUE, include.lowest = TRUE)]

  brk2 <- c(-Inf, 0.50, 0.65, 1.00, 1.60, 2.20, Inf)
  lab2 <- c("<=0.50","0.50-0.65","0.65-1.00","1.00-1.60","1.60-2.20",">2.20")
  cumLR[, CLRmCAT := cut(cum_lr, brk2, labels = lab2, right = TRUE, include.lowest = TRUE)]

  # left-join back to sob
  setkey(DT,    state_code, county_code, CROP, commodity_year)
  setkey(cumLR, state_code, county_code, CROP, commodity_year)
  cumLR[DT][]
}

#' Attach "program age" by (state, county, commodity, year)
#'
#' Computes, for each row's (state_code, county_code, commodity_code, commodity_year),
#' the **span of years observed prior to the current year** with positive liability
#' in the combined SOBCOV/SOBSCC aggregates:
#'
#' \deqn{ program\_age\_by\_year(y) = \max\{t<y\} - \min\{t<y\}, \quad
#'        t \in \text{years with liab\_actual > 0} }
#'
#' If there is no prior year with positive liability, the age is set to 0.
#' Binned variants at widths 10/5/4 years are also added.
#'
#' @param sob data.frame/data.table with (required) columns:
#'   \code{state_code}, \code{county_code}, \code{commodity_code}, \code{commodity_year}.
#' @param sobcov Optional data.frame/data.table for SOBCOV; if \code{NULL}, it is downloaded.
#'   Must contain \code{commodity_year, commodity_code, state_code, county_code,
#'   net_reported_quantity, liability_amount}.
#' @param sobscc Optional data.frame/data.table for SOBSCC; if \code{NULL}, it is downloaded.
#'   Same column expectations as \code{sobcov}.
#' @return data.table equal to \code{sob} with new columns:
#'   \code{program_age_by_year}, \code{program_age_by_year_10},
#'   \code{program_age_by_year_05}, \code{program_age_by_year_04}.
#' @details
#' - Uses efficient cumulative min/max over a union timeline of **target years** (from \code{sob})
#'   and **observed positive-liability years** (from SOBCOV/SOBSCC) per group.
#' - All joins are left joins; no rows are dropped from \code{sob}.
#' - Age is computed on the year scale (integers) and **does not add 1** to the span,
#'   matching your original specification (\eqn{\max - \min}).
#' @import data.table
#' @importFrom utils download.file
#' @export
enrich_with_program_age <- function(sob, sobcov = NULL, sobscc = NULL) {

  DT <- copy(as.data.table(sob))
  req <- c("state_code","county_code","commodity_code","commodity_year")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns in `sob`: ", paste(miss, collapse = ", "))

  ## ---- 1) Load / summarize SOBCOV & SOBSCC ---------------------------------
  load_and_summarize <- function(obj, url_if_null) {
    if (is.null(obj)) {
      tf <- tempfile(fileext = ".rds")
      utils::download.file(url_if_null, tf, mode = "wb", quiet = TRUE)
      obj <- readRDS(tf)
    }
    obj <- as.data.table(obj)
    need <- c("commodity_year","commodity_code","state_code","county_code",
              "net_reported_quantity","liability_amount")
    miss <- setdiff(need, names(obj))
    if (length(miss)) stop("Aggregate source missing columns: ", paste(miss, collapse = ", "))
    obj[, .(
      area_actual = sum(net_reported_quantity, na.rm = TRUE),
      liab_actual = sum(liability_amount,    na.rm = TRUE)
    ), by = .(commodity_year, commodity_code, state_code, county_code)]
  }

  sobcov_agg <- load_and_summarize(
    sobcov,
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobcov_all.rds"
  )
  sobscc_agg <- load_and_summarize(
    sobscc,
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobscc_all.rds"
  )

  # Combine and keep positive, finite liability
  agg <- rbindlist(list(sobscc_agg, sobcov_agg), use.names = TRUE, fill = TRUE)
  agg <- agg[is.finite(liab_actual) & liab_actual > 0]

  ## ---- 2) Build per-group timeline (union of target & observed years) -------
  # Harmonize key types
  DT[, `:=`(
    state_code    =  as.numeric(as.character(state_code)),
    county_code   =  as.numeric(as.character(county_code)),
    commodity_code=  as.numeric(as.character(commodity_code)),
    commodity_year=  as.numeric(as.character(commodity_year))
  )]
  agg[, `:=`(
    state_code    =  as.numeric(as.character(state_code)),
    county_code   =  as.numeric(as.character(county_code)),
    commodity_code=  as.numeric(as.character(commodity_code)),
    commodity_year=  as.numeric(as.character(commodity_year))
  )]

  target <- unique(DT[, .(state_code, county_code, commodity_code, commodity_year)])
  obs    <- unique(agg[, .(state_code, county_code, commodity_code, commodity_year)])
  obs[, has_pos := TRUE]

  timeline <- rbindlist(
    list(
      target[, .(state_code, county_code, commodity_code, commodity_year, has_pos = FALSE)],
      obs
    ),
    use.names = TRUE, fill = TRUE
  )[
    # if both appear, keep has_pos = TRUE
    , .(has_pos = any(has_pos)), by = .(state_code, county_code, commodity_code, commodity_year)
  ]

  setorder(timeline, state_code, county_code, commodity_code, commodity_year)

  ## ---- 3) Cumulative prior min/max & age ------------------------------------
  timeline[, `:=`(
    yr_min_thru = cummin(ifelse(has_pos, commodity_year, +Inf)),
    yr_max_thru = cummax(ifelse(has_pos, commodity_year, -Inf))
  ), by = .(state_code, county_code, commodity_code)]

  timeline[, `:=`(
    min_prior = shift(yr_min_thru, type = "lag", fill = +Inf),
    max_prior = shift(yr_max_thru, type = "lag", fill = -Inf)
  ), by = .(state_code, county_code, commodity_code)]

  timeline[, program_age_by_year :=
             fifelse(is.finite(min_prior) & is.finite(max_prior),
                     max_prior - min_prior, NA_real_)]

  # Keep only target rows (years present in sob)
  age_map <- timeline[target,
                      on = .(state_code, county_code, commodity_code, commodity_year),
                      nomatch = 0L,
                      .(state_code, county_code, commodity_code, commodity_year,
                        program_age_by_year)]

  # Default missing ages to zero (no prior positive-liability year)
  age_map[is.na(program_age_by_year), program_age_by_year := 0]

  ## ---- 4) Bin helpers (10, 5, 4-year widths) --------------------------------
  make_bins <- function(x, step) {
    if (!length(x)) return(factor())
    mx <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(mx)) mx <- 0
    upper <- ceiling(mx / step) * step
    brks  <- c(0, seq(step, upper, by = step), Inf)
    labs  <- c(
      paste0(head(brks, -2), "-", (head(brks, -2) + step - 1L)),
      paste0(">=", brks[length(brks) - 1L])
    )
    cut(x, breaks = brks, right = FALSE, include.lowest = TRUE, labels = labs, ordered_result = TRUE)
  }

  age_map[, `:=`(
    program_age_by_year_10 = make_bins(program_age_by_year, 10L),
    program_age_by_year_05 = make_bins(program_age_by_year, 5L),
    program_age_by_year_04 = make_bins(program_age_by_year, 4L)
  )]

  ## ---- 5) Left-join back to sob ---------------------------------------------
  setkey(DT,     state_code, county_code, commodity_code, commodity_year)
  setkey(age_map,state_code, county_code, commodity_code, commodity_year)
  out <- age_map[DT]

  out[]
}

#' Add cause-of-loss shares (area & indemnity) and binary flags
#'
#' Downloads/uses Cause-of-Loss (COL) detail and SOBCOV aggregates to compute,
#' for each \{state, county, commodity, year\}, the percent of area and percent
#' of indemnity attributable to broad event categories (drought, heat, cold,
#' moisture, biotic, other). Results are joined back to `sob` as:
#' \itemize{
#'   \item \code{evntA_*}: percent of area (0-100) by event
#'   \item \code{evntI_*}: percent of indemnity (0-100) by event
#'   \item \code{CAT_evntA_*}, \code{CAT_evntI_*}: binary indicators (>0 to 1)
#' }
#'
#' @param sob data.frame/data.table with keys:
#'   \code{state_code}, \code{county_code}, \code{commodity_code}, \code{commodity_year}.
#' @param colsom Optional COL detail table. If \code{NULL}, downloads
#'   \code{col/colsom_all.rds}. Must have:
#'   \code{(crop_yr|commodity_year), (crop_cd|commodity_code),
#'         (state_cd|state_code), (county_cd|county_code),
#'         damage, net_determined_quantity, indemnity_amount}.
#' @param sobcov Optional SOBCOV table for denominators. If \code{NULL}, downloads
#'   \code{sob/sobcov_all.rds}. Must have:
#'   \code{(crop_yr|commodity_year), (crop_cd|commodity_code),
#'         (state_cd|state_code), (county_cd|county_code),
#'         net_reported_quantity|net_acre_qty,
#'         liability_amount|liability_amt}, and optionally \code{net_acre_typ}
#'   (rows containing "ACRE" are kept if present).
#'
#' @return data.table = \code{sob} with new \code{evntA_*}, \code{evntI_*},
#'   \code{CAT_evntA_*}, \code{CAT_evntI_*} columns.
#' @details
#' Event recodes:
#' \itemize{
#'   \item \strong{evnt*_drght}: DROUGHT; FAILURE OF IRRIGATION SUPPLY/EQUIPMENT; INABILITY TO PREPARE LAND FOR IRRIGATION; DROUGHT DEVIATION
#'   \item \strong{evnt*_tempH}: EXCESS SUN; HEAT; HOT WIND
#'   \item \strong{evnt*_tempL}: COLD WET WEATHER; COLD WINTER; FREEZE; FROST
#'   \item \strong{evnt*_moist}: EXCESS MOISTURE/PRECIPITATION/RAIN; FLOOD; POOR DRAINAGE; EROSION
#'   \item \strong{evnt*_biotc}: PLANT DISEASE; WILDLIFE; INSECTS; MYCOTOXIN; ASIAN SOYBEAN RUST; ASIATIC CITRUS CANKER; DISEASE, AQUACULTURE; MEDFLY; MYCOTOXIN (AFLATOXIN)
#'   \item \strong{evnt*_other}: all other/uncoded damages
#' }
#' Shares are capped to 100 and missing shares are filled with 0.
#' @import data.table
#' @importFrom utils download.file
#' @importFrom readr read_csv
#' @export
enrich_with_loss_cause <- function(sob, colsom = NULL, sobcov = NULL) {

  ## --- helpers -----------------------------------------------------
  std_keys <- c("commodity_year","commodity_code","state_code","county_code")

  .std_rename <- function(DT, mapping) {
    have <- intersect(names(mapping), names(DT))
    if (length(have)) setnames(DT, old = have, new = unname(mapping[have]))
    DT
  }

  ## --- check sob & prep -------------------------------------------
  DT <- copy(as.data.table(sob))
  need <- std_keys
  miss <- setdiff(need, names(DT))
  if (length(miss)) stop("`sob` missing columns: ", paste(miss, collapse = ", "))

  # normalize key types
  DT[, `:=`(
    state_code    =  as.numeric(as.character(state_code)),
    county_code   =  as.numeric(as.character(county_code)),
    commodity_code=  as.numeric(as.character(commodity_code)),
    commodity_year=  as.numeric(as.character(commodity_year))
  )]

  ## --- load COL detail --------------------------------------------
  if (is.null(colsom)) {
    tf <- tempfile(fileext = ".rds")
    utils::download.file(
      "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/col/colsom_all.rds",
      tf, mode = "wb", quiet = TRUE
    )
    colsom <- readRDS(tf)
  }
  COL <- as.data.table(colsom)
  # accept either crop_* or commodity_*
  COL <- .std_rename(COL, c(
    crop_yr = "commodity_year", crop_cd = "commodity_code",
    state_cd = "state_code", county_cd = "county_code",
    cause_of_loss_description = "damage"
  ))
  req_col <- c(std_keys, "damage", "net_determined_quantity", "indemnity_amount")
  miss <- setdiff(req_col, names(COL))
  if (length(miss)) stop("`colsom` missing columns: ", paste(miss, collapse = ", "))

  # recode damage -> damage_rc
  COL[, damage_u := toupper(trimws(as.character(damage)))]
  COL[, damage_rc := fcase(
    damage_u %chin% c("DROUGHT","FAILURE OF IRRIGATION SUPPLY","FAILURE OF IRRIGATION EQUIPMENT",
                      "INABILITY TO PREPARE LAND FOR IRRIGATION","DROUGHT DEVIATION"), "evnt_drght",
    damage_u %chin% c("EXCESS SUN","HEAT","HOT WIND"),                                 "evnt_tempH",
    damage_u %chin% c("COLD WET WEATHER","COLD WINTER","FREEZE","FROST"),              "evnt_tempL",
    damage_u %chin% c("EXCESS MOISTURE/PRECIPITATION/RAIN","FLOOD","POOR DRAINAGE","EROSION"),
    "evnt_moist",
    damage_u %chin% c("PLANT DISEASE","WILDLIFE","INSECTS","MYCOTOXIN","ASIAN SOYBEAN RUST",
                      "ASIATIC CITRUS CANKER","DISEASE, AQUACULTURE","MEDFLY","MYCOTOXIN (AFLATOXIN)"),
    "evnt_biotc",
    default = "evnt_other"
  )]

  COL <- COL[, .(
    damage_acre  = sum(net_determined_quantity, na.rm = TRUE),
    damage_indem = sum(indemnity_amount,    na.rm = TRUE)
  ), by = .(commodity_year, commodity_code, state_code, county_code, damage_rc)]

  ## --- load SOBCOV denominators -----------------------------------
  if (is.null(sobcov)) {
    tf <- tempfile(fileext = ".rds")
    utils::download.file(
      "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobcov_all.rds",
      tf, mode = "wb", quiet = TRUE
    )
    sobcov <- readRDS(tf)
  }
  SC <- as.data.table(sobcov)
  SC <- .std_rename(SC, c(
    crop_yr = "commodity_year", crop_cd = "commodity_code",
    state_cd = "state_code",    county_cd = "county_code",
    liability_amt = "liability_amount", net_acre_qty = "net_reported_quantity"
  ))

  # pick fields present
  area_field <- intersect(c("net_reported_quantity","net_acre_qty"), names(SC))
  liab_field <- intersect(c("liability_amount","liability_amt"), names(SC))
  if (length(area_field) == 0L || length(liab_field) == 0L)
    stop("`sobcov` must contain area and liability fields.")

  # if net_acre_typ exists, keep rows containing "ACRE"
  if ("net_acre_typ" %in% names(SC)) {
    SC <- SC[grepl("ACRE", SC$net_acre_typ, ignore.case = TRUE)]
  }

  SC <- SC[, .(
    area_actual = sum(get(area_field[1]), na.rm = TRUE),
    liab_actual = sum(get(liab_field[1]), na.rm = TRUE)
  ), by = .(commodity_year, commodity_code, state_code, county_code)]

  # keep finite + positive denominators only for share calc (not for joining)
  setkey(SC, commodity_year, commodity_code, state_code, county_code)
  setkey(COL, commodity_year, commodity_code, state_code, county_code)

  SH <- SC[COL]  # left join COL onto denominators

  # shares (0-100), cap at 100, guard denominators
  SH[, `:=`(
    damage_acre_share  = fifelse(is.finite(area_actual) & area_actual > 0,
                                 pmin(100, 100 * damage_acre  / area_actual), NA_real_),
    damage_indem_share = fifelse(is.finite(liab_actual) & liab_actual > 0,
                                 pmin(100, 100 * damage_indem / liab_actual), NA_real_)
  )]

  # replace NAs (missing event/denom) with 0 for pivot friendliness
  SH[is.na(damage_acre_share),  damage_acre_share  := 0]
  SH[is.na(damage_indem_share), damage_indem_share := 0]

  ## --- wide pivots (FIX: wrap formula with as.formula) -----------------------
  keycols <- c("commodity_year","commodity_code","state_code","county_code")
  form <- as.formula(paste(paste(keycols, collapse = " + "), "~ damage_rc"))
  A <- dcast(SH, form, value.var = "damage_acre_share",  fill = 0)
  I <- dcast(SH, form, value.var = "damage_indem_share", fill = 0)

  # prefix columns
  setnames(A, old = setdiff(names(A), keycols),
           new = paste0("evntA_", setdiff(names(A), keycols)))
  setnames(I, old = setdiff(names(I), keycols),
           new = paste0("evntI_", setdiff(names(I), keycols)))

  # join back to sob
  setkey(DT, commodity_year, commodity_code, state_code, county_code)
  setkey(A,  commodity_year, commodity_code, state_code, county_code)
  setkey(I,  commodity_year, commodity_code, state_code, county_code)

  OUT <- A[DT]
  OUT <- I[OUT]

  # fill missing event columns with 0 and build CAT_* indicators
  evnt_cols <- grep("^evnt[AI]_", names(OUT), value = TRUE)
  if (length(evnt_cols)) {
    for (v in evnt_cols) {
      OUT[is.na(get(v)), (v) := 0]
      OUT[, paste0("CAT_", v) := as.integer(get(v) > 0)]
    }
  }

  OUT[]
}
