###############################################################################
# Script: 001_clean_rma_sobtpu.R
#
# Description:
#   This script processes RMA Statement of Business (SOB) data for 2014–2025.
#   It prepares an analysis-ready dataset that includes:
#     - Aggregated insured acres, liability, premiums, subsidies, and indemnities
#     - Supplemental Coverage Option (SCO) shares by coverage level
#     - Enhanced Coverage Option (ECO) shares (eco90 and eco95)
#
#   The output is saved as an RDS file:
#     data/rma_sob_with_sco_eco.rds
#
# Steps performed:
#   1. Load SOB data using get_sob_data()
#   2. Filter by relevant insurance plans, coverage type, and reporting level
#   3. Normalize plan codes to align endorsements with base plans
#   4. Aggregate insured acres and financial metrics
#   5. Calculate SCO shares and ECO shares relative to insured acres
#   6. Merge results into a single dataset
#   7. Clean invalid values (0, NA, Inf) and cap SCO/ECO shares at 1
#   8. Save the processed dataset as an RDS file
#
# Author: Francis Tsiboe
###############################################################################
rm(list=ls(all=TRUE));gc()
source("scripts/000_hidden_safetynet_helpers.R")
study_env <- setup_environment()

# Pull Statement of Business (SOB) data for study year
sob <- get_sob_data(sob_version = "sobtpu", year = study_env$year_beg:study_env$year_end)

# Keep only desired insurance plan codes (base plans 1–3 and several endorsements)
sob <- sob[sob$insurance_plan_code %in% c(1:3, 31:33, 35:36, 87:89, 90), ]

# Exclude CAT coverage ('C') and keep only records reported at the Acres level
sob <- sob[!sob$coverage_type_code %in% "C", ]
sob <- sob[sob$reporting_level_type %in% "Acres", ]

# Switch to data.table for fast aggregations/joins
setDT(sob)

# Treat plan code 90 as plan 1 (re-map in place)
sob[insurance_plan_code %in% 90, insurance_plan_code := 1]

# ===== Base data: aggregate core metrics by year, pool, and election =====
data <- sob[
  # keep base/related plan codes for this aggregation
  insurance_plan_code %in% c(1:3, 90),
  .(
    insured_acres        = sum(net_reporting_level_amount,   na.rm = TRUE),
    liability_amount     = sum(liability_amount,             na.rm = TRUE),
    total_premium_amount = sum(total_premium_amount,         na.rm = TRUE),
    subsidy_amount       = sum(subsidy_amount,               na.rm = TRUE),
    indemnity_amount     = sum(indemnity_amount,             na.rm = TRUE)
  ),
  by = c("commodity_year", FCIP_INSURANCE_POOL, FCIP_INSURANCE_ELECTION)
][
  # drop zero/invalid insured acres groups
  !insured_acres %in% c(0, NA, NaN, Inf, -Inf)
]

# ===== SCO share by coverage level (plans 31–33) =====
sco_data <- sob[
  insurance_plan_code %in% c(31:33),
  .(sco = sum(endorsed_commodity_reporting_level_amount, na.rm = TRUE)),
  by = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "coverage_level_percent")
][
  # normalize plan codes: 31→1, 32→2, 33→3 to align with base plan codes
  , insurance_plan_code := insurance_plan_code - 30
][
  # join in base 'insured_acres' by year/pool/plan/coverage level
  data[, .(insured_acres = sum(insured_acres, na.rm = TRUE)),
       by = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "coverage_level_percent")],
  on = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "coverage_level_percent"),
  nomatch = 0
][
  # convert SCO endorsed acres to share of insured acres
  , sco := sco / insured_acres
]

# Keep unique SCO rows with just the key fields and the SCO share
sco_data <- unique(
  sco_data[, c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "coverage_level_percent", "sco"), with = FALSE]
)

# Join SCO shares back to the base 'data'
data <- data[sco_data, on = intersect(names(data), names(sco_data)), nomatch = 0]

# ===== ECO shares by coverage level (plans 87–89) =====
eco_data <- sob[
  insurance_plan_code %in% c(87:89),
  .(eco = sum(endorsed_commodity_reporting_level_amount, na.rm = TRUE)),
  by = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "coverage_level_percent")
][
  # normalize plan codes: 87→1, 88→2, 89→3
  , insurance_plan_code := insurance_plan_code - 86
][
  # rename coverage level into wide column names: e.g., 0.90 → "eco90"
  , coverage_level_percent := paste0("eco", round(coverage_level_percent * 100))
][
  # drop invalid/zero ECO amounts
  !eco %in% c(0, NA, NaN, Inf, -Inf)
] |>
  # pivot to wide so each coverage level becomes a column (eco90, eco95, …)
  tidyr::spread(coverage_level_percent, eco)

# Convert to data.table and divide by insured acres to get shares
eco_data <- as.data.table(eco_data)[
  # bring in insured_acres totals by year/pool/plan
  data[, .(insured_acres = sum(insured_acres, na.rm = TRUE)),
       by = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code")],
  on = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code"),
  nomatch = 0
][
  # convert ECO endorsed acres to shares
  , eco90 := eco90 / insured_acres
][
  , eco95 := eco95 / insured_acres
]

# Keep unique ECO rows with only keys and desired ECO share columns
eco_data <- unique(
  eco_data[, c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "eco90", "eco95"), with = FALSE]
)

# Join ECO shares back to the base 'data'
data <- data[eco_data, on = intersect(names(data), names(eco_data)), nomatch = 0]

# Work as a data.frame for the following vectorized ifelse edits
data <- as.data.frame(data)

# Clamp SCO/ECO shares: replace invalids with 0, cap at 1
for (xx in c("sco", "eco90", "eco95")) {
  data[, xx] <- ifelse(data[, xx] %in% c(0, NA, NaN, Inf, -Inf), 0, data[, xx])
  data[, xx] <- ifelse(data[, xx] > 1, 1, data[, xx])
}

# Final guard: drop rows that still have invalid insured acres
data <- data[!data$insured_acres %in% c(0, NA, NaN, Inf, -Inf), ]

# Save the result as a data.table RDS for downstream use
saveRDS(as.data.table(data), file = "data/cleaned_rma_sobtpu.rds")

