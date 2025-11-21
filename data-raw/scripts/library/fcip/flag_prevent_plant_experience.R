#' Flag prevent-plant experience in Summary of Business by Practice and Type
#'
#' This function merges Summary of Business by Practice and Type
#' data with prevented-planting experience from cause-of-loss data and
#' prevented-plant guarantee adjustment factors. It:
#'
#' \itemize{
#'   \item Filters and pivots the prevented-plant adjustment-factor table to a
#'         wide format keyed by commodity/plan/option.
#'   \item Aligns SOB/TPU records and cause-of-loss records to common keys.
#'   \item Identifies prevented-plant experience using multiple matching
#'         strategies (\code{prevent_plant_status} cases 0-4).
#'   \item Computes the share of prevented-plant quantity in total determined
#'         net quantity (\code{prevent_plant_share}) where possible.
#' }
#'
#' The resulting SOB/TPU data table includes:
#' \itemize{
#'   \item \code{prevent_plant_share}: Percent share of prevented-plant quantity in total
#'         net determined quantity (0-100).
#'   \item \code{prevent_plant_status}: Indicator of how prevent-plant experience was
#'         identified:
#'         \describe{
#'           \item{0}{No matching cause-of-loss experience}
#'           \item{-1}{Has matching experience but not yet classified (Cases 1-3)}
#'           \item{1}{Case 1: One-to-one match on liability/premium/indemnity}
#'           \item{2}{Case 2: One SOB/TPU record contains all indemnity in pool}
#'           \item{3}{Case 3: One peril contains all indemnity in pool}
#'            \item{4}{Case 4: Group-level PP incidence greater than or equal to the specified threshold (e.g., 90%).}
#'         }
#' }
#'
#' @param sobtpu A \code{data.table} of Summary of Business by Practice and Type records,
#'   including liability, premium, subsidy, indemnity, coverage, and plan info.
#' @param pp_guarantee_adjustment_factor A \code{data.table} of prevented-plant
#'   guarantee adjustment factors keyed by commodity, plan, and insurance option.
#' @param colsom A \code{data.table} of cause-of-loss summary (loss experience)
#'   records, including liability, premium, subsidy, indemnity, stage codes,
#'   causes of loss, and quantities.
#' @param stage_code_list A \code{data.table} containing RMA stage-code
#'   definitions with fields such as \code{commodity_year}, \code{stage_code},
#'   and \code{stage_code_description}, used to link COLSOB records to
#'   prevented-planting stage descriptions.
#' @param pp_incidence_factor Threshold (in percentage points, 0-100) applied to
#'   \code{prevent_plant_share} for Case 4 classification. If the share of
#'   net determined quantity associated with prevented-planting stages meets or
#'   exceeds this value, the corresponding SOBTPU entries are assigned
#'   \code{prevent_plant_status = 4}. The default threshold is 90.
#'
#' @return A \code{data.table} equal to the input \code{sobtpu} restricted to
#'   relevant records and augmented with \code{prevent_plant_share} and \code{prevent_plant_status}.
#'
#' @import data.table
flag_prevent_plant_experience <- function(
    sobtpu,
    pp_guarantee_adjustment_factor,
    colsom,
    stage_code_list,
    pp_incidence_factor = 90){

  sobtpu <- data.table::as.data.table(sobtpu)
  pp_guarantee_adjustment_factor <- data.table::as.data.table(pp_guarantee_adjustment_factor)
  colsom <- data.table::as.data.table(colsom)
  stage_code_list <- data.table::as.data.table(stage_code_list)

  pp_guarantee_adjustment_factor[,data_source := NULL]

  # Normalize blank/NA insurance options to "00"
  pp_guarantee_adjustment_factor[
    insurance_option_code %in%  c(""," ",NA),
    insurance_option_code := "00"
  ]

  # Create wide-style column names for PP adjustment factors
  pp_guarantee_adjustment_factor[
    ,
    insurance_option_code := paste0("pp_adj_factor_", insurance_option_code)
  ]

  # Pivot to wide format on insurance_option_code
  pp_guarantee_adjustment_factor <- pp_guarantee_adjustment_factor |>
    tidyr::spread(
      insurance_option_code,
      prevented_planting_guarantee_adjustment_factor
    )
  pp_guarantee_adjustment_factor <- as.data.table(pp_guarantee_adjustment_factor)

  # ---------------------------------------------------------------------------
  # Align SOBTPU to PP adjustment factors
  # ---------------------------------------------------------------------------
  sobtpu <- sobtpu[
    unique(pp_guarantee_adjustment_factor[
      ,
      intersect(names(sobtpu), names(pp_guarantee_adjustment_factor)),
      with = FALSE
    ]),
    on = intersect(names(sobtpu), names(pp_guarantee_adjustment_factor)),
    nomatch = 0
  ]

  # Filter to major insurance plans and valid liability
  sobtpu <- sobtpu[insurance_plan_code %in% c(1:3,90), ]
  sobtpu <- sobtpu[!coverage_type_code %in% "C", ]
  sobtpu <- sobtpu[
    is.finite(liability_amount) & !is.na(liability_amount) & liability_amount != 0
  ]

  # Unique ID within sobtpu for later tracking
  sobtpu[, UID1 := .I]

  # Keep a full copy for final output (this will be returned)
  sobtpu_full <- sobtpu

  # For matching on indemnity experience, require positive/finite indemnity
  sobtpu <- sobtpu[!indemnity_amount %in% c(0,NA,NaN,Inf,-Inf), ]

  # ---------------------------------------------------------------------------
  # Align COLSOM to PP adjustment factors
  # ---------------------------------------------------------------------------
  colsom <- colsom[
    unique(pp_guarantee_adjustment_factor[
      ,
      intersect(names(colsom), names(pp_guarantee_adjustment_factor)),
      with = FALSE
    ]),
    on = intersect(names(colsom), names(pp_guarantee_adjustment_factor)),
    nomatch = 0
  ]

  colsom <- colsom[!liability_amount %in% c(0,NA,NaN,Inf,-Inf), ]
  colsom <- colsom[insurance_plan_code %in% c(1:3,90), ]
  colsom <- colsom[!coverage_type_code %in% "C", ]

  # Preserve original COLSOM monetary fields for matching
  colsom[, liability_col := liability_amount]
  colsom[, totalprem_col := total_premium_amount]
  colsom[, subsidy_col   := subsidy_amount]
  colsom[, indem_col     := indemnity_amount]

  # Keep only needed columns for downstream matching
  colsom <- colsom[
    ,
    c(
      "commodity_year","state_code","county_code","commodity_code",
      "insurance_plan_code","coverage_type_code",
      "liability_col","totalprem_col","subsidy_col","indem_col",
      "damage_name_recode","stage_code","cause_of_loss_code",
      "month_of_loss","year_of_loss",
      "net_determined_quantity","net_planted_quantity",
      "policies_earning_premium_count","policies_indemnified_count",
      "cause_of_loss_description"
    ),
    with = FALSE
  ]

  # ---------------------------------------------------------------------------
  # Attach stage-code descriptions and isolate prevented-plant stages
  # ---------------------------------------------------------------------------
  colsom <- colsom[stage_code_list, on = c("commodity_year","stage_code"),nomatch = 0]

  # Identify common ID keys between SOBTPU and COLSOM
  IDLIST <- names(sobtpu)[names(sobtpu) %in% names(colsom)]

  if (length(IDLIST) == 0L) {
    stop("No common ID keys between sobtpu and colsom; check input schemas.")
  }

  # Restrict COLSOM to records with stage descriptions containing "Prevented"
  colsom <- colsom[
    unique(colsom[grepl("Prevented", stage_code_description), IDLIST, with = FALSE]),
    on = IDLIST,
    nomatch = 0
  ]

  # Unique ID within COLSOM for later exclusion
  colsom[, UID2 := .I]

  # ---------------------------------------------------------------------------
  # [Case 0] Compute prevent_plant_share: share of prevented quantity in total quantity
  # ---------------------------------------------------------------------------
  DATA0 <- colsom[
    grepl("Prevented", stage_code_description),
    lapply(.SD, function(x) sum(x, na.rm = TRUE)),
    by = IDLIST,
    .SDcols = c("net_determined_quantity")
  ][
    colsom[
      ,
      .(net_determined_quantity_sum = sum(net_determined_quantity, na.rm = TRUE)),
      by = IDLIST
    ],
    on = IDLIST,
    nomatch = 0
  ][
    ,
    prevent_plant_share := round((net_determined_quantity / net_determined_quantity_sum) * 100)
  ]

  # Merge prevent_plant_share back to the full SOBTPU universe
  sobtpu_full <- merge(
    sobtpu_full,
    DATA0[
      ,
      c(intersect(names(sobtpu_full), names(DATA0)), "prevent_plant_share"),
      with = FALSE
    ],
    by = intersect(names(sobtpu_full), names(DATA0)),
    all.x = TRUE
  )

  sobtpu_full <- sobtpu_full[
    is.finite(liability_amount) & !is.na(liability_amount) & liability_amount != 0
  ]
  sobtpu_full[
    is.na(prevent_plant_share) | !is.finite(prevent_plant_share),
    prevent_plant_share := 0
  ]
  sobtpu_full[
    is.na(prevent_plant_share) | !is.finite(prevent_plant_share) | prevent_plant_share < 0,
    prevent_plant_share := 0
  ]

  # ---------------------------------------------------------------------------
  # Restrict SOBTPU to those with matching COLSOM experience & init prevent_plant_status
  # ---------------------------------------------------------------------------
  sobtpu <- sobtpu[
    unique(colsom[
      ,
      intersect(names(sobtpu), names(colsom)),
      with = FALSE
    ]),
    on = intersect(names(sobtpu), names(colsom)),
    nomatch = 0
  ]

  # prevent_plant_status:
  #   0  = no matching experience (never in sobtpu subset)
  #  -1  = matched, but not yet classified into Case 1, 2, or 3
  sobtpu_full[
    ,
    prevent_plant_status := ifelse(!UID1 %in% sobtpu$UID1, 0, -1)
  ]

  # ---------------------------------------------------------------------------
  # [Case 1] One-to-one match on experience via rounded monetary fields
  # ---------------------------------------------------------------------------
  SOB1 <- copy(sobtpu)
  SOB1[, liability_amount     := round(liability_amount)]
  SOB1[, total_premium_amount := round(total_premium_amount)]
  SOB1[, subsidy_amount       := round(subsidy_amount)]
  SOB1[, indemnity_amount     := round(indemnity_amount)]

  COL1 <- copy(colsom)
  COL1[, liability_amount     := round(liability_col)]
  COL1[, total_premium_amount := round(totalprem_col)]
  COL1[, subsidy_amount       := round(subsidy_col)]
  COL1[, indemnity_amount     := round(indem_col)]

  DATA1 <- SOB1[
    COL1,
    on = intersect(names(COL1), names(SOB1)),
    nomatch = 0
  ]

  sobtpu <- sobtpu[!UID1 %in% DATA1$UID1]
  colsom <- colsom[!UID2 %in% DATA1$UID2]

  DATA1 <- DATA1[grepl("Prevented", stage_code_description)]
  sobtpu_full[UID1 %in% DATA1$UID1, prevent_plant_status := 1]

  # ---------------------------------------------------------------------------
  # [Case 2] One SOBTPU pool contains all indemnity
  # ---------------------------------------------------------------------------
  DATA2 <- sobtpu[
    ,
    .(indemnity_amount_length = length(indemnity_amount)),
    by = IDLIST
  ]

  DATA2 <- sobtpu[
    DATA2,
    on = intersect(names(DATA2), names(sobtpu)),
    nomatch = 0
  ]

  DATA2 <- DATA2[indemnity_amount_length %in% 1]
  DATA2 <- colsom[DATA2, on = IDLIST, nomatch = 0]

  sobtpu <- sobtpu[!UID1 %in% DATA2$UID1]
  colsom <- colsom[!UID2 %in% DATA2$UID2]

  DATA2 <- DATA2[grepl("Prevented", stage_code_description)]
  sobtpu_full[UID1 %in% DATA2$UID1, prevent_plant_status := 2]

  # ---------------------------------------------------------------------------
  # [Case 3] One peril contains all indemnity
  # ---------------------------------------------------------------------------
  DATA3 <- colsom[
    colsom[
      ,
      .(indem_col_length = length(indem_col)),
      by = IDLIST
    ],
    on = IDLIST,
    nomatch = 0
  ]

  DATA3 <- DATA3[indem_col_length %in% 1]
  DATA3 <- sobtpu[DATA3, on = IDLIST, nomatch = 0]

  sobtpu <- sobtpu[!UID1 %in% DATA3$UID1]
  colsom <- colsom[!UID2 %in% DATA3$UID2]

  DATA3 <- DATA3[grepl("Prevented", stage_code_description)]
  sobtpu_full[UID1 %in% DATA3$UID1, prevent_plant_status := 3]

  # ---------------------------------------------------------------------------
  # [Case 4] High-incidence PP groups: assign PP status where at least
  #           pp_incidence_factor% of net determined quantity is associated
  #           with PP stage codes
  # ---------------------------------------------------------------------------
  sobtpu_full[
    prevent_plant_share >= pp_incidence_factor & prevent_plant_status <= 0,
    prevent_plant_status := 4
  ]

  sobtpu_full[indemnity_amount %in% 0, prevent_plant_status := 0]
  
  return(sobtpu_full)
}

