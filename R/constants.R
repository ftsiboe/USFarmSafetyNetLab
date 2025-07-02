
#' FIXED PARAMETERS for FCIP simulation
#' @keywords internal
#' @noRd
#' @export
FCIP_FIXED_PARAMETERS <- list(
  revenue_lookup_adjustment_factor = 1,
  unit_structure_discount_factor = 1,
  additive_optional_rate_adjustment_factor = 0,
  multiplicative_optional_rate_adjustment_factor = 1,
  capped_revenue_add_on_factor=0,
  liability_adjustment_factor=1,
  multiple_commodity_adjustment_factor=1)


#' Factor for estimating acres under full subsidy or zero coverage
#'
#' When a policy change results in a 100% premium subsidy or causes an agent to
#' drop coverage entirely, the per‐acre out-of-pocket cost becomes zero. To
#' approximate the insured acres consistent with the revealed budget
#' (the pre-policy total out-of-pocket expenditure), the simulator applies this
#' factor in place of a zero cost.
#'
#' @details
#' Normally, insured acres are estimated as:
#' \deqn{acres = \frac{revealed\_budget}{post\_policy\_cost\_per\_acre}}.
#' If \code{post\_policy\_cost\_per\_acre} is zero (free policy or no coverage),
#' insured acres are approximated by:
#' \deqn{acres = \frac{revealed\_budget}{FCIP\_FREE\_ACRES\_FACTOR \times pre\_policy\_premium\_rate}}.
#' @keywords internal
#' @noRd
#' @return
#' A single \code{numeric} factor (default: 0.10).
#'
#' @seealso
#' \code{\link{FCIP_AREA_INSURED_MAX_FACTOR}} for capping insured‐acres acres under full subsidy or zero coverage.
#'
#' @examples
#' \dontrun{
#' # Override the default factor:
#' rFarmPolicySim:::FCIP_FREE_ACRES_FACTOR <- 0.2
#' }
#' @export
FCIP_FREE_ACRES_FACTOR <- 10/100


#' Maximum insured‐acres cap rate
#'
#' Defines the maximum allowable increase in insured acres when a policy change
#' yields zero out‐of‐pocket cost (e.g., 100% premium subsidy or dropped coverage).
#' After computing provisional acres via \code{FCIP_FREE_ACRES_FACTOR}, the simulator
#' caps the result at:
#' \deqn{max\_acres = (1 + FCIP\_AREA\_INSURED\_MAX\_FACTOR) \times revealed\_acres}
#' where \code{revealed_acres} is the pre‐policy observed acres (optionally
#' weighted by the number of competing post‐policy alternatives).
#' @keywords internal
#' @noRd
#' @return
#' A numeric scalar indicating the cap rate (default: 0.01, i.e., +1%).
#'
#' @details
#' - Prevents unrealistically large acre estimates when cost per acre drops to zero.
#' - Caps acres at \eqn{(1 + FCIP_AREA_INSURED_MAX_FACTOR) \times revealed_acres}.
#'
#' @seealso
#' \code{\link{FCIP_FREE_ACRES_FACTOR}} for estimating acres under full subsidy or zero coverage.
#'
#' @examples
#' \dontrun{
#' # Default cap (+1%)
#' FCIP_AREA_INSURED_MAX_FACTOR
#'
#' # Override to +5%
#' rFarmPolicySim:::FCIP_AREA_INSURED_MAX_FACTOR <- 0.05
#'}
#' @export
FCIP_AREA_INSURED_MAX_FACTOR <- 1/100


#' Insurance pool identifier fields
#'
#' A character vector of column names that together define a unique insurance
#' pool in the Federal Crop Insurance Program (FCIP).
#'
#' @details
#' Insurance pools represent the most granular level of rate making within FCIP.
#' Each pool is uniquely identified by the combination of:
#' \itemize{
#'   \item \strong{state_code}: State FIPS code
#'   \item \strong{county_code}: County FIPS code
#'   \item \strong{commodity_code}: Crop commodity code
#'   \item \strong{type_code}: Crop type (e.g., grain vs. silage)
#'   \item \strong{practice_code}: Production practice (e.g., irrigated, organic)
#' }
#'
#' @format A \code{character} vector of field names.
#' @keywords internal
#' @noRd
#' @return A \code{character} vector specifying the columns used to define
#'   each FCIP insurance pool.
#'
#' @seealso
#' \code{\link{FCIP_INSURANCE_ELECTION}} for the set of fields defining an insurance election.
#' \code{\link{FCIP_INSURANCE_ELECTION_RCODED}} for the set of fields defining a recoded insurance pool.
#'
#' @examples
#' \dontrun{
#' # Default insurance pool fields
#' FCIP_INSURANCE_POOL
#'
#' # Override to a subset of the original fields
#' rFarmPolicySim:::FCIP_INSURANCE_POOL <- c(
#'   "state_code", "county_code", "commodity_code"
#' )
#'}
#' @export
FCIP_INSURANCE_POOL <- c(
  "state_code",
  "county_code",
  "commodity_code",
  "type_code",
  "practice_code"
)


#' Insurance election identifier fields
#'
#' A character vector of column names that define an insurance election within
#' the Federal Crop Insurance Program (FCIP). Each field corresponds to an
#' attribute of a policy election.
#'
#' @format A \code{character} vector of field names:
#' \describe{
#'   \item{unit_structure_code}{Structure of the insured unit (e.g., basic, optional).}
#'   \item{insurance_plan_code}{Code for the insurance product (e.g., MPCI, CRC).}
#'   \item{coverage_type_code}{Type of coverage (e.g., Actual/Assumed Yield, Yield Protection).}
#'   \item{coverage_level_percent}{Elected coverage level as a percentage of approved yield or price.}
#' }
#' @keywords internal
#' @noRd
#' @return A \code{character} vector of field names used to specify insurance elections.
#'
#' @seealso
#' \code{\link{FCIP_INSURANCE_ELECTION_RCODED}} for the set of fields defining a recoded insurance pool.
#' \code{\link{FCIP_INSURANCE_POOL}} for the set of fields defining an insurance pool.
#'
#' @examples
#' \dontrun{
#' # Default election fields
#' FCIP_INSURANCE_ELECTION
#'
#' # Override to include only plan and coverage level
#' rFarmPolicySim:::FCIP_INSURANCE_ELECTION <- c(
#'   "insurance_plan_code",
#'   "coverage_level_percent"
#' )
#'}
#' @export
FCIP_INSURANCE_ELECTION <- c(
  "unit_structure_code",
  "insurance_plan_code",
  "coverage_type_code",
  "coverage_level_percent"
)


#' Insurance election identifier fields (recoded)
#'
#' A character vector of recoded column names that specify an insurance
#' election within the Federal Crop Insurance Program (FCIP). These fields
#' correspond to recoded versions of the original election attributes.
#'
#' @format A \code{character} vector of field names:
#' \describe{
#'   \item{unit_structure_recode}{Recoded unit structure (e.g., basic, optional).}
#'   \item{insurance_plan_recode}{Recoded insurance plan code (e.g., APH, YP).}
#'   \item{coverage_type_code}{Coverage type code (unchanged).}
#'   \item{coverage_level_percent}{unchanged}
#' }
#' @keywords internal
#' @noRd
#' @return
#' A \code{character} vector of recoded insurance election field names.
#'
#' @seealso
#' \code{\link{FCIP_INSURANCE_POOL}} for the set of fields defining an insurance pool.
#' \code{\link{FCIP_INSURANCE_ELECTION}} for the set of fields defining an insurance election.
#'
#' @examples
#' \dontrun{
#' # View the default recoded election fields
#' FCIP_INSURANCE_ELECTION_RCODED
#'
#' # Override to drop the recoded plan field
#' rFarmPolicySim::FCIP_INSURANCE_ELECTION_RCODED <- c(
#'   "unit_structure_recode",
#'   "coverage_type_code",
#'   "coverage_level_percent"
#' )
#'}
#' @export
FCIP_INSURANCE_ELECTION_RCODED <- c(
  "unit_structure_recode",
  "insurance_plan_recode",
  "coverage_type_code",
  "coverage_level_percent"
)


#' Column names to coerce to numeric
#'
#' A character vector of column names that should be converted from character
#' to numeric during data ingestion and cleaning.
#'
#' @details
#' The following fields, although often stored as text, represent numeric
#' values and must be coerced for accurate calculation and analysis:
#' \itemize{
#'   \item \code{commodity_year}: Crop year of the record.
#'   \item Fields in \code{FCIP_INSURANCE_POOL}:
#'     \code{state_code}, \code{county_code}, \code{commodity_code},
#'     \code{type_code}, \code{practice_code}.
#'   \item \code{record_category_code}: Category of the record.
#'   \item \code{insurance_plan_code}: Code for the insurance plan.
#'   \item \code{coverage_level_percent}: Coverage level percentage.
#' }
#'
#' @format A \code{character} vector of column names.
#' @keywords internal
#' @noRd
#' @return A \code{character} vector of field names to coerce to numeric.
#'
#' @seealso
#' \code{\link{FCIP_INSURANCE_POOL}} for the set of fields defining an insurance pool.
#'
#' @examples
#' \dontrun{
#' # View default keys
#' FCIP_FORCE_NUMERIC_KEYS
#'
#' # Extend with a custom numeric field
#' rFarmPolicySim:::FCIP_FORCE_NUMERIC_KEYS <- c(
#'   rFarmPolicySim:::FCIP_FORCE_NUMERIC_KEYS,
#'   "custom_numeric_field"
#' )
#' }
#'
#' @export
FCIP_FORCE_NUMERIC_KEYS <- c(
  "commodity_year",
  FCIP_INSURANCE_POOL,
  "record_category_code",
  "insurance_plan_code",
  "coverage_level_percent"
)

