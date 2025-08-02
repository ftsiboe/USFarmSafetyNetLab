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
#' @return A \code{character} vector specifying the columns used to define
#'   each FCIP insurance pool.
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
#' @return A \code{character} vector of field names used to specify insurance elections.
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
#' @return
#' A \code{character} vector of recoded insurance election field names.
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
#' @return A \code{character} vector of field names to coerce to numeric.
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
  "state_code",
  "county_code",
  "sub_county_code",
  "commodity_code",
  "type_code",
  "commodity_type_code",
  "class_code",
  "sub_class_code",
  "intended_use_code",
  "irrigation_practice_code",
  "cropping_practice_code",
  "organic_practice_code",
  "interval_code",
  "range_class_code",
  "practice_code",
  "record_category_code",
  "insurance_plan_code",
  "coverage_level_percent",
  "price_volatility_factor",
  "area_loss_start_percent",
  "area_loss_end_percent"
)

#' Column names to coerce to character
#'
#' A character vector of column names that should be converted to character
#' during data ingestion and cleaning.
#'
#' @format A \code{character} vector of column names.
#' @return A \code{character} vector of field names to coerce to character
#'
#' @export
FCIP_FORCE_CHARACTER_KEYS <- c(
  "unit_structure_code",
  "coverage_type_code",
  "record_type_code",
  "reference_amount_code",
  "optional_unit_allowed_flag",
  "basic_unit_allowed_flag",
  "enterprise_unit_allowed_flag",
  "whole_farm_unit_allowed_flag",
  "program_type_code",
  "type_practice_use_code",
  "private_508h_flag"
)
