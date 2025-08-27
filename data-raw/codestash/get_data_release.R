#' Download and cache a data release RDS file from the US-FarmSafetyNet-Lab GitHub repository
#'
#' This function constructs the filename `<name>_<year>.rds`, determines the appropriate GitHub release tag
#' based on the prefix of `name` ("sob" or "col"), and downloads the file via the piggyback package
#' if it is not already present in the user cache directory. The file is then read into R and returned.
#' Currently supports only Summary of Business releases ("sobscc", "sobcov", "sobtpu") and Cause of Loss releases ("colsom").
#'
#' @param name Character. Base name of the data release (one of "sobscc", "sobcov", "sobtpu", or "colsom"). Defaults to "sobscc".
#' @param year Character or numeric. Year of the data release (e.g., 2020) or "all" for combined releases across all years.
#'             Defaults to "all".
#'             Availability:
#'             * sobscc: "all" only 
#'             * sobcov: "all" and individual years from 1989-2025
#'             * sobtpu: "all" and individual years from 1999-2025
#'             * colsom: "all" and individual years from 1989-2025
#' @return The R object loaded from the requested RDS file.
#' @details
#' - `tools::R_user_dir("USFarmSafetyNetLab", which = "cache")` locates or creates a per-user cache directory.
#' - `piggyback::pb_download()` fetches the file from the GitHub repo "ftsiboe/US-FarmSafetyNet-Lab" under the appropriate tag.
#' - Subsequent calls with the same name and year retrieve the cached file, avoiding repeated downloads.
#'
#' @importFrom tools R_user_dir
#' @importFrom piggyback pb_download
#' @examples
#' \dontrun{
#'   # Download or retrieve cached "sobscc_2021.rds"
#'   df <- get_data_release("sobscc", 2021)
#' }
#' @export
get_data_release <- function(name = "sobscc", year = "all") {
  # Construct file name
  name_list <- paste0(name, "_", year, ".rds")
  
  # Determine GitHub release tag based on data type
  if (grepl("sob", name)) tag <- "sob"
  if (grepl("col", name)) tag <- "col"
  #if (grepl("adm", name)) tag <- "adm"
  
  # Set up cache directory
  dest_dir <- tools::R_user_dir("USFarmSafetyNetLab", which = "cache")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Define destination file path
  file_name <- name_list[1]
  dest_file <- file.path(dest_dir, file_name)
  
  # Download file if not cached
  if(!file.exists(dest_file)){
    piggyback::pb_download(
      file = file_name,
      repo = "ftsiboe/US-FarmSafetyNet-Lab",
      tag  = tag,
      dest = dest_dir
    )
  }
  
  # Read and return the RDS file
  readRDS(dest_file)
}