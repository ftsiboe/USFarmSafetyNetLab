#' Setup Project Environment
#'
#' Initializes the working environment for a project by creating required
#' directories, setting useful global options, and fixing the random seed.
#'
#' @param year_beg Integer. Beginning year of the analysis (default: 2001).
#' @param year_end Integer. Ending year of the analysis
#'   (default: current system year).
#' @param seed Integer. Random seed for reproducibility (default: 1980632).
#' @param project_name Character. Project name (required). Used to build
#'   fast-scratch directory paths.
#' @param local_directories List of project-local directories to create
#'   (default: \code{list("data-raw/output", "data-raw/scripts", "data")}).
#' @param fastscratch_root Optional character. Root directory for fast-scratch
#'   files. If \code{NULL}, it is set automatically:
#'   \itemize{
#'     \item Windows: \code{"C:/fastscratch"}
#'     \item Linux/macOS: \code{"/fastscratch/<username>"}
#'   }
#' @param fastscratch_directories List of fast-scratch subdirectories (relative
#'   to \code{<fastscratch_root>/<project_name>}) to create. If \code{NULL},
#'   no fast-scratch subdirectories are created and \code{wd} is returned as an
#'   empty list.
#'
#' @details
#' The function ensures the requested directories exist, creating them if
#' necessary. Directory keys in the returned \code{wd} list are the basenames of
#' the provided \code{fastscratch_directories}.
#'
#' It also sets the following options:
#' \itemize{
#'   \item \code{options(scipen = 999)} (turns off scientific notation)
#'   \item \code{options(future.globals.maxSize = 8 * 1024^3)} (~8 GiB)
#'   \item \code{options(dplyr.summarise.inform = FALSE)} (quiet \pkg{dplyr})
#' }
#'
#' Finally, the random number generator is seeded with the provided \code{seed}.
#'
#' @return A list with:
#' \describe{
#'   \item{wd}{Named list of created fast-scratch directories. Empty if
#'     \code{fastscratch_directories = NULL}.}
#'   \item{year_beg}{Starting year (integer).}
#'   \item{year_end}{Ending year (integer).}
#'   \item{seed}{Seed value used for RNG.}
#' }
#'
#' @examples
#' \dontrun{
#' env <- setup_environment(
#'   year_beg = 2015,
#'   year_end = 2024,
#'   project_name = "HiddenSafetynet2025",
#'   fastscratch_directories = c("output/sims", "output/expected")
#' )
#' str(env$wd)
#' }
#'
#' @export
setup_environment <- function(
    year_beg = 2001,
    year_end = as.numeric(format(Sys.Date(), "%Y")),
    seed = 1980632,
    project_name,
    local_directories = list(
      file.path("data-raw", "output"),
      file.path("data-raw", "scripts"),
      file.path("data")
    ),
    fastscratch_root = NULL,
    fastscratch_directories = NULL) {

  # Validate required inputs
  if (missing(project_name) || is.null(project_name) || !nzchar(project_name)) {
    stop("`project_name` is required and cannot be empty.", call. = FALSE)
  }

  # Validate year and seed
  stopifnot(is.numeric(year_beg), is.numeric(year_end), is.numeric(seed), length(seed) == 1)
  year_beg <- as.integer(year_beg)
  year_end <- as.integer(year_end)
  seed     <- as.integer(seed)
  if (year_beg > year_end) stop("`year_beg` must be <= `year_end`.", call. = FALSE)

  # fastscratch directories
  fastscratch <- list()
  if (!is.null(fastscratch_directories)) {

    if (is.null(fastscratch_root)) {
      sysname <- tolower(as.character(Sys.info()[["sysname"]]))
      user    <- Sys.info()[["user"]]
      if (identical(user, "") || is.na(user)) user <- "unknown"
      fastscratch_root <- ifelse(
        grepl("windows", sysname),
        "C:/fastscratch",
        file.path("/fastscratch", user)
      )
      dir.create(fastscratch_root, recursive = TRUE, showWarnings = FALSE)
    }

    for (i in seq_along(fastscratch_directories)) {
      fastscratch[[basename(fastscratch_directories[[i]])]] <-
        file.path(fastscratch_root, project_name, fastscratch_directories[[i]])
    }
    invisible(lapply(fastscratch, function(p) dir.create(p, recursive = TRUE, showWarnings = FALSE)))
  }

  # Project-local directories
  for (i in seq_along(local_directories)) {
    dir.create(local_directories[[i]], recursive = TRUE, showWarnings = FALSE)
  }

  # Options
  options(scipen = 999L)
  options(future.globals.maxSize = 8 * 1024^3)  # bytes (~8 GiB)
  options(dplyr.summarise.inform = FALSE)

  # RNG seed
  set.seed(seed)

  # Return environment
  list(
    wd       = fastscratch,
    year_beg = year_beg,
    year_end = year_end,
    seed     = seed
  )
}

