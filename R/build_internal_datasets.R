#' Build and Document Helper Datasets
#'
#' Processes raw yearly `.rds` files into combined helper datasets and generates
#' roxygen docs for each. For every logical dataset (identified by its **base
#' file name** with year and suffixes removed), the function:
#' 1) loads all matching `.rds` files,
#' 2) coerces columns to character then infers types via [readr::type_convert()],
#' 3) row-binds across years,
#' 4) saves a compressed `.rda` under `./data/<file_out>.rda`, and
#' 5) appends a roxygen block to `R/helper_data.R` describing the combined dataset.
#'
#' If `R/helper_data.R` already exists, it is **renamed** to
#' `R/helper_data_<YYYY-MM-DD>.R` before a fresh file is created.
#'
#' @section Assumptions / Requirements:
#' - Each `source_files` path points to an `.rds` that deserializes to a `data.frame`
#'   (or `tibble`).
#' - The combined data must contain at least columns:
#'   - `commodity_year` (numeric/integer) used to report the year span in docs.
#'   - `data_source` (character) used to populate the roxygen `@source` field.
#' - Logical dataset identity is derived from the file **base name** after removing
#'   any leading year and certain suffixes:
#'   `file_out <- gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds", "", basename(path))`.
#'
#' @param source_files `character` vector.
#'   Absolute or relative file paths to yearly `.rds` files to be considered.
#' @param size_threshold `numeric(1)`.
#'   Maximum allowed size **in megabytes** for the largest yearly file within a
#'   logical dataset. If any year's file exceeds this threshold, that logical
#'   dataset is **skipped entirely**. Default: `5`.
#'
#' @details
#' The function first computes per-file sizes and groups by the derived logical
#' dataset name (`file_name`). Only groups whose **maximum** yearly file size is
#' `< size_threshold` proceed. For each remaining group, the yearly tables are:
#' read (`readRDS()`), coerced to `character` to unify types across files, then
#' passed to [readr::type_convert()] to infer consistent column types. The
#' combined table is saved with `compress = "xz"` for small on-disk footprint.
#'
#' Side effects:
#' - Creates `./data/` if missing.
#' - Backs up an existing `R/helper_data.R` by appending `Sys.Date()` to its name,
#'   then creates a fresh `R/helper_data.R` with a title header and one roxygen
#'   block per combined dataset.
#'
#' @return
#' (Invisibly) returns `NULL`. Primary outputs are written files:
#' - One `.rda` per combined dataset under `./data/`.
#' - A regenerated `R/helper_data.R` with roxygen docs referencing those objects.
#'
#' @section Roxygen blocks generated:
#' For each dataset `<file_out>`, a block with:
#' - `@name` / `@title` set to `<file_out>`
#' - `@description` mentioning it is a combined dataset
#' - `@format` reporting `nrow`, `ncol`, and year range from `commodity_year`
#' - `@source` from the first non-`NA` `data_source`
#' - `@usage data(<file_out>)`
#' - The final string literal `" <file_out> "` so the object is indexed by roxygen
#' 
#' @noRd
#' @keywords internal
build_internal_datasets <- function(source_files, size_threshold = 5 ){
  
  # ensure ./data exists
  if (!dir.exists("./data")) dir.create("./data", recursive = TRUE)
  
  sizes <- file.info(source_files)$size
  
  file_info <- data.frame(
    file_path  = source_files,
    size_bytes = sizes,
    size_mb    = sizes / (1024 * 1024),
    stringsAsFactors = FALSE
  )
  
  # Use the same stripping rule you later use for file_out
  file_info$file_name <- gsub(
    pattern = "^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds$",
    replacement = "",
    x = basename(source_files)
  )
  
  max_sizes <- file_info |>
    dplyr::group_by(file_name) |>
    dplyr::summarize(max_size = max(size_mb), .groups = "drop") |>
    dplyr::filter(max_size < size_threshold)
  
  file_info <- file_info |>
    dplyr::filter(file_name %in% max_sizes$file_name)
  
  # if nothing passes, just exit quietly
  if (nrow(file_info) == 0L) return(invisible(NULL))
  
  # ensure ./R exists before writing helper file
  if (!dir.exists("./R")) dir.create("./R", recursive = TRUE)
  
  if (file.exists("./R/helper_data.R")) {
    file.rename("./R/helper_data.R", paste0("./R/helper_data_", Sys.Date(), ".R"))
  }
  
  write("#' @title Simulator Helper Datasets\n",
        file = "./R/helper_data.R", append = FALSE)
  
  for (f in unique(file_info$file_name)) {
    # character vector (not data.frame)
    file_paths <- file_info$file_path[file_info$file_name == f]
    
    data <- file_paths |>
      purrr::map(function(p) {
        df <- readRDS(p)
        df[] <- lapply(df, as.character)
        df
      }) |>
      dplyr::bind_rows() |>
      suppressMessages() |>
      (\(x) readr::type_convert(x))()
    
    file_out <- unique(gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds$", "", basename(file_paths)))
    stopifnot(length(file_out) == 1L)
    file_out <- file_out[[1]]
    
    assign(file_out, data)
    save(list = file_out, file = file.path("data", paste0(file_out, ".rda")), compress = "xz")
    
    data_source <- data$data_source[1]
    
    doc_entry <- paste0(
      "#' @name ", file_out, "\n",
      "#' @title ", file_out, "\n",
      "#' @description A combined dataset for ", file_out, "\n",
      "#' @format A data frame with ", nrow(data), " rows and ", ncol(data),
      " columns covering ", min(data$commodity_year), "-", max(data$commodity_year), ".\n",
      "#' @source ", data_source, "\n",
      "#' @usage data(", file_out, ")\n",
      "\"", file_out, "\""
    )
    write(doc_entry, file = "./R/helper_data.R", append = TRUE)
  }
  
  invisible(NULL)
}


