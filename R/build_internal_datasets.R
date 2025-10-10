#' Get File Information from a Directory
#'
#' Scans a specified directory for files with a given suffix and returns a data frame
#' containing their file paths, sizes in bytes, and sizes in megabytes.
#'
#' @param directory A character string specifying the path to the directory to scan.
#'   Defaults to \code{"./data-raw"}.
#' @param file_suffix A character string specifying the file suffix to match.
#'   Defaults to \code{".rds"}.
#'
#' @return A data frame with columns:
#'   \item{file_path}{Full file path}
#'   \item{size_bytes}{File size in bytes}
#'   \item{size_mb}{File size in megabytes}
#'
#' @examples
#' \dontrun{
#' get_file_info()
#' get_file_info(directory = "./my-data", file_suffix = ".csv")
#' }
#' @source coppied from rfcip on 07/24/2025
get_file_info <- function(directory = "./data-raw", file_suffix = ".rds") {
  # Get list of all files recursively
  files <- list.files(path = directory, recursive = TRUE, full.names = TRUE, pattern = file_suffix)
  
  # Filter only actual files (not directories)
  files <- files[file.info(files)$isdir == FALSE]
  
  # Get file sizes
  sizes <- file.info(files)$size
  
  # Create data frame
  df <- data.frame(
    file_path = files,
    size_bytes = sizes,
    size_mb = sizes / (1024 * 1024), # Convert to MB
    stringsAsFactors = FALSE
  )
  
  return(df)
}


#' Build and Document Helper Datasets
#'
#' @description
#' `build_internal_datasets()` processes all raw `.rds` files found under a specified
#' directory, harmonizes and combines them across years, applies type conversions,
#' writes compressed `.rda` files into `./data/`, and generates or updates
#' `R/helper_data.R` with roxygen documentation for each combined dataset.
#'
#' @param dir_source `character(1)`
#'   Path to the top-level directory containing raw yearly `.rds` files.
#'   Defaults to `"./data-raw/internal_datasets"`.
#' @param size_threshold   `numeric(1)`
#'   Maximum file size (in megabytes) allowed for inclusion.  Any dataset whose
#'   largest yearly file exceeds this threshold is skipped entirely.  Defaults to `5`.
#'   
#' @return
#' Invisibly returns `NULL`. Side effects include creation of `.rda` files in `./data/`
#' and generation of `R/helper_data.R` containing roxygen entries.
#'
#' @importFrom purrr map
#' @importFrom readr type_convert
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   # Combine raw data in "./data-raw", excluding files >1 MB:
#'   build_fcipSim_helper_datasets(dir = "./data-raw", size_threshold = 1)
#' }
build_internal_datasets <- function(dir_source = "./data-raw/internal_datasets", size_threshold = 5 ){
  
  # id "./data" doesn't exist, create it
  if(!dir.exists("./data")) {
    dir.create("./data")
  }
  
  file_info <- get_file_info(directory =dir_source, file_suffix = ".rds")
  
  # Add a column with the file name without any of the parent folders
  file_info$file_name <- gsub(paste0(dir_source, "/"), "", file_info$file_path)
  file_info$file_name <- gsub("[0-9]{4}/", "", file_info$file_name)
  file_info$file_name <- gsub(".rds", "", file_info$file_name)
  
  # keep only files that are less than the size threshold (in MB). Applied to
  # maximum size over all years
  max_sizes <- file_info |>
    dplyr::group_by(.data$file_name) |>
    dplyr::summarize(max_size = max(.data$size_mb)) |>
    dplyr::filter(.data$max_size < size_threshold)
  
  file_info <- file_info |> filter(.data$file_name %in% max_sizes$file_name)
  
  # if "./R/helper_data.R" already exists, rename it with the date appended
  if(file.exists("./R/helper_data.R")){
    file.rename("./R/helper_data.R", paste0("./R/helper_data_", Sys.Date(), ".R"))
  }
  
  # create a new file with the header for the documentation
  write("#' @title Simulator Helper Datasets\n",
        file = "./R/helper_data.R", append = FALSE)
  
  # for each unique value in the file_name column,
  # load all the datasets corresponding to that file name,
  # combine them, save as .rda, and document in helper_data.R
  for(f in unique(file_info$file_name)){
    
    # get the file paths for the current file name
    file_paths <- file_info[file_info$file_name == f, "file_path"]
    
    # load and combine the datasets
    data <- file_paths |>
      purrr::map(~ {
        df <- readRDS(.x)                 # read in the data frame
        df[] <- lapply(df, as.character) # convert all columns to character
        df                               # return the data frame
      }) |>
      dplyr::bind_rows()                 # bind rows
    
    # convert columns to their appropriate types
    data <- suppressMessages(readr::type_convert(data))
    
    # derive the output name from file paths
    file_out <- unique(gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds", "",
                            basename(file_paths)))
    
    # assign and save as .rda
    assign(file_out, data)
    save(list = file_out, file = paste0("./data/", file_out, ".rda"), compress = "xz")
    
    # extract data source for documentation
    data_source <- data$data_source[1]
    
    # build a roxygen doc entry
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
    
    # append the doc entry
    write(doc_entry, file = "./R/helper_data.R", append = TRUE)
  }
  
}
