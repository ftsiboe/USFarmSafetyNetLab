#' Download and cache raw data files
#'
#' @description
#' Ensures that a `./data-raw` directory exists, and for each requested data type,
#' downloads the corresponding dataset (if not already present) and saves it as an RDS file.
#'
#' @param dir_fastscratch `character(1)`
#'   Path to a directory where downloaded files will be stored.  Defaults to `"./data-raw"`.
#'
#' @details
#' - If the `./data-raw` directory does not exist, it will be created.
#' - For `"sobtpu"`:
#'   - Checks for an existing `sobtpu.rds` file in `./data-raw`.
#'   - If missing, calls `rfcip::get_sob_data(sob_version="sobtpu", year=1999:current_year)`
#'     to retrieve the data, coerces it to a `data.table`, and saves it to `./data-raw/sobtpu.rds`.
#'
#' @import rfcip
#' @return
#' Invisibly returns `NULL`. Side effects include creating files under `./data-raw`.
#'
#' @examples
#' \dontrun{
#'   # Download only the SOBTPU data if not already cached
#'   download_raw_data("sobtpu")
#'
#'   # Download multiple data types (future-proof)
#'   download_raw_data(c("sobtpu"))
#' }
#'
#' @export
download_raw_data <- function(dir_fastscratch="./data-raw/fastscratch"){
  
  # Create target directory if needed
  if (!dir.exists(dir_fastscratch)) {
    dir.create(dir_fastscratch, recursive = TRUE)
  }
  
  # Download if sobtpu is missing OR more than 7 days old
  file_path <- paste0(dir_fastscratch,"/sobtpu.rds")
  if (
    !file.exists(file_path) ||
    difftime(Sys.time(), file.info(file_path)$mtime, units = "days") > 7
  ) {
    sobtpu <- rfcip::get_sob_data(
      sob_version = "sobtpu",
      year        = 1999:as.numeric(format(Sys.Date(), "%Y"))
    )
    data.table::setDT(sobtpu)
    saveRDS(sobtpu, file = file_path)
  }
  
  # Download nass large datasets
  get_nass_large_datasets(large_dataset="crops", dir_nass_qs = paste0(dir_fastscratch,"/nass_qs"))
  get_nass_large_datasets(large_dataset="economics", dir_nass_qs = paste0(dir_fastscratch,"/nass_qs"))
  
}


#' Calculate key distributional moments for a numeric vector of revenues
#'
#' @param x Numeric vector of revenues (e.g. per-draw revenues)
#' @return A one-row data.frame with:
#'   - mu:      mean
#'   - md:      median
#'   - sd:      standard deviation
#'   - cv:      coefficient of variation (sd / mu)
#'   - vr:      variance
#'   - sk:      skewness
#'   - ku:      kurtosis
#'   - lapv:    mean squared loss deviations below the mean
#'   - lrpv:    lapv / probability of loss
#'   - nlapv:   lapv normalized by mu
#'   - nlrpv:   lrpv normalized by mu
#'   - lres2:   same as lapv (for clarity)
#'   - cdf:     empirical probability of loss (P(X < μ))
#' @importFrom moments skewness kurtosis
#' @importFrom stats median var
#' @export
calculate_revenue_moments <- function(x) {
  mu    <- mean(x,     na.rm = TRUE)
  md    <- median(x,   na.rm = TRUE)
  sd    <- sd(x,       na.rm = TRUE)
  cv    <- sd / mu
  vr    <- var(x,      na.rm = TRUE)
  sk    <- moments::skewness(x, na.rm = TRUE)
  ku    <- moments::kurtosis(x, na.rm = TRUE)
  res   <- x - mu
  lres2 <- mean(ifelse(res > 0, NA, res^2), na.rm = TRUE)
  cdf   <- mean(ifelse(res < 0, 1, 0), na.rm = TRUE)
  lapv  <- lres2
  lrpv  <- lapv / cdf
  nlapv <- lapv / mu
  nlrpv <- lrpv / mu
  
  data.frame(
    mu    = mu,
    md    = md,
    sd    = sd,
    cv    = cv,
    vr    = vr,
    sk    = sk,
    ku    = ku,
    lapv  = lapv,
    lrpv  = lrpv,
    nlapv = nlapv,
    nlrpv = nlrpv,
    lres2 = lres2,
    cdf   = cdf,
    stringsAsFactors = FALSE
  )
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
#'   Path to the top‐level directory containing raw yearly `.rds` files.
#'   Defaults to `"./data-raw/internal_datasets"`.
#' @param size_threshold   `numeric(1)`
#'   Maximum file size (in megabytes) allowed for inclusion.  Any dataset whose
#'   largest yearly file exceeds this threshold is skipped entirely.  Defaults to `1`.
#'   
#' @return
#' Invisibly returns `NULL`. Side effects include creation of `.rda` files in `./data/`
#' and generation of `R/helper_data.R` containing roxygen entries.
#'
#' @import dplyr
#' @importFrom magrittr %>%
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
build_internal_datasets <- function(dir_source = "./data-raw/internal_datasets", size_threshold = 1 ){
  
  # id "./data" doesn't exist, create it
  if(!dir.exists("./data")) {
    dir.create("./data")
  }
  
  file_info <- rmaADM:::get_file_info(directory =dir_source, file_suffix = ".rds")
  
  # Add a column with the file name without any of the parent folders
  file_info$file_name <- gsub(paste0(dir_source, "/"), "", file_info$file_path)
  file_info$file_name <- gsub("[0-9]{4}/", "", file_info$file_name)
  file_info$file_name <- gsub(".rds", "", file_info$file_name)
  
  # keep only files that are less than the size threshold (in MB). Applied to
  # maximum size over all years
  max_sizes <- file_info %>%
    group_by(.data$file_name) %>%
    summarize(max_size = max(.data$size_mb)) %>%
    filter(.data$max_size < size_threshold)
  
  file_info <- file_info %>%
    filter(.data$file_name %in% max_sizes$file_name)
  
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
    data <- file_paths %>%
      purrr::map(~ {
        df <- readRDS(.x)                 # read in the data frame
        df[] <- lapply(df, as.character) # convert all columns to character
        df                               # return the data frame
      }) %>%
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






