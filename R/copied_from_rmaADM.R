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
#' @source copied from https://github.com/dylan-turner25/rmaADM/blob/main/R/helpers.R
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


#' Locate the download link for the actuarial data master
#'
#' @param year the year of the actuarial data master to download
#' @param adm_url the url where the ADM FTP site is
#' @param ice_url the url where the ICE (insurance control elements) FTP site is
#' @param data_source either "adm" or "ice". Defaults to "adm".
#'
#' @returns a list of the data and layout file urls with the time the file was last updated on RMA's server
#'
#' @importFrom stringr str_match_all str_extract
#' @importFrom stats time
#' @source copied from https://github.com/dylan-turner25/rmaADM/blob/main/R/helpers.R
locate_download_link <- function(year = 2012,
                                 adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
                                 ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                                 data_source = "adm"){
  
  if(data_source == "ice"){
    url <- ice_url
  }
  
  if(data_source == "adm"){
    url <- adm_url
  }
  
  # read in the webpage
  html <- suppressWarnings(paste0(readLines(url), collapse = "\n"))
  
  # locate all the links
  links <- as.character(data.frame(stringr::str_match_all(html,
                                                          "href=\"(.*?)\""))[, 1])
  
  # get the link with the matching year
  link <- links[grepl(year,links)]
  link <- link[!grepl("test",link)] # for ICE links
  
  
  # apply some cleaning opperations
  link <- gsub("href=\"", "", link)
  link <- gsub("\"", "", link)
  link <- gsub("\\./", "", link)
  link <- paste0(url, link)
  
  # navigate the the cleaned link to get the correct sublink
  html <- suppressWarnings(paste0(readLines(link), collapse = "\n"))
  
  # Extract the <pre> block where the file info resides
  pre_block <- str_extract(html, "<pre>.*?</pre>")
  
  # Extract date, time, size, and filename from each line using regex
  matches <- str_match_all(
    pre_block,
    "(\\d{2}/\\d{2}/\\d{4})\\s+(\\d{2}:\\d{2}\\s+[AP]M)\\s+(\\d+)\\s+<a href=\"\\.\\/(.*?)\">"
  )[[1]]
  
  # Convert to data frame
  file_info <- data.frame(
    date = matches[, 2],
    time = matches[, 3],
    size_bytes = as.numeric(matches[, 4]),
    filename = matches[, 5],
    stringsAsFactors = FALSE
  )
  
  
  # combine date and time into a single POSIXct column
  file_info <- file_info |>
    dplyr::mutate(
      datetime = as.POSIXct(paste(date, time), format = "%m/%d/%Y %I:%M %p", tz = "EST")
    )
  
  # filter the file info to only include the data and layout files
  file_info <- file_info |>
    dplyr::filter(grepl("ytd|layout", tolower(filename)))
  
  
  # add the base url and year to the file names
  file_info <- file_info |>
    dplyr::mutate(filename = paste0(url,year,"/",filename))
  
  # convert the links to a list and name them
  links <- as.list(file_info$filename)
  
  # name the link that contains "YTD" as "data"
  names(links)[which(grepl("YTD|ytd",links))] <- "data"
  
  # name the link that contains "Layout" as "layout"
  names(links)[which(grepl("Layout|layout",links))] <- "layout"
  
  # add the update date to the links
  links$update_date <- file_info$datetime[grepl("YTD",file_info$filename)]
  
  # unlist layout and data links
  links$data <- unlist(links$data)
  links$layout <- unlist(links$layout)
  
  # return the links
  return(links)
  
}

#' Internal helper to download and verify a ZIP file
#'
#' Attempts to download a ZIP archive from a given URL up to a specified number of times,
#' and verifies that it is a valid ZIP by listing its contents. If the download or verification
#' fails after all attempts, an error is raised.
#'
#' @param url A character string giving the URL of the ZIP file to download.
#' @param destfile A character string giving the path (including filename) where the downloaded
#'   file should be saved locally.
#' @param method Optional character string specifying the download method to pass to
#'   \code{\link[utils]{download.file}} (e.g. "curl", "libcurl", "wget"). If \code{NULL},
#'   R chooses the default.
#' @param attempts Integer number of times to retry the download and verification before
#'   giving up. Defaults to 3.
#'
#' @return Invisibly returns \code{TRUE} if the file was successfully downloaded and verified.
#'   If all attempts fail, the function throws an error.
#'
#' @keywords internal
#' @noRd
download_and_verify <- function(url, destfile, method = NULL, attempts = 3) {
  for (i in seq_len(attempts)) {
    # try a download
    try(utils::download.file(
      url, destfile = destfile,
      mode   = "wb",
      method = method,
      quiet  = TRUE
    ), silent = TRUE)
    
    info <- file.info(destfile)
    # did we at least get a non‐zero file?
    if (!is.na(info$size) && info$size > 0) {
      # try listing the zip contents
      z <- try(utils::unzip(destfile, list = TRUE), silent = TRUE)
      if (inherits(z, "data.frame") && nrow(z) > 0) {
        return(invisible(TRUE))
      }
    }
    
    # otherwise wait a bit and retry
    Sys.sleep(1)
  }
  stop("Failed to download or verify zip from ", url)
}

#' Apply standardized data cleaning opperations
#'
#' @param df a data frame to clean
#'
#' @returns a cleaned data frame
#' @importFrom janitor clean_names
#' @importFrom readr type_convert
#' @source copied from https://github.com/dylan-turner25/rmaADM/blob/main/R/helpers.R
clean_adm_data <- function(df){
  
  # clean column names
  df <- janitor::clean_names(df)
  
  # Sanitize all character data to ensure valid UTF-8 encoding
  char_cols <- sapply(df, is.character)
  for(col in names(df)[char_cols]) {
    # First try UTF-8 cleaning, then fall back to ASCII if needed
    df[[col]] <- iconv(df[[col]], from = "", to = "UTF-8", sub = "")
    # If still problematic, convert to ASCII
    df[[col]] <- iconv(df[[col]], to = "ASCII//TRANSLIT", sub = "")
  }
  
  # enforce data types
  df <- suppressMessages(readr::type_convert(df))
  
  # identify date
  date_cols <- grep("date", names(df), value = TRUE)
  
  # try to parse dates
  for(col in date_cols){
    input = as.character(df[[col]]) # convert to character
    input <- as.character(gsub("[^0-9]", "", input)) # remove non-numeric characters
    try({
      converted_dates <- readr::parse_date(input, format = "%Y%m%d", na = c("", "NA"))
      # if converted dates are not all NA
      if(!all(is.na(converted_dates))) {
        df[[col]] <- converted_dates
      }
    })
  }
  
  
  # Convert key columns to numeric if they exist
  numeric_cols <- intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))
  if(length(numeric_cols) > 0) {
    for(col in numeric_cols) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    }
  }
  
  # return the df
  return(df)
  
}