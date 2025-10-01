#' Download and cache USDA NASS Quick Stats large dataset files
#'
#' @description
#' `downloaded_nass_large_datasets()` retrieves a Quick Stats file from the USDA National Agricultural Statistics Service (NASS)
#' https://www.nass.usda.gov/datasets/ page and saves it locally.  If the file is already present in the target directory, it is not re-downloaded.
#'
#' @param large_datasets `character list`
#'   The base name of the Quick Stats file to download.  For example, use `"crops"` to fetch
#'   `qs.crops_YYYYMMDD.txt.gz` or include `"census2022"` (e.g. `"census2022"`) to fetch the gzipped 2022 census version
#'   (`qs.census2022.txt.gz`). any of:
#'   "census2002","census2007","census2012","census2017","census2022",
#'   "census2007zipcode","census2017zipcode",
#'   "animals_products","crops","demographics","economics","environmental"
#' @param dir_dest `character(1)`
#'   Path to a directory where downloaded files will be stored.  Defaults to `"./data-raw/fastscratch/nass/"`.
#'
#' @return
#' Invisibly returns the normalized file large_dataset (e.g. `"qs.crops_YYYYMMDD.txt.gz"` or `"qs.censusYYYY.txt.gz"`) that was
#' downloaded or already present.
#'
#' @details
#' 1. Prepends `"qs."` to the provided `large_dataset`.  If `large_dataset` contains `"census"`, appends `".txt.gz"`,
#'    otherwise `NULL`.
#' 2. Ensures `dir_dest` exists (creates it if needed).
#' 3. Scrapes the NASS datasets page (`https://www.nass.usda.gov/datasets/`) for links ending in `.txt.gz`.
#' 4. Downloads the matching file into `dir_dest` if not already present.
#'
#' @importFrom xml2 read_html
#' @importFrom utils download.file
#' @family USDA NASS Quick Stats 
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 'crops' dataset if not already cached:
#' downloaded_nass_large_datasets(large_dataset = "crops")
#'
#' # Download the 2022 census version:
#' downloaded_nass_large_datasets(large_dataset = "census2022", 
#' dir_dest = "./data-raw/fastscratch/nass/")
#' }
downloaded_nass_large_datasets <- function(large_datasets, dir_dest = "./data-raw/fastscratch/nass/"){
  
  # Create target directory if needed
  if (!dir.exists(dir_dest)) {
    dir.create(dir_dest, recursive = TRUE)
  }
  
  lapply(
    large_datasets,
    function(large_dataset){
      tryCatch({
        # Normalize the file large_dataset
        if (grepl("census", large_dataset)) {
          file_name <- paste0("qs.", large_dataset,".txt.gz")
        } else {
          file_name <- paste0("qs.", large_dataset)
        }
        
        # Scrape available dataset URLs
        base_url <- "https://www.nass.usda.gov"
        dataset_page <- xml2::read_html(paste0(base_url, "/datasets/"))
        hrefs <- dataset_page |>
          rvest::html_nodes("a") |>
          rvest::html_attr("href")
        txt_links <- hrefs[grepl("\\.txt", hrefs)]
        qs_urls <- txt_links[grepl("datasets", txt_links)]
        
        # Identify the specific URL for this dataset
        matched <- qs_urls[grepl(file_name, qs_urls)]
        dest_file <- file.path(dir_dest, gsub("^/datasets/", "", matched))
        
        # Download if not already present
        if (!basename(dest_file) %in% list.files(dir_dest, pattern = file_name)) {
          
          if(!grepl("census",file_name)){
            unlink(list.files(dir_dest, pattern = file_name,full.names = TRUE))
          }
          
          download.file(
            url      = paste0(base_url, matched),
            destfile = dest_file,
            mode     = "wb"
          )
        }
      }, error=function(e){})
      return(large_dataset)})
  
  return(list.files(dir_dest))
}
