#' Geocode location names (ZIP-centroid first, optional Census fallback)
#'
#' @description
#' Adds latitude and longitude coordinates to a dataset that contains a column
#' (e.g., `location_name`) with address-like text such as
#' `"AGREX, MOBILE, AL 36602"`.  
#' The function first uses offline ZIP centroid coordinates from
#' \pkg{zipcodeR}, and can optionally fall back to online geocoding using the
#' U.S. Census API via \pkg{tidygeocoder} (\code{method = "census"}).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts a 5-digit ZIP code from the specified location column.
#'   \item Joins ZIP-level latitude and longitude from \pkg{zipcodeR}.
#'   \item Marks matched rows with \code{geocode_method = "zip_centroid"}.
#'   \item If \code{use_fallback = TRUE}, uses the U.S. Census API through
#'         \pkg{tidygeocoder} for rows where ZIP-based coordinates are missing,
#'         and updates \code{geocode_method = "census"}.
#' }
#'
#' This hybrid approach ensures high speed and reproducibility for most
#' locations (via ZIP lookup) and full U.S. coverage when combined with the
#' Census fallback.
#'
#' @param data A \code{data.frame} or \code{data.table}.
#' @param location_col Column in \code{data} that contains location strings such
#'   as `"AGREX, MOBILE, AL 36602"`. Can be specified as a bare name or a string.
#' @param use_fallback Logical; if \code{TRUE}, perform fallback geocoding for
#'   rows without ZIP-centroid coordinates using the U.S. Census API.
#'   Default = \code{FALSE}.
#' @param fallback_method Character; the geocoding provider passed to
#'   \code{tidygeocoder::geocode()}. Default = \code{"census"}.
#' @param respect_existing_state If TRUE and `state_abbreviation` exists,
#'
#' @return
#' A modified version of \code{data} with additional columns:
#' \itemize{
#'   \item \code{zip} - Extracted 5-digit ZIP code.
#'   \item \code{lat}, \code{lon} - Latitude and longitude.
#'   \item \code{geocode_method} - The geocoding source ("zip_centroid" or "census").
#' }
#'
#' @importFrom rlang ensym as_name sym
#' @importFrom stringr str_extract str_trim
#' @importFrom tidygeocoder geocode
#' @importFrom zipcodeR geocode_zip
#' @export
geocode_locations <- function(
    data,
    location_col,
    use_fallback = FALSE,
    fallback_method = "census",
    respect_existing_state = TRUE
){
  
  # --- tidy-eval on column name ---
  loc_sym  <- rlang::ensym(location_col)
  loc_name <- rlang::as_name(loc_sym)
  
  # normalize input
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$.row_id <- seq_len(nrow(data))  # preserve order & identity
  
  if (!loc_name %in% names(data)) stop("Column '", loc_name, "' not found.")
  
  # 1) extract ZIP and join ZIP centroids
  data$zip <- stringr::str_extract(data[[loc_name]], "\\b\\d{5}\\b")
  
  zipdb <- zipcodeR::zip_code_db |>
    dplyr::transmute(zip = zipcode, lat = lat, lon = lng)
  
  data <- dplyr::left_join(data, zipdb, by = "zip") |>
    dplyr::mutate(
      geocode_method = dplyr::if_else(!is.na(lat) & !is.na(lon),
                                      "zip_centroid", NA_character_)
    )
  
  # 2) optional fallback: Census via tidygeocoder
  if (use_fallback) {
    if (!requireNamespace("tidygeocoder", quietly = TRUE))
      stop("Fallback requested but 'tidygeocoder' is not installed.")
    need_geo <- data |>
      dplyr::filter(is.na(lat) | is.na(lon)) |>
      dplyr::distinct(.data[[loc_name]]) |>
      dplyr::mutate(
        addr = stringr::str_trim(
          stringr::str_extract(.data[[loc_name]],
                               "[^,]+,\\s*[A-Z]{2}\\s*\\d{5}\\b$")
        )
      )
    need_geo$addr[is.na(need_geo$addr)] <- need_geo[[loc_name]][is.na(need_geo$addr)]
    
    if (nrow(need_geo) > 0) {
      geo_res <- tidygeocoder::geocode(
        need_geo,
        address = addr,
        method  = fallback_method,
        limit   = 1
      )
      geo_join <- geo_res |>
        dplyr::transmute(!!loc_sym := .data[[loc_name]],
                         lat_f = lat, lon_f = long)
      
      data <- data |>
        dplyr::left_join(geo_join, by = loc_name) |>
        dplyr::mutate(
          lat = dplyr::coalesce(lat, lat_f),
          lon = dplyr::coalesce(lon, lon_f),
          geocode_method = dplyr::case_when(
            geocode_method == "zip_centroid" ~ geocode_method,
            !is.na(lat_f) & !is.na(lon_f)    ~ fallback_method,
            TRUE                             ~ geocode_method
          )
        ) |>
        dplyr::select(-lat_f, -lon_f)
    }
  }
  
  # 3) spatial join to county/state (NO dedupe; keep per-row mapping)
  pts <- data |>
    dplyr::mutate(lat = as.numeric(lat), lon = as.numeric(lon)) |>
    dplyr::filter(!is.na(lat) & !is.na(lon)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  counties <- urbnmapr::get_urbn_map("counties", sf = TRUE)[
    , c("county_fips", "county_name", "state_name", "state_abbv", "state_fips")]
  
  # align CRS
  pts <- sf::st_transform(pts, sf::st_crs(counties))
  
  # use S2 safely
  sf::sf_use_s2(TRUE)
  
  joined <- sf::st_join(pts, counties, join = sf::st_within, left = TRUE)
  
  # fallback: nearest polygon if exactly on boundary/water
  need_fix <- which(is.na(joined$county_fips))
  if (length(need_fix)) {
    nearest <- sf::st_nearest_feature(joined[need_fix, ], counties)
    joined[need_fix, c("county_fips","county_name","state_name","state_abbv","state_fips")] <-
      counties[nearest, c("county_fips","county_name","state_name","state_abbv","state_fips")]
  }
  
  geom_cols <- c("county_fips","county_name","state_name","state_abbv","state_fips")
  out_geo <- joined |>
    sf::st_drop_geometry() |>
    dplyr::select(.row_id, dplyr::all_of(geom_cols))
  
  # 4) merge back to original rows by .row_id (no duplication)
  data <- as.data.frame(dplyr::left_join(data, out_geo, by = ".row_id"))
  
  # tidy up
  data <- data[names(data)[!grepl("state_abbreviation",names(data))]]
  data <- data[order(data$.row_id), ]
  data$.row_id <- NULL
  data
}