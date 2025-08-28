#' Smooth county-level estimates via spatial contiguity
#'
#' This function
#'   1. Loads all U.S. county geometries via **urbnmapr**
#'   2. Joins in a user-supplied data.frame of county estimates (by 5-digit FIPS)
#'   3. (Optionally) converts to a **terra** SpatVector for further terra ops
#'   4. Builds a contiguity neighbor list using **sf::st_touches**
#'   5. Iteratively fills any missing estimates by applying a user-supplied function to neighbor values
#'   6. Classifies the smoothed values into `n_classes` using Jenks natural breaks 
#'
#' @param data         A data.frame or tibble containing at least two columns:
#'                       - a 5-digit FIPS code (character)
#'                       - a numeric estimate to be smoothed
#' @param fip_col      Name of the FIPS-code column in `fit_df` (default 'fip').
#' @param estimate_col Name of the numeric estimate column in `fit_df` (default 'estimate').
#' @param iterations   Number of smoothing passes to perform (default 5).
#' @param fun          A function to aggregate neighbor values; must take a numeric vector and return a single numeric (default mean).
#' @param n_classes    Number of categories for Jenks natural-breaks classification (default 10).
#'
#' @return An **sf** object of all U.S. counties with these new columns:  
#'   - estimate: your original values (NA where missing)  
#'   - estimate_smooth: the same values after neighborhood-function imputation  
#'   - estimate_cat: factor giving the Jenks-break category  
#'
#' @importFrom urbnmapr get_urbn_map
#' @importFrom stats setNames
#' @importFrom sf st_touches
#' @importFrom utils head tail
#' @importFrom terra vect
#' @importFrom classInt classIntervals
#' @export
smooth_county_estimates <- function(
    data,
    fip_col       = "fip",
    estimate_col  = "estimate",
    iterations    = 5,
    fun           = mean,
    n_classes     = 10) {
  # 1) load all U.S. counties
  counties_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE)
  
  # 2) join in user estimates by FIPS
  by_arg <- stats::setNames(fip_col, "county_fips")
  map_sf <- dplyr::left_join(counties_sf, data, by = by_arg)
  
  # 3) (optional) convert to terra::SpatVector
  counties_vect <- terra::vect(map_sf)
  
  # 4) build contiguity list via sf
  nbr_list <- sf::st_touches(map_sf)
  
  # 5) iterative neighbor-function smoothing
  sm      <- map_sf[[estimate_col]]
  missing <- which(is.na(sm))
  
  for (i in seq_len(iterations)) {
    sm_new <- sm
    sm_new[missing] <- sapply(missing, function(j) {
      ngb_vals <- sm[nbr_list[[j]]]
      ngb_vals <- ngb_vals[!is.na(ngb_vals)]
      if (length(ngb_vals) == 0) {
        NA_real_
      } else {
        fun(ngb_vals)
      }
    })
    sm <- sm_new
  }
  
  map_sf$estimate_smooth <- sm
  # 6) classify the smoothed estimates using Jenks natural breaks
  #    use unique values to avoid duplicate-break errors
  uniq_vals <- unique(sm[!is.na(sm)])
  brks      <- unique(classInt::classIntervals(uniq_vals,
                                               n = n_classes,
                                               style = "jenks")$brks)
  map_sf <- map_sf |>
    dplyr::mutate(
      estimate_cat = cut(
        estimate_smooth,
        breaks         = brks,
        include.lowest = TRUE,
        labels         = paste0(
          format(round(head(brks, -1), 2), nsmall = 2),
          "-",
          format(round(tail(brks, -1), 2), nsmall = 2)
        )
      )
    )
  
  map_sf
  
}