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
#' @param contiguity Either "rook" (shared edge) or "queen" (edge or corner). Default "rook".
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
    n_classes     = 10,
    contiguity    = c("rook","queen")) {
  
  contiguity <- match.arg(contiguity)
  
  counties_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE)
  
  by_arg <- stats::setNames(fip_col, "county_fips")
  map_sf <- dplyr::left_join(counties_sf, data, by = by_arg)
  
  # optional: keep for downstream compatibility
  counties_vect <- terra::vect(map_sf)
  
  # neighbors
  nbr_list <- if (contiguity == "rook") {
    sf::st_relate(map_sf, pattern = "F***1****")
  } else {
    sf::st_touches(map_sf)
  }
  
  sm <- map_sf[[estimate_col]]
  
  # we will only ever write into cells that were NA at the start
  missing_idx <- which(is.na(sm))
  
  for (i in seq_len(iterations)) {
    # which of the originally-missing are still NA?
    todo <- missing_idx[is.na(sm[missing_idx])]
    if (!length(todo)) break
    
    fill_vals <- vapply(todo, function(j) {
      ngb <- nbr_list[[j]]
      if (!length(ngb)) return(NA_real_)
      vals <- sm[ngb]
      vals <- vals[is.finite(vals)]
      if (!length(vals)) NA_real_ else fun(vals)
    }, numeric(1))
    
    # only write where we actually got a number
    write_idx <- todo[is.finite(fill_vals)]
    if (length(write_idx)) sm[write_idx] <- fill_vals[is.finite(fill_vals)]
  }
  
  map_sf$estimate_smooth <- sm
  
  # Jenks classification (cap k and silence harmless warnings)
  uniq_vals <- unique(sm[is.finite(sm)])
  if (!length(uniq_vals)) {
    map_sf$estimate_cat <- factor(NA_character_)
    return(map_sf)
  }
  k <- max(1L, min(n_classes, length(uniq_vals)))
  brks <- suppressWarnings(
    unique(classInt::classIntervals(uniq_vals, n = k, style = "jenks")$brks)
  )
  
  map_sf <- map_sf |>
    dplyr::mutate(
      estimate_cat = cut(
        estimate_smooth,
        breaks         = brks,
        include.lowest = TRUE,
        labels         = paste0(
          format(round(head(brks, -1), 2), nsmall = 2), "-",
          format(round(tail(brks, -1), 2), nsmall = 2)
        )
      )
    )
  
  map_sf
}



