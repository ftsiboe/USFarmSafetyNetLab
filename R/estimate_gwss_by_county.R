#' Estimate geographically weighted summary statistics (GWSS) for counties
#'
#' Computes Geographically Weighted Summary Statistics (GWSS) for a scalar,
#' county-level variable observed in a subset of counties, then evaluates the
#' statistics at **all** county locations (points-on-surface). This is useful
#' for spatial smoothing and gap-filling (imputation) when some counties are
#' missing observations.
#'
#' The function:
#' 1) pulls U.S. counties from **urbnmapr** and projects to a suitable CRS;
#' 2) copies `data[[fip_col]]` into `"county_fips"` and joins to the map;
#' 3) fits GWSS using **only observed counties** (points) and selects an
#'    adaptive bandwidth by cross-validation on a random subsample sized by
#'    `draw_rate`;
#' 4) evaluates GWSS at **all counties** and returns local summaries keyed by
#'    `county_fips`.
#'
#' @details
#' **Inputs**: `data` must contain a county identifier column referenced by
#' `fip_col` and a numeric column referenced by `variable`. Internally, a column
#' `"county_fips"` is created for joining with
#' `urbnmapr::get_urbn_map("counties")`.
#'
#' **Distance metric** (`distance_metric`) defines `(p, theta, longlat)` for
#' `GWmodel::gw.dist()`. Use `gw_distance_metric_names()` to list options.
#' If `longlat = TRUE` (e.g., `"Great Circle"`), counties are transformed to
#' EPSG:4326; otherwise to `target_crs` (default 5070, meters).
#'
#' **Bandwidth selection**: CV is run on a uniform random subsample of size
#' `ceiling(draw_rate * n_obs)`, bounded to `[5, n_obs - 1]`. Call `set.seed()`
#' beforehand for reproducibility. Returns `NULL` (with a message) if fewer than
#' 5 counties have finite values.
#'
#' @param data A `data.frame`/`data.table` with at least `fip_col` and `variable`.
#' @param fip_col Character. Name of the county ID column in `data`; copied to `"county_fips"`.
#' @param variable Character. Name of the numeric column in `data` to summarize.
#' @param distance_metric Character. One of [gw_distance_metric_names()]. Default: `"Euclidean"`.
#' @param kernel Character. One of `"gaussian"`, `"exponential"`, `"bisquare"`, `"boxcar"`, `"tricube"`. Default: `"gaussian"`.
#' @param target_crs Integer EPSG used to project county geometries when `longlat = FALSE`. Default: **5070** (NAD83 / CONUS Albers, meters).
#' @param draw_rate Numeric in (0, 1]. Fraction of observed counties used during bandwidth cross-validation. Default: **0.5** (50%).
#' @param approach Character. Bandwidth selection approach passed to `GWmodel::bw.gwr()`. One of `"CV"`, `"AIC"`, `"AICc"`. Default: `"CV"`.
#' @param adaptive Logical. Use adaptive (nearest-neighbour count) bandwidth instead of fixed distance. Default: `TRUE`.
#'
#' @return A `data.table` of GW summary statistics for **all counties**, with a
#'   `county_fips` column for merging back to polygons. Column names follow
#'   **GWmodel** conventions for the internal `value` variable (created from
#'   `variable`), e.g. `value_LM`, `value_LSD`, `value_LCV`, `value_LSKe`,
#'   `value_LSSke`, etc. Attributes attached: `"bandwidth"`, `"distance_params"`,
#'   `"kernel"`, `"approach"`, `"adaptive"`. Returns `NULL` when there are
#'   < 5 observed counties.
#'
#' @section Imputation workflow:
#' \preformatted{
#' set.seed(123)
#' gw <- estimate_gwss_by_county(
#'   data = my_data, fip_col = "county_fips", variable = "my_var"
#' )
#' sf_out <- counties_sf |>
#'   dplyr::left_join(gw[, c("county_fips","value_LM")], by = "county_fips") |>
#'   dplyr::mutate(my_var_imputed = dplyr::if_else(is.finite(my_var), my_var, value_LM))
#' }
#' 
#' @import GWmodel data.table
#' @importFrom urbnmapr get_urbn_map
#' @importFrom sf st_transform st_point_on_surface
#' @importFrom sp coordinates
#' @export
estimate_gwss_by_county <- function(
    data,
    fip_col,
    variable,
    distance_metric = "Euclidean",
    kernel         = "gaussian",
    target_crs     = 5070,
    draw_rate      = 0.5,
    approach       = "CV",
    adaptive       = TRUE
){
  # --- Validate args -----------------------------------------------------------
  if (missing(data) || is.null(data)) {
    stop("Argument `data` must be supplied.")
  }
  if (is.null(fip_col) || is.null(variable)) {
    stop("Input is missing one of: `fip_col`, `variable`.")
  }
  
  allowed_kernels  <- c("gaussian","exponential","bisquare","boxcar","tricube")
  if (!kernel %in% allowed_kernels) {
    stop("`kernel` must be one of: ", paste(allowed_kernels, collapse = ", "))
  }
  
  allowed_approach <- c("CV","AIC","AICc")
  if (!approach %in% allowed_approach) {
    stop("`approach` must be one of: ", paste(allowed_approach, collapse = ", "))
  }
  
  dm <- resolve_distance_metric(distance_metric)
  p <- dm$p; theta <- dm$theta; longlat <- dm$longlat
  
  # --- Coerce to data.table & prepare columns ---------------------------------
  data <- data.table::as.data.table(data)
  if (!all(c(fip_col, variable) %in% names(data))) {
    stop("`data` must contain columns: ", fip_col, " and ", variable, ".")
  }
  
  data[, county_fips := as.character(get(fip_col))]
  data[, value := get(variable)]
  data <- data[is.finite(value) & !is.na(county_fips) & nzchar(county_fips)]
  data <- unique(data, by = "county_fips")  # 1 row per county
  
  # --- Geometry & CRS ----------------------------------------------------------
  counties_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE)
  counties_sf <- if (isTRUE(longlat)) {
    sf::st_transform(counties_sf, crs = 4326)   # lon/lat for great-circle
  } else {
    sf::st_transform(counties_sf, crs = target_crs)
  }
  
  # --- Join attributes; keep ALL counties --------------------------------------
  sf_join <- counties_sf |>
    dplyr::left_join(as.data.frame(data), by = "county_fips")
  
  # Observed subset for fitting
  sf_obs <- sf_join |> dplyr::filter(is.finite(value))
  if (nrow(sf_obs) < 5L) {
    message("Too few observed counties to fit GW smoothing.")
    return(NULL)
  }
  
  # --- Polygons -> Points (summary = all; data = observed) ---------------------
  pts_sf_all <- sf::st_point_on_surface(sf_join)
  pts_sp_all <- as(pts_sf_all, "Spatial")
  stopifnot(inherits(pts_sp_all, "SpatialPointsDataFrame"))
  
  pts_sf_obs <- sf::st_point_on_surface(sf_obs)
  pts_sp_obs <- as(pts_sf_obs, "Spatial")
  stopifnot(inherits(pts_sp_obs, "SpatialPointsDataFrame"))
  
  coords_all <- sp::coordinates(pts_sp_all)  # n_all x 2
  coords_obs <- sp::coordinates(pts_sp_obs)  # n_obs x 2
  
  # --- Bandwidth via CV on 50% subsample (bounded) -----------------------------
  n_obs <- nrow(pts_sp_obs)
  n_sub <- min(n_obs - 1L, max(5L, ceiling(draw_rate * n_obs)))  # [5, n_obs-1]
  sub_ids <- sample.int(n_obs, n_sub)
  
  pts_sp_sub <- pts_sp_obs[sub_ids, ]
  coords_sub <- coords_obs[sub_ids, , drop = FALSE]
  
  dMat_sub <- GWmodel::gw.dist(
    dp.locat = coords_sub, rp.locat = coords_sub,
    p = p, theta = theta, longlat = longlat
  )
  
  bw <- GWmodel::bw.gwr(
    formula  = value ~ 1,
    data     = pts_sp_sub,
    approach = approach,
    adaptive = adaptive,
    kernel   = kernel,
    p        = p, theta = theta, longlat = longlat,
    dMat     = dMat_sub
  )
  
  # --- GW summary at ALL counties (summary.locat = SpatialPoints) --------------
  dMat_os <- GWmodel::gw.dist(
    dp.locat = coords_obs,  # observed
    rp.locat = coords_all,  # all counties
    p = p, theta = theta, longlat = longlat
  )
  
  if (!all(dim(dMat_os) == c(nrow(pts_sp_obs), nrow(pts_sp_all)))) {
    stop("dMat_os has unexpected dimensions. Expected ",
         nrow(pts_sp_obs), " x ", nrow(pts_sp_all), ".")
  }
  
  gwss_obj <- GWmodel::gwss(
    data          = pts_sp_obs,
    summary.locat = pts_sp_all,    # SpatialPoints* avoids sp.locat error
    bw            = bw,
    vars          = "value",
    kernel        = kernel,
    adaptive      = adaptive,
    p             = p, theta = theta, longlat = longlat,
    dMat          = dMat_os,
    quantile      = FALSE
  )
  
  gw_df <- as.data.frame(gwss_obj$SDF@data)
  gw_df$county_fips <- pts_sp_all@data[["county_fips"]]
  data.table::setDT(gw_df)
  
  # Attach useful attributes to result
  attr(gw_df, "bandwidth")       <- bw
  attr(gw_df, "distance_params") <- list(p = p, theta = theta, longlat = longlat)
  attr(gw_df, "kernel")          <- kernel
  attr(gw_df, "approach")        <- approach
  attr(gw_df, "adaptive")        <- adaptive
  
  gw_df
}



#' GW distance metric presets for GWmodel
#'
#' Provides a curated set of distance metric presets (Minkowski family and great-circle)
#' for \pkg{GWmodel}. Each preset specifies \code{(p, theta, longlat)} for
#' \code{GWmodel::gw.dist()}.
#'
#' @return A named list of presets, each entry a \code{list(p, theta, longlat)}.
#' @export
gw_distance_metric_presets <- function() {
  list(
    # Euclidean / L2
    "Euclidean"                       = list(p = 2.0,  theta = 0.0, longlat = FALSE),
    "Euclidean (rotated theta=0.8)"       = list(p = 2.0,  theta = 0.8, longlat = FALSE), # rotation no-op for p=2
    
    # Manhattan / L1
    "Manhattan"                       = list(p = 1.0,  theta = 0.0, longlat = FALSE),
    "Manhattan (rotated theta=0.5)"       = list(p = 1.0,  theta = 0.5, longlat = FALSE),
    
    # Minkowski (general Lp)
    "Minkowski p=1.5"                 = list(p = 1.5,  theta = 0.0, longlat = FALSE),
    "Minkowski p=1.5 (rotated theta=0.8)" = list(p = 1.5,  theta = 0.8, longlat = FALSE),
    "Minkowski p=3"                   = list(p = 3.0,  theta = 0.0, longlat = FALSE),
    "Minkowski p=3 (rotated theta=0.8)"   = list(p = 3.0,  theta = 0.8, longlat = FALSE),
    
    # Chebyshev / L_inf (approx via large p)
    "Chebyshev (approx L_inf, p = 10)"     = list(p = 10.0, theta = 0.0, longlat = FALSE),
    
    # Geodesic
    "Great Circle"                    = list(p = 2.0,  theta = 0.0, longlat = TRUE)
  )
}

#' Resolve a GW distance metric preset
#' @param name Character scalar. One of \code{gw_distance_metric_names()}.
#' @param stop_on_error Logical. If \code{TRUE}, throw for unknown names; else \code{NULL}.
#' @return \code{list(p, theta, longlat)} or \code{NULL}.
#' @export
resolve_distance_metric <- function(name, stop_on_error = TRUE) {
  presets <- gw_distance_metric_presets()
  dm <- presets[[name]]
  if (is.null(dm) && isTRUE(stop_on_error)) {
    stop("Unknown `distance_metric`: ", name,
         "\nAvailable: ", paste(names(presets), collapse = ", "))
  }
  dm
}

#' List valid GW distance metric names
#' @return Character vector of valid preset names.
#' @export
gw_distance_metric_names <- function() {
  names(gw_distance_metric_presets())
}

