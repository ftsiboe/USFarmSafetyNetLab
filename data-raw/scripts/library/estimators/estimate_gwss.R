#' Estimate geographically weighted summary statistics (GWSS) for points
#'
#' @description
#' Computes geographically weighted summary statistics (via **GWmodel::gwss**) at
#' specified prediction locations using a **single global bandwidth** determined
#' from a subsample of observed points. All observed points are used for every
#' prediction; `group_variable` only partitions the prediction set for
#' organizational or computational reasons.
#'
#' @details
#' - Coordinates are assumed to be in **WGS84 (EPSG:4326)**.
#' - Prediction points without any observed points within `feasible_radius`
#'   miles are excluded.
#' - Bandwidth is estimated **once** via `GWmodel::bw.gwr()` on a random subsample
#'   (size = `draw_rate * n_obs`, bounded to 5, n_obs - 1).
#' - `group_variable`, if provided, splits the prediction set for parallel or
#'   modular computation, but the same global bandwidth and all observed points
#'   are used for each group.
#'
#' @param locat_observed `data.frame`/`data.table` with columns named by
#'   `longitude_col`, `latitude_col`, and a numeric column named by `variable`.
#'   Must be in decimal degrees (EPSG:4326).
#' @param locat_predict `data.frame`/`data.table` with columns named by
#'   `longitude_col` and `latitude_col`. Must be in decimal degrees (EPSG:4326).
#'   Rows with no nearby observations (within `feasible_radius`) are dropped.
#' @param longitude_col,latitude_col `character` column names for coordinates.
#' @param variable `character` name of numeric column in `locat_observed` to summarize.
#' @param distance_metric `character` distance metric passed to
#'   `GWmodel::gw.dist()` via `resolve_distance_metric()`. Default `"Euclidean"`.
#' @param kernel `character` kernel for GWR/GWSS weights. One of
#'   `"gaussian"`, `"exponential"`, `"bisquare"`, `"boxcar"`, `"tricube"`.
#'   Default `"gaussian"`.
#' @param target_crs `integer` EPSG code used for projection during radius
#'   pre-screening (in meters). Default `5070` (NAD83 / CONUS Albers).
#' @param draw_rate `numeric` fraction (0,1] of observed points used for
#'   bandwidth selection. Default `0.5`.
#' @param approach `character` one of `"CV"`, `"AIC"`, `"AICc"`, passed to
#'   `GWmodel::bw.gwr()`. Default `"CV"`.
#' @param adaptive `logical` whether to use adaptive (kNN) bandwidth. Default `TRUE`.
#' @param feasible_radius `numeric` radius in **miles** for pre-screening prediction
#'   locations. Default `100`.
#' @param group_variable `character` or `NULL`. Optional column in `locat_predict`
#'   used to partition prediction points; observed points are always global.
#' @param identifiers `character` vector of column names from `locat_predict` to
#'   carry through to the output.
#'
#' @return A `data.table` containing:
#' \itemize{
#'   \item GWSS summary statistics (e.g., local mean, SD, etc.)
#'   \item `longitude`, `latitude` - coordinates of prediction points
#'   \item `block_variable` - group label or 1L
#'   \item `bandwidth` - estimated global bandwidth
#'   \item `p`, `theta`, `longlat`, `approach`, `adaptive`, `variable` - metadata
#'   \item any columns from `identifiers`
#' }
#' Returns an empty `data.table` if no prediction locations pass the radius screen.
#'
#' @section Implementation notes:
#' - Coordinates must be valid WGS84 (longitude = x, latitude = y).
#' - Radius pre-screen is computed using projected distances (in meters).
#' - Global bandwidth ensures comparability across groups.
#' - Random seed (`set.seed(1L)`) ensures reproducibility of CV subsampling.
#'
#' @references
#' Fotheringham, A. S., Brunsdon, C., & Charlton, M. (2002).
#' *Geographically Weighted Regression: The Analysis of Spatially Varying Relationships.*
#' John Wiley & Sons.
#'
#' @import data.table 
#' @export
estimate_gwss_by_point <- function(
    locat_observed,
    locat_predict,
    longitude_col,
    latitude_col,
    variable,
    distance_metric = "Euclidean",
    kernel          = "gaussian",
    target_crs      = 5070,
    draw_rate       = 0.5,
    approach        = "CV",
    adaptive        = TRUE,
    feasible_radius = 100,
    group_variable  = NULL,
    identifiers     = NULL
){
  
  # locat_observed  = df
  # locat_predict   = gridCountyLinker[commodity_name %in% "CORN"]
  # longitude_col   = "lon"
  # latitude_col    = "lat"
  # variable        = "cash_price_open"
  # distance_metric = "Euclidean"
  # kernel          = "gaussian"
  # target_crs      = 5070
  # draw_rate       = 0.5
  # approach        = "CV"
  # adaptive        = TRUE
  # feasible_radius = 100
  # group_variable  = "state_code"
  # identifiers     = "grid_id"
  
  
  # ---- Validate
  if (missing(locat_observed) || is.null(locat_observed)) stop("`locat_observed` is required.")
  if (missing(locat_predict)  || is.null(locat_predict))  stop("`locat_predict` is required.")
  if (is.null(longitude_col) || is.null(latitude_col) || is.null(variable))
    stop("`longitude_col`, `latitude_col`, and `variable` must be character column names.")
  
  allowed_kernels  <- c("gaussian","exponential","bisquare","boxcar","tricube")
  allowed_approach <- c("CV","AIC","AICc")
  if (!kernel   %in% allowed_kernels)  stop("`kernel` must be one of: ", paste(allowed_kernels,  collapse=", "))
  if (!approach %in% allowed_approach) stop("`approach` must be one of: ", paste(allowed_approach, collapse=", "))
  
  dm <- resolve_distance_metric(distance_metric)
  p <- dm$p; theta <- dm$theta; longlat <- dm$longlat
  
  # ---- Coerce & column checks
  locat_observed <- data.table::as.data.table(data.table::copy(locat_observed))
  locat_predict  <- data.table::as.data.table(data.table::copy(locat_predict))
  
  if (!all(c(longitude_col, latitude_col, variable) %in% names(locat_observed))) {
    stop("`locat_observed` must contain: ", paste(c(longitude_col, latitude_col, variable), collapse=", "))
  }
  if (!all(c(longitude_col, latitude_col) %in% names(locat_predict))) {
    stop("`locat_predict` must contain: ", paste(c(longitude_col, latitude_col), collapse=", "))
  }
  if (!is.null(group_variable) && !group_variable %in% names(locat_predict)) {
    stop("`group_variable` not found in `locat_predict`.")
  }
  if (!is.null(identifiers)) {
    missing_ids <- setdiff(identifiers, names(locat_predict))
    if (length(missing_ids))
      stop("These `identifiers` are missing from `locat_predict`: ", paste(missing_ids, collapse=", "))
  }
  
  # ---- Standardize columns (avoid get())
  data.table::setnames(locat_observed, old = c(longitude_col, latitude_col, variable),
                       new = c("longitude","latitude","value"))
  data.table::setnames(locat_predict,  old = c(longitude_col, latitude_col),
                       new = c("longitude","latitude"))
  
  # keep a copy of original numeric coords if you want them untouched
  locat_predict[, longitude_flag := longitude]
  locat_predict[, latitude_flag  := latitude]
  
  # ---- Clean observed & collapse duplicates
  locat_observed <- locat_observed[
    is.finite(value) & !is.na(longitude) & !is.na(latitude),
    .(value = mean(value, na.rm = TRUE)),
    by = .(longitude, latitude)
  ]
  
  # ---- Build Spatial (sp) objects once
  sp::coordinates(locat_observed) <- stats::as.formula("~ longitude + latitude")
  sp::coordinates(locat_predict)  <- stats::as.formula("~ longitude + latitude")
  
  # ---- Radius pre-screen (metric CRS)
  obs_sf <- sf::st_as_sf(locat_observed); sf::st_crs(obs_sf) <- 4326
  all_sf <- sf::st_as_sf(locat_predict);  sf::st_crs(all_sf) <- 4326
  obs_m  <- sf::st_transform(obs_sf, target_crs)
  all_m  <- sf::st_transform(all_sf, target_crs)
  
  radius_m <- feasible_radius * 1609.344
  idx_list <- sf::st_is_within_distance(all_m, obs_m, dist = radius_m)
  locat_predict@data$n_obs_within_radius <- lengths(idx_list)
  locat_predict <- locat_predict[locat_predict@data$n_obs_within_radius > 0, ]
  if (nrow(locat_predict) == 0L) return(data.table::data.table())
  
  # ---- Global bandwidth via CV (observed subsample)
  coords_obs <- sp::coordinates(locat_observed)
  n_obs <- nrow(locat_observed)
  if (n_obs < 6L) stop("Not enough observed points after cleaning (need >= 6 for CV).")
  
  n_sub <- min(n_obs - 1L, max(5L, ceiling(draw_rate * n_obs)))
  set.seed(1L)
  sub_ids    <- sample.int(n_obs, n_sub)
  coords_sub <- coords_obs[sub_ids, , drop = FALSE]
  pts_sp_sub <- locat_observed[sub_ids, ]
  
  Dobs_sub <- GWmodel::gw.dist(
    dp.locat = coords_sub, rp.locat = coords_sub,
    p = p, theta = theta, longlat = longlat
  )
  
  bw <- GWmodel::bw.gwr(
    formula  = value ~ 1,
    data     = pts_sp_sub,
    approach = approach,
    adaptive = adaptive,
    kernel   = kernel,
    p = p, theta = theta, longlat = longlat,
    dMat     = Dobs_sub
  )
  
  # ---- Split predictions by unique group 
  if (is.null(group_variable)) {
    locat_predict@data$block_variable <- 1L
  } else {
    locat_predict@data$block_variable <- locat_predict@data[[group_variable]]
  }
  groups <- split(seq_len(nrow(locat_predict)), locat_predict@data$block_variable)
  
  gw_df <- lapply(
    names(groups),  
    function(g) {
      tryCatch({
        idx_g <- groups[[g]]
        rp_sp <- locat_predict[idx_g, ]
        coords_rp <- sp::coordinates(rp_sp)
        
        D_or <- GWmodel::gw.dist(
          dp.locat = coords_obs,
          rp.locat = coords_rp,
          p = p, theta = theta, longlat = longlat
        )
        
        gwss_obj <- GWmodel::gwss(
          data          = locat_observed,   # ALL observed
          summary.locat = rp_sp,
          bw            = bw,
          vars          = "value",
          kernel        = kernel,
          adaptive      = adaptive,
          p             = p, theta = theta, longlat = longlat,
          dMat          = D_or,
          quantile      = FALSE
        )
        
        out <- as.data.frame(gwss_obj$SDF@data)
        out$longitude <- rp_sp@data[["longitude_flag"]]  # or "longitude"
        out$latitude  <- rp_sp@data[["latitude_flag"]]   # or "latitude"
        out$bandwidth <- bw
        if (!is.null(identifiers)) {
          for (ix in identifiers) out[[ix]] <- rp_sp@data[[ix]]
        }
        out
      }, error = function(e) NULL)
    }
  )
  
  gw_df <- data.table::rbindlist(gw_df, use.names = TRUE, fill = TRUE)
  
  # ---- Attach metadata
  if (nrow(gw_df)) {
    gw_df[, `:=`(
      p        = p,
      theta    = theta,
      longlat  = longlat,
      approach = approach,
      adaptive = adaptive,
      variable = variable
    )]
  }
  
  data.table::setDT(gw_df)
  return(gw_df)
}


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
#' @import data.table
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
  pts_sp_all <- methods::as(pts_sf_all, "Spatial")
  stopifnot(inherits(pts_sp_all, "SpatialPointsDataFrame"))
  
  pts_sf_obs <- sf::st_point_on_surface(sf_obs)
  pts_sp_obs <- methods::as(pts_sf_obs, "Spatial")
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

