test_that("smooth_county_estimates smooths neighbors and classifies without external I/O", {
  skip_if_not_installed("sf")
  skip_if_not_installed("classInt")
  skip_if_not_installed("dplyr")
  # urbnmapr and terra may be absent in CI; we mock their functions anyway.
  
  library(sf)
  library(dplyr)
  
  # ---- Build a tiny synthetic county map (4 touching squares + 1 island) ----
  # 2x2 grid squares at (0,0)-(2,2), plus an island at (10,10)-(11,11)
  sq <- function(xmin, ymin, xmax, ymax) {
    st_polygon(list(matrix(
      c(xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin), ncol = 2, byrow = TRUE)))
  }
  
  polys <- st_sfc(
    sq(0,0,1,1),   # A
    sq(1,0,2,1),   # B
    sq(0,1,1,2),   # C
    sq(1,1,2,2),   # D
    sq(10,10,11,11) # E (island; no neighbors)
  )
  
  counties_sf_fake <- st_as_sf(
    data.frame(
      county_fips = c("00001","00002","00003","00004","00999"),
      stringsAsFactors = FALSE
    ),
    geometry = polys, crs = NA
  )
  
  # ---- Mock urbnmapr::get_urbn_map() and terra::vect() ----
  f_get_urbn_map <- function(map = "counties", sf = TRUE, ...) {
    stopifnot(identical(map, "counties"), isTRUE(sf))
    counties_sf_fake
  }
  f_vect <- function(x, ...) x  # no-op; just return input
  
  testthat::local_mocked_bindings(get_urbn_map = f_get_urbn_map, .package = "urbnmapr")
  testthat::local_mocked_bindings(vect = f_vect, .package = "terra")
  
  # ---- Input data: missing values to be smoothed; island left NA ----
  # A=1, B=NA, C=3, D=NA, (E island = NA)
  est_df <- data.frame(
    fip = c("00001","00002","00003","00004","00999"),
    estimate = c(1, NA, 3, NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Default fun = mean, iterations = 3 should propagate values:
  # Neighbors:
  # - A touches B and C
  # - B touches A and D
  # - C touches A and D
  # - D touches B and C
  # E has no neighbors
  #
  # Round 1:
  #   B <- mean(A,D[NA])   -> mean(1, NA) -> 1
  #   D <- mean(B[NA],C=3) -> mean(NA, 3) -> 3
  # Round 2:
  #   none become NA; B and D remain 1 and 3
  # Final smooth: A=1, B=1, C=3, D=3, E=NA
  
  out <- smooth_county_estimates(
    data         = est_df,
    fip_col      = "fip",
    estimate_col = "estimate",
    iterations   = 1,        # <- one-pass imputation
    fun          = mean,
    n_classes    = 4,
    contiguity   = "rook"
  )
  
  # Basic structure
  expect_s3_class(out, "sf")
  expect_true(all(c("estimate_smooth", "estimate_cat") %in% names(out)))
  
  # Pull results aligned by fips for easy checking
  res <- out %>%
    st_drop_geometry() %>%
    select(county_fips, estimate, estimate_smooth, estimate_cat) %>%
    arrange(county_fips)
  
  # Verify smoothing outcome
  # A=1, B=1, C=3, D=3, E=NA
  sm <- setNames(res$estimate_smooth, res$county_fips)
  expect_equal(sm[["00001"]], 1)
  expect_equal(sm[["00002"]], 1)
  expect_equal(sm[["00003"]], 3)
  expect_equal(sm[["00004"]], 3)
  expect_true(is.na(sm[["00999"]]))  # island stays NA
  
  # Classification: should be a factor; only non-NA smoothed values are classified
  expect_s3_class(res$estimate_cat, "factor")
  # non-NA entries must have non-NA categories
  expect_true(all(!is.na(res$estimate_cat[res$county_fips != "00999"])))
  
  # ---- Check that custom 'fun' is respected (use median) ----
  # With A=1, C=3, B neighbors {A,D}, D neighbors {B,C}
  # Using median, final should still converge to B=1, D=3 under these values.
  out_median <- smooth_county_estimates(
    data         = est_df,
    fip_col      = "fip",
    estimate_col = "estimate",
    iterations   = 3,
    fun          = stats::median,
    n_classes    = 4
  )
  res_med <- out_median %>%
    st_drop_geometry() %>%
    select(county_fips, estimate_smooth) %>%
    arrange(county_fips)
  sm_med <- setNames(res_med$estimate_smooth, res_med$county_fips)
  expect_equal(sm_med[["00002"]], 1)
  expect_equal(sm_med[["00004"]], 3)
  expect_true(is.na(sm_med[["00999"]]))
})

test_that("Jenks classification degrades gracefully with few unique values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("classInt")
  skip_if_not_installed("dplyr")
  
  library(sf)
  library(dplyr)
  
  # Reuse the same map mocks
  # (redefine here to keep tests independent)
  sq <- function(xmin, ymin, xmax, ymax) {
    st_polygon(list(matrix(
      c(xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin), ncol = 2, byrow = TRUE)))
  }
  polys <- st_sfc(
    sq(0,0,1,1),
    sq(1,0,2,1),
    sq(0,1,1,2)
  )
  counties_sf_fake <- st_as_sf(
    data.frame(county_fips = c("10001","10002","10003")),
    geometry = polys, crs = NA
  )
  
  f_get_urbn_map <- function(map = "counties", sf = TRUE, ...) counties_sf_fake
  f_vect <- function(x, ...) x
  testthat::local_mocked_bindings(get_urbn_map = f_get_urbn_map, .package = "urbnmapr")
  testthat::local_mocked_bindings(vect = f_vect, .package = "terra")
  
  # Only two unique smoothed values but ask for n_classes=5
  dat <- data.frame(
    fip = c("10001","10002","10003"),
    estimate = c(10, NA, 20),
    stringsAsFactors = FALSE
  )
  
  out <- smooth_county_estimates(
    data         = dat,
    fip_col      = "fip",
    estimate_col = "estimate",
    iterations   = 2,
    fun          = mean,
    n_classes    = 5
  )
  
  res <- out %>% st_drop_geometry()
  # estimate_cat is a factor; number of levels may be < n_classes due to few uniques
  expect_s3_class(res$estimate_cat, "factor")
  expect_true(nlevels(res$estimate_cat) >= 1)
  # non-NA smoothed values should have non-NA categories
  expect_true(all(!is.na(res$estimate_cat[!is.na(res$estimate_smooth)])))
})
