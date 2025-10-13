test_that("clean_rma_sobtpu writes file, returns message, and computes shares", {
  testthat::skip_on_cran()

  out_dir  <- withr::local_tempdir()
  fake_env <- list(year_beg = 2020L, year_end = 2020L)

  POOL_COLS  <- c("state_code","county_code","commodity_code","type_code","practice_code")
  ELECT_COLS <- c("unit_structure_code","insurance_plan_code","coverage_type_code","coverage_level_percent")

  n <- 10L
  DT <- data.table::data.table(
    commodity_year = c(2020L,2020L,2020L,  2020L,2020L,2020L,  2020L,2020L,  2020L,2020L),

    # pool keys
    state_code   = rep("AR", n),
    county_code  = rep("001", n),
    commodity_code = rep("CORN", n),
    type_code      = rep("000", n),
    practice_code  = rep("00",  n),

    # election keys
    unit_structure_code    = rep("OU", n),
    insurance_plan_code    = c(1L, 2L, 90L, 31L, 32L, 33L, 87L, 89L, 1L, 1L),
    coverage_type_code     = c("B","B","B", "B","B","B", "B","B", "C","B"),
    coverage_level_percent = c(NA, NA, NA, 0.75, 0.75, 0.75, 0.90, 0.95, NA, NA),

    reporting_level_type = c("Acres","Acres","Acres", "Acres","Acres","Acres", "Acres","Acres", "Acres","Units"),

    net_reporting_level_amount = c(100, 200, 150,  0, 0, 0,  0, 0,  50, 25),
    liability_amount           = c(10,20,15, 0,0,0, 0,0, 5, 3),
    total_premium_amount       = c(5,10,7,  0,0,0,  0,0, 1, 1),
    subsidy_amount             = c(3,6,4,   0,0,0,  0,0, 1, 0),
    indemnity_amount           = c(2,1,3,   0,0,0,  0,0, 0, 0),

    endorsed_commodity_reporting_level_amount = c(
      0, 0, 0,     # base rows
      60, 0, 0,    # SCO plan 31 (→ base plan 1) at 0.75
      120, 50,     # ECO: 87→1 (eco90=120), 89→3 (eco95=50)
      0, 0         # filtered rows (CAT and non-Acres)
    )
  )

  fake_get_sob_data <- function(sob_version, year) {
    testthat::expect_identical(sob_version, "sobtpu")
    testthat::expect_identical(year, fake_env$year_beg:fake_env$year_end)
    DT
  }
  fake_setup_environment <- function() fake_env

  mockery::stub(clean_rma_sobtpu, "get_sob_data", fake_get_sob_data)
  mockery::stub(clean_rma_sobtpu, "setup_environment", fake_setup_environment)

  # Run
  msg <- clean_rma_sobtpu(output_directory = out_dir)

  # Return message
  testthat::expect_type(msg, "character")
  testthat::expect_match(msg, "Finished processing RMA SOB data")
  testthat::expect_match(msg, "Saved to:")

  # File written
  save_path <- file.path(out_dir, "cleaned_rma_sobtpu.rds")
  testthat::expect_true(file.exists(save_path))

  # Load and check structure
  res <- readRDS(save_path)
  testthat::expect_s3_class(res, "data.table")

  needed_cols <- c(
    "commodity_year",
    POOL_COLS,
    ELECT_COLS,
    "insured_acres","liability_amount","total_premium_amount","subsidy_amount","indemnity_amount",
    "sco","eco90","eco95"
  )
  missing_cols <- setdiff(needed_cols, names(res))
  testthat::expect(
    length(missing_cols) == 0,
    sprintf("Missing expected columns: %s", paste(missing_cols, collapse = ", "))
  )

  # Shares clamped to [0,1]
  for (nm in c("sco","eco90","eco95")) {
    testthat::expect_true(is.numeric(res[[nm]]))
    testthat::expect_true(all(res[[nm]] >= 0, na.rm = TRUE))
    testthat::expect_true(all(res[[nm]] <= 1, na.rm = TRUE))
  }

  # Numeric check: plan 1 base acres = 100 + 150 (90→1) = 250; eco90(plan1)=120/250=0.48
  if ("insurance_plan_code" %in% names(res)) {
    eco90_p1 <- res[insurance_plan_code == 1, eco90]
    if (length(eco90_p1)) {
      testthat::expect_true(any(abs(eco90_p1 - 0.48) < 1e-8))
    }
  }

  # No invalid insured acres; no negative shares
  testthat::expect_false(any(!is.finite(res$insured_acres)))
  testthat::expect_false(any(res$sco   < 0, na.rm = TRUE))
  testthat::expect_false(any(res$eco90 < 0, na.rm = TRUE))
  testthat::expect_false(any(res$eco95 < 0, na.rm = TRUE))
})
