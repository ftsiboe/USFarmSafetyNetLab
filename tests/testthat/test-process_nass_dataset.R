test_that("process_nass_dataset filters, cleans, aggregates, and renames without I/O", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("stringr")
  library(data.table)
  
  # temp dir + a dummy file the function will 'discover'
  td <- tempfile("nass-cache-"); dir.create(td)
  file.create(file.path(td, "qs.crops_20250101.txt.gz"))
  
  # Mock fread so no disk/network is used
  dt_fake <- data.table(
    STATE_FIPS_CODE       = c("38","38","19","19"),
    COUNTY_ANSI           = c("001","001","003","003"),
    ASD_CODE              = c("01","01","03","03"),   # <- REQUIRED by function
    YEAR                  = c("2020","2020","2020","2020"),
    SECTOR_DESC           = c("CROPS","CROPS","CROPS","CROPS"),
    AGG_LEVEL_DESC        = c("STATE","STATE","STATE","STATE"),
    SHORT_DESC            = rep("SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU", 4),
    REFERENCE_PERIOD_DESC = rep("MARKETING YEAR", 4),
    FREQ_DESC             = rep("ANNUAL", 4),
    STATISTICCAT_DESC     = rep("PRICE RECEIVED", 4),
    COMMODITY_DESC        = rep("SOYBEANS", 4),
    UTIL_PRACTICE_DESC    = rep("", 4),
    VALUE                 = c("1,000","300","800","1,200"),
    `CV_%`                = c("5","10","7","8")
  )
  testthat::local_mocked_bindings(fread = function(files) dt_fake, .package = "data.table")
  
  out <- process_nass_dataset(
    dir_source        = td,
    large_dataset     = "crops",
    statisticcat_desc = "PRICE RECEIVED",
    nassqs_params     = list(
      agg_level_desc        = "STATE",
      short_desc            = "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
      reference_period_desc = "MARKETING YEAR",
      freq_desc             = "ANNUAL",
      year                  = "2020"
    )
  )
  
  # Structure checks
  expect_true("price_received" %in% names(out))  # value renamed
  expect_false("value" %in% names(out))
  #expect_true("cv" %in% names(out))              # cv_% -> cv
  expect_true(all(c("state_code","county_code","commodity_year","commodity_name","asd_code") %in% names(out)))
  
  # Aggregation checks (mean over duplicates)
  nd_val <- out[state_code == 38, unique(price_received)]
  ia_val <- out[state_code == 19, unique(price_received)]
  expect_equal(nd_val, 650)   # mean(1000,300)
  expect_equal(ia_val, 1000)  # mean(800,1200)
  
  # Types
  expect_true(is.numeric(out$state_code))
  expect_true(is.numeric(out$county_code))
  expect_true(is.numeric(out$commodity_year))
  expect_true(is.numeric(out$asd_code))
  
  # Commodity title-cased
  #expect_true(all(grepl("^Soybeans$", out$commodity_name)))
})

test_that("process_nass_dataset normalizes commodity names (sorghum & beans etc.)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("stringr")
  library(data.table)
  
  td <- tempfile("nass-cache-"); dir.create(td)
  file.create(file.path(td, "qs.crops_20250101.txt.gz"))
  
  dt_fake <- data.table(
    STATE_FIPS_CODE       = c("20","20","08"),
    COUNTY_ANSI           = c("001","001","005"),
    ASD_CODE              = c("02","02","01"),
    YEAR                  = c("2021","2021","2021"),
    AGG_LEVEL_DESC        = c("STATE","STATE","STATE"),
    STATISTICCAT_DESC     = c("PRICE RECEIVED","PRICE RECEIVED","PRICE RECEIVED"),
    COMMODITY_DESC        = c("SORGHUM","SORGHUM","BEANS"),
    UTIL_PRACTICE_DESC    = c("SILAGE","", ""),
    VALUE                 = c("100","200","300"),
    `CV_%`                = c("3","4","5")
  )
  testthat::local_mocked_bindings(fread = function(files) dt_fake, .package = "data.table")
  
  out <- process_nass_dataset(
    dir_source        = td,
    large_dataset     = "crops",
    statisticcat_desc = "PRICE RECEIVED",
    nassqs_params     = list(agg_level_desc = "STATE", year = "2021")
  )
  
  expect_true(any(out$commodity_name == "Silage Sorghum"))  # SILAGE path
  expect_true(any(out$commodity_name == "Grain Sorghum"))   # non-SILAGE sorghum path
  expect_true(any(out$commodity_name == "Dry Beans"))       # BEANS -> Dry Beans
})

test_that("process_nass_dataset validates inputs and respects filters", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("stringr")
  library(data.table)
  
  td <- tempfile("nass-cache-"); dir.create(td)
  file.create(file.path(td, "qs.crops_20250101.txt.gz"))
  
  dt_fake <- data.table(
    STATE_FIPS_CODE   = "27",
    COUNTY_ANSI       = "007",
    ASD_CODE          = "01",
    YEAR              = "2019",
    AGG_LEVEL_DESC    = "NATIONAL",
    STATISTICCAT_DESC = "PRICE RECEIVED",
    COMMODITY_DESC    = "CORN",
    UTIL_PRACTICE_DESC= "",
    VALUE             = "123",
    `CV_%`            = "2"
  )
  testthat::local_mocked_bindings(fread = function(files) dt_fake, .package = "data.table")
  
  expect_error(process_nass_dataset(large_dataset = c("crops","animals_products")),
               "`large_dataset` must be length 1")
  expect_error(process_nass_dataset(large_dataset = "not_real"),
               "`large_dataset` must be one of:")
  expect_error(process_nass_dataset(large_dataset = "crops", statisticcat_desc = c("A","B")),
               "`statisticcat_desc` must be length 1")
  
  out <- process_nass_dataset(
    dir_source        = td,
    large_dataset     = "crops",
    statisticcat_desc = "PRICE RECEIVED",
    nassqs_params     = list(agg_level_desc = "NATIONAL", year = "2019")
  )
  expect_true(nrow(out) >= 1)
  expect_true(all(out$commodity_year == 2019))
})
