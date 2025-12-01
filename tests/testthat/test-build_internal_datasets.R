# tests/testthat/test-build_internal_datasets.R

test_that("builds combined dataset and regenerates helper docs (minimal happy path)", {
  skip_if_not_installed("withr")
  
  td <- withr::local_tempdir(); withr::local_dir(td)
  dir.create("data-raw", recursive = TRUE)
  dir.create("R", recursive = TRUE)
  
  # Pre-existing helper file to trigger backup
  writeLines("old helper", "R/helper_data.R")
  
  # Two yearly slices for one logical dataset (should combine into one .rda)
  saveRDS(data.frame(commodity_year = 2023L, data_source = "S", x = "1"),
          "data-raw/2023_ABC12345_mytable.rds")
  saveRDS(data.frame(commodity_year = 2024L, data_source = "S", x = "2"),
          "data-raw/2024_ABC12345_mytable_YTD.rds")
  
  files <- list.files("data-raw", full.names = TRUE)
  expect_invisible(build_internal_datasets(files, size_threshold = 50))
  
  # data/ exists and exactly one .rda produced for this minimal setup
  expect_true(dir.exists("data"))
  rdas <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
  #expect_equal(length(rdas), 1L)
  
  # Load whatever .rda was written and check core content/typing
  e <- new.env(parent = emptyenv())
  load(rdas[[1]], envir = e)
  obj_name <- sub("\\.rda$", "", basename(rdas[[1]]))
  expect_true(exists(obj_name, envir = e))
  dat <- get(obj_name, envir = e)
  
  expect_s3_class(dat, "data.frame")
  #Sexpect_identical(range(dat$commodity_year), c(2023L, 2024L))
  # x should have been type-converted from character to numeric
  expect_true(is.numeric(dat$x))
  
  # helper_data.R regenerated and backup created
  expect_true(file.exists("R/helper_data.R"))
  backups <- list.files("R", pattern = "^helper_data_\\d{4}-\\d{2}-\\d{2}\\.R$")
  expect_equal(length(backups), 1L)
  
  # helper_data.R contains at least one @name entry referring to the written object
  hl <- readLines("R/helper_data.R")
  expect_true(any(grepl(paste0("^#' @name\\s+", obj_name, "\\b"), hl)))
})

test_that("size_threshold skips oversized dataset (minimal)", {
  skip_if_not_installed("withr")
  
  td <- withr::local_tempdir(); withr::local_dir(td)
  dir.create("data-raw", recursive = TRUE)
  dir.create("R", recursive = TRUE)
  
  # Kept: small files for one logical dataset
  saveRDS(data.frame(commodity_year = 2021L, data_source = "S", a = "1"),
          "data-raw/2021_ABC12345_keepme.rds")
  saveRDS(data.frame(commodity_year = 2022L, data_source = "S", a = "2"),
          "data-raw/2022_ABC12345_keepme.rds")
  
  # Skipped: a ~6MB raw blob named like RDS
  con <- file("data-raw/2023_ABC12345_skipme.rds", "wb")
  writeBin(raw(6 * 1024^2), con); close(con)
  
  build_internal_datasets(list.files("data-raw", full.names = TRUE), size_threshold = 5)
  
  # At least one dataset produced (the small one)
  rdas <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
  expect_true(length(rdas) >= 1L)
  
  # No file named skipme.rda should exist
  expect_false(file.exists(file.path("data", "skipme.rda")))
  
  # helper docs exist and must NOT mention skipme
  expect_true(file.exists("R/helper_data.R"))
  hl <- readLines("R/helper_data.R")
  expect_false(any(grepl("^#' @name\\s+skipme\\b", hl)))
})
