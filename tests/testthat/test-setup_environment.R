test_that("setup_environment creates dirs, sets options/seed, and returns structure", {
  skip_if_not_installed <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
    if (length(missing)) testthat::skip(paste("Missing required packages:", paste(missing, collapse = ", ")))
  }
  skip_if_not_installed(c("future.apply", "rfcip", "data.table", "rfcipCalcPass"))
  skip_if_not_installed(c("withr", "testthat"))

  withr::local_dir(withr::local_tempdir())

  withr::local_options(list(scipen = getOption("scipen"),
                            future.globals.maxSize = getOption("future.globals.maxSize"),
                            dplyr.summarise.inform = getOption("dplyr.summarise.inform")))
  withr::local_seed(1)

  fs_root <- file.path(tempdir(), "fastscratch_testroot")

  res <- setup_environment(year_beg = 2015, year_end = 2016, seed = 123, fastscratch_root = fs_root)

  # --- return structure ---
  expect_type(res, "list")
  expect_true(all(c("wd", "year_beg", "year_end") %in% names(res)))
  expect_type(res$wd, "list")
  expect_identical(res$year_beg, 2015L)
  expect_identical(res$year_end, 2016L)

  # --- fastscratch dirs created ---
  expect_true(dir.exists(fs_root))
  expect_true(dir.exists(file.path(fs_root, "HiddenSafetynet2025", "output", "sims")))
  expect_true(dir.exists(file.path(fs_root, "HiddenSafetynet2025", "output", "expected")))
  expect_true(dir.exists(file.path(fs_root, "HiddenSafetynet2025", "output", "draw_farm")))
  expect_true(dir.exists(file.path(fs_root, "HiddenSafetynet2025", "output", "draw_cost")))

  # --- project-local dirs created under data/ ---
  expect_true(dir.exists("data"))
  expect_true(dir.exists(file.path("output")))
  expect_true(dir.exists(file.path("data", "cleaned_agents_data")))

  # --- options set as documented ---
  expect_identical(getOption("scipen"), 999L)
  expect_identical(getOption("dplyr.summarise.inform"), FALSE)
  expect_identical(getOption("future.globals.maxSize"), 8 * 1024^3)

  # --- RNG seed effect: reproducible under current RNGkind ---
  got <- runif(3)
  set.seed(123)
  expected <- runif(3)
  expect_equal(got, expected)
})

test_that("setup_environment validates inputs and errors when year_beg > year_end", {
  skip_if_not_installed <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
    if (length(missing)) testthat::skip(paste("Missing required packages:", paste(missing, collapse = ", ")))
  }
  skip_if_not_installed(c("future.apply", "rfcip", "data.table", "rfcipCalcPass"))

  fs_root <- file.path(tempdir(), "fastscratch_testroot2")
  expect_error(
    setup_environment(year_beg = 2025, year_end = 2024, seed = 1, fastscratch_root = fs_root),
    regexp = "`year_beg` must be <= `year_end`"
  )
})

test_that("setup_environment respects provided fastscratch_root", {
  skip_if_not_installed <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
    if (length(missing)) testthat::skip(paste("Missing required packages:", paste(missing, collapse = ", ")))
  }
  skip_if_not_installed(c("future.apply", "rfcip", "data.table", "rfcipCalcPass"))

  withr::local_dir(withr::local_tempdir())
  fs_root <- file.path(tempdir(), "fastscratch_custom_root")
  res <- setup_environment(year_beg = 2018, year_end = 2019, seed = 321, fastscratch_root = fs_root)

  expect_true(dir.exists(fs_root))
  expect_true(grepl(normalizePath(fs_root, winslash = "/"),
                    normalizePath(res$wd$dir_sim, winslash = "/"),
                    fixed = TRUE))
})
