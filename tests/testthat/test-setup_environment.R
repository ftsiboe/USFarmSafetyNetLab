# tests/testthat/test-setup_environment.R

test_that("errors if project_name is missing or empty", {
  # missing
  expect_error(setup_environment(), "`project_name` is required", fixed = FALSE)
  # empty
  expect_error(
    setup_environment(project_name = ""),
    "`project_name` is required",
    fixed = FALSE
  )
})

test_that("errors if year_beg > year_end", {
  expect_error(
    setup_environment(project_name = "P", year_beg = 2026, year_end = 2025),
    "`year_beg` must be <= `year_end`",
    fixed = TRUE
  )
})

test_that("creates local project directories in the working dir", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()     # auto-cleaned
  withr::local_dir(td)             # setwd to temp for this test
  
  env <- setup_environment(project_name = "AnyProj", fastscratch_directories = NULL)
  
  # default local directories
  expect_true(dir.exists(file.path("data-raw", "output")))
  expect_true(dir.exists(file.path("data-raw", "scripts")))
  expect_true(dir.exists("data"))
  
  # return structure
  expect_type(env, "list")
  expect_named(env, c("wd", "year_beg", "year_end", "seed"), ignore.order = TRUE)
  expect_true(is.list(env$wd))
  expect_true(is.integer(env$year_beg))
  expect_true(is.integer(env$year_end))
  expect_true(is.integer(env$seed))
})

test_that("fastscratch directories are created and named by basename", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  withr::local_dir(td)
  
  # we explicitly pass fastscratch_root to avoid touching any real system paths
  fs_root <- file.path(td, "fastscratch-root")
  dirs    <- c("output/sims", "output/expected")
  env <- setup_environment(
    project_name = "ProjX",
    fastscratch_root = fs_root,
    fastscratch_directories = dirs
  )
  
  # expected absolute paths
  exp_sims     <- file.path(fs_root, "ProjX", "output", "sims")
  exp_expected <- file.path(fs_root, "ProjX", "output", "expected")
  
  # created on disk
  expect_true(dir.exists(exp_sims))
  expect_true(dir.exists(exp_expected))
  
  # wd is a named list keyed by basenames
  expect_setequal(names(env$wd), c("sims", "expected"))
  expect_identical(normalizePath(env$wd$sims, mustWork = TRUE),
                   normalizePath(exp_sims, mustWork = TRUE))
  expect_identical(normalizePath(env$wd$expected, mustWork = TRUE),
                   normalizePath(exp_expected, mustWork = TRUE))
})

test_that("options are set as documented", {
  skip_if_not_installed("withr")
  # capture & restore options after this test
  withr::local_options(list(
    scipen = getOption("scipen"),
    future.globals.maxSize = getOption("future.globals.maxSize"),
    dplyr.summarise.inform = getOption("dplyr.summarise.inform")
  ))
  
  # use a temp wd to avoid stray dirs in the repo
  td <- withr::local_tempdir()
  withr::local_dir(td)
  
  setup_environment(project_name = "OptsOnly", fastscratch_directories = NULL)
  
  expect_identical(getOption("scipen"), 999L)
  expect_identical(getOption("future.globals.maxSize"), 8 * 1024^3)
  expect_identical(getOption("dplyr.summarise.inform"), FALSE)
})

test_that("sets RNG seed deterministically", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  withr::local_dir(td)
  
  # Preserve & restore RNG kind and state
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  # First seeding: generate a sequence
  env1 <- setup_environment(
    project_name = "RNGProj",
    seed = 424242L,
    fastscratch_directories = NULL
  )
  got1 <- runif(5)
  
  # Re-seed with the same seed: should reproduce exactly
  env2 <- setup_environment(
    project_name = "RNGProj",
    seed = 424242L,
    fastscratch_directories = NULL
  )
  got2 <- runif(5)
  
  expect_identical(env1$seed, 424242L)
  expect_identical(env2$seed, 424242L)
  expect_equal(got1, got2)
  
  # Different seed: should differ from got1 (very high probability)
  setup_environment(
    project_name = "RNGProj",
    seed = 424243L,
    fastscratch_directories = NULL
  )
  got3 <- runif(5)
  
  expect_false(isTRUE(all.equal(got1, got3)))
})


test_that("coerces numeric-like inputs to integer years/seed", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir(); withr::local_dir(td)
  
  env <- setup_environment(
    year_beg = 2001.0,
    year_end = as.numeric(format(Sys.Date(), "%Y")),  # numeric
    seed     = 1980632.0,
    project_name = "Coerce",
    fastscratch_directories = NULL
  )
  
  expect_type(env$year_beg, "integer")
  expect_type(env$year_end, "integer")
  expect_type(env$seed, "integer")
})
