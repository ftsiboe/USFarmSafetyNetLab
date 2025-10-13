test_that("downloaded_nass_large_datasets caches without network", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("rvest")
  
  # helper: resolve the namespace where the function (and its imports) live
  .ns_of <- function(fun) {
    # 1) prefer the current package under test (works in standard testthat runs)
    nm <- tryCatch(utils::packageName(), error = function(e) NULL)
    if (!is.null(nm) && nm %in% loadedNamespaces()) return(asNamespace(nm))
    # 2) otherwise, use the function's own environment/namespace
    env <- environment(fun)
    if (isNamespace(env)) return(env)
    topenv(env)
  }
  
  if (!exists("read_html", envir = asNamespace("xml2"), inherits = FALSE)) {
    skip("xml2::read_html not found in this environment")
  }
  
  td <- tempfile("nass-cache-"); dir.create(td, recursive = TRUE)
  
  # Pretend the datasets page exposes these links
  fake_hrefs <- c(
    "/datasets/qs.crops_20250101.txt.gz",
    "/datasets/qs.census2022.txt.gz"
  )
  
  downloads <- character(0)
  
  # --- fakes ---
  f_read_html <- function(url) structure(list(url = url), class = "fake_html")
  f_html_nodes <- function(doc, selector) doc
  f_html_attr  <- function(doc, attr) fake_hrefs
  f_download   <- function(url, destfile, mode = "wb", ...) {
    dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
    file.create(destfile)
    downloads <<- c(downloads, destfile)
    0L
  }
  
  # --- mocks ---
  testthat::local_mocked_bindings(read_html = f_read_html, .env = asNamespace("xml2"))
  # If your implementation actually calls rvest helpers directly, uncomment:
  # testthat::local_mocked_bindings(html_nodes = f_html_nodes,
  #                                 html_attr  = f_html_attr,
  #                                 .env = asNamespace("rvest"))
  
  # IMPORTANT: mock download.file in the function's *own* package namespace
  pkg_ns <- .ns_of(downloaded_nass_large_datasets)
  testthat::local_mocked_bindings(download.file = f_download, .env = pkg_ns)
  
  # 1) First run should "download" both files
  out1 <- downloaded_nass_large_datasets(
    large_datasets = c("crops", "census2022"),
    dir_dest = td
  )
  expect_true(file.exists(file.path(td, "qs.census2022.txt.gz")))
  expect_length(downloads, 2L)
  expect_true(any(grepl("^qs\\.census2022\\.txt\\.gz$", basename(out1))))
  
  # 2) Second run should hit cache: no new downloads
  out2 <- downloaded_nass_large_datasets(
    large_datasets = c("crops", "census2022"),
    dir_dest = td
  )
  expect_setequal(basename(out2), basename(out1))
  expect_length(downloads, 2L)  # unchanged
})

test_that("non-census cleanup keeps latest variant only", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("rvest")
  
  .ns_of <- function(fun) {
    nm <- tryCatch(utils::packageName(), error = function(e) NULL)
    if (!is.null(nm) && nm %in% loadedNamespaces()) return(asNamespace(nm))
    env <- environment(fun)
    if (isNamespace(env)) return(env)
    topenv(env)
  }
  
  if (!exists("read_html", envir = asNamespace("xml2"), inherits = FALSE)) {
    skip("xml2::read_html not found in this environment")
  }
  
  td <- tempfile("nass-cache-"); dir.create(td, recursive = TRUE)
  
  # Simulate multiple 'crops' vintages on the page
  fake_hrefs <- c(
    "/datasets/qs.crops_20240101.txt.gz",
    "/datasets/qs.crops_20250101.txt.gz"  # latest we want
  )
  
  downloads <- character(0)
  
  f_read_html <- function(url) structure(list(url = url), class = "fake_html")
  f_html_nodes <- function(doc, selector) doc
  f_html_attr  <- function(doc, attr) fake_hrefs
  f_download   <- function(url, destfile, mode = "wb", ...) {
    dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
    file.create(destfile); downloads <<- c(downloads, destfile); 0L
  }
  
  testthat::local_mocked_bindings(read_html = f_read_html, .env = asNamespace("xml2"))
  # If needed for your implementation, uncomment:
  # testthat::local_mocked_bindings(html_nodes = f_html_nodes,
  #                                 html_attr  = f_html_attr,
  #                                 .env = asNamespace("rvest"))
  
  pkg_ns <- .ns_of(downloaded_nass_large_datasets)
  testthat::local_mocked_bindings(download.file = f_download, .env = pkg_ns)
  
  # Pre-place an older file to verify non-census cleanup path doesn’t break
  file.create(file.path(td, "qs.crops_20230101.txt.gz"))
  
  out <- downloaded_nass_large_datasets("crops", dir_dest = td)
  
  expect_length(downloads, 1L)  # only latest "downloaded"
})
