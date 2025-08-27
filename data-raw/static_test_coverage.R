# usethis::use_github_action() 
# rm(list=ls(all=TRUE));gc()
if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr")
devtools::document()
library(rfcip)
library(data.table)
library(rmaADM)
get_color <- function(pct) {
  if (pct >= 90) return("brightgreen")
  if (pct >= 75) return("green")
  if (pct >= 60) return("yellow")
  if (pct >= 40) return("orange")
  return("red")
}

# usethis::use_github_action("test-coverage") # Test coverage workflow
detach("package:rfcipReSim", unload = TRUE)
covr::report(file="data-raw/badges/test-coverage-report.html")
package_coverage_percent <- round(covr::percent_coverage(covr::package_coverage(type = "tests")), 2)
download.file(sprintf("https://img.shields.io/badge/coverage-%.2f%%25-%s.svg",
                      package_coverage_percent, get_color(package_coverage_percent)),
              "data-raw/badges/coverage.svg", mode = "wb")


