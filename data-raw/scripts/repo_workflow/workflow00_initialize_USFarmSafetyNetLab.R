# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
# usethis::use_github_action()

rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(stringr)

# Clean generated artifacts
unlink(c("NAMESPACE","./R/helper_data.R",
         list.files("./R", full.names = TRUE,pattern = "helper_data_"),
         list.files("./data", full.names = TRUE),
         list.files("./man", full.names = TRUE)))

source("data-raw/scripts/repo_workflow/run_internal_datasets.R")

# Sanity pass through R/ sources: shows any non-ASCII characters per file
for (i in list.files("R", full.names = TRUE)) {
  print(paste0("********************", i, "********************"))
  tools::showNonASCIIfile(i)
}

# Rebuild documentation from roxygen comments
devtools::document()

# Check man pages only (faster than full devtools::check)
devtools::check_man()

# Build PDF manual into the current working directory
devtools::build_manual(path = getwd())

# Optional: run tests / full package check (uncomment when needed)
# devtools::test()
devtools::check()

