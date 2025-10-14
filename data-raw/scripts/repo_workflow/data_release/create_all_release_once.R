
rm(list = ls(all = TRUE))

source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

# Verify auth first (nice sanity check)
if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# Summary of Business
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag = "sob",
  name = "Summary of Business",
  body = paste("Summary of Business data breaks out FCIP participation at variaous levels:",
               paste0("**sobtpu** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their contract choice (i), the insurance pool they selected (j), ",
                      "and the crop year (t). Contract choices combine insurance plan (e.g., APH, RP), ",
                      "coverage level, and unit structure (e.g., Optional [OU], Enterprise [EU]). ",
                      "Pools are the most granular rate‐setting level and are distinguished by county, commodity, ",
                      "crop type, and practice (e.g., irrigated, organic).", collapse = ""),
               paste0("**sobcov** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their coverage level, county, commodity, and  commodity year.", collapse = ""),
               paste0("**sobscc** aggregates loss experience for groups of producers who are ",
                      "similarly defined by their county, commodity, and  commodity year.", collapse = ""),sep = "\n\n"))

# Cause of Loss
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "col",
  name = "Cause of Loss",
  body = "Cause of Loss breaks out FCIP participation by peril")

# Actuarial Data Master - Extracts
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "adm_extracts",
  name = "Actuarial Data Master - Extracts",
  body = "Various items aggregated from the FCIP's Actuarial Data Master"
)

# Actuarial Data Master - Legacy
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "adm_legacy",
  name = "Actuarial Data Master - Legacy",
  body = "The FCIP's Actuarial Data Master from 1996 to 2010"
)

# Insurance Control Elements
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag = "ice",
  name = "Insurance Control Elements",
  body = "Various items aggregated from the FCIP's  Insurance Control Elements")

# USDA NASS Data - Extracts
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "nass_extracts",
  name = "USDA NASS Data - Extracts",
  body = "Various items aggregated from USDA NASS"
)

# Spatial features
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag = "spatial_features",
  name = "Spatial features",
  body = "A collection of frequently used spatial features")


# Replications
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "reps",
  name = "Replications",
  body = "Various items aggregated from replications"
)











