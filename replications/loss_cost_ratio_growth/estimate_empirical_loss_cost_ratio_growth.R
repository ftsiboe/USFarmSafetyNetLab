# -----------------------------------------------------------------------------
# Script:     Spatial Variation in Loss Cost Ratio Growth (FCIP)
# Purpose:    Estimate county-level trends and spatial variation in the
#             log loss–cost ratio (LCR) of the U.S. Federal Crop Insurance
#             Program (FCIP) using a rolling-window panel estimator with
#             iterative spatial smoothing.
# Inputs:     - “sobscc” data release (all years): county-crop-year indemnity
#               and liability records
# Outputs:    - RDS files of raw and smoothed trend estimates by county,
#               for each combination of:
#               • history_window ∈ {5,10,15,20,25,30} years  
#               • smoothing function ∈ {mean, median, mode, min, max}  
#               • target year ∈ [min_year+30 … max_year]
# Key Steps:  1. Data prep: compute LCR, log-LCR, panel & spatial IDs  
#             2. Build estimation catalog (all scenarios)  
#             3. For each scenario:
#                • Subset to the rolling window of years  
#                • Define time trend within panel  
#                • Run panel-based spatial smoothing estimator  
#                • Iteratively smooth county estimates  
#                • Save results to “output/estimations/”  
# -----------------------------------------------------------------------------

# Clear the workspace and run garbage collection to free memory
rm(list = ls(all = TRUE))
gc()

# Re-generate documentation for any R package functions in this project
devtools::document()

# Create directories for storing output (if they don’t already exist)
dir_estimations <- "./data-raw/fastscratch/replications/loss_cost_ratio_growth/output/estimations/"

if (!dir.exists(dir_estimations)) {
  dir.create(dir_estimations, recursive = TRUE)
}

replications_release <- "./data-raw/data_release/replications"

if (!dir.exists(replications_release)) {
  dir.create(replications_release, recursive = TRUE)
}


# Load the full set of “sobscc” data for all available years
sobscc <- tempfile(fileext = ".rds")
download.file(
  "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobscc_1948_1988.rds",
  sobscc, mode = "wb", quiet = TRUE)
sobscc <- readRDS(sobscc)

# Compute the loss cost ratio (LCR) and its logarithm
sobscc[, lcr := indemnity_amount / liability_amount]     # LCR = indemnity paid / liability covered
sobscc[, lnlcr := log(lcr)]                              # Take natural log for modeling

# Drop any rows with missing or infinite log‐LCR values
sobscc <- sobscc[complete.cases(sobscc) & is.finite(lnlcr)]

# Create a unique identifier for each county‐crop combination
# state_code: 2 digits, county_code: 3 digits, commodity_code: 3 digits
sobscc[, county_crop_id := paste0(
  stringr::str_pad(state_code, 2, pad = "0"),
  stringr::str_pad(county_code, 3, pad = "0"),
  stringr::str_pad(commodity_code, 3, pad = "0")
)]

# Create a FIPS code for each county (state + county)
sobscc[, county_fips := paste0(
  stringr::str_pad(state_code, 2, pad = "0"),
  stringr::str_pad(county_code, 3, pad = "0")
)]

# Define the range of years to estimate over:
# start at (minimum year + 30) up to the maximum year in the data
year_list <- (min(sobscc$commodity_year, na.rm = TRUE) + 30):
  max(sobscc$commodity_year, na.rm = TRUE)

# Build a catalog of all estimation scenarios:
# for each history window (5, 10, 15, 20, 25, 30),
# and for each smoothing function (mean, median, mode, min, max),
# and for each target year in year_list
estimation_catalog <- as.data.frame(
  data.table::rbindlist(
    lapply(seq(5, 30, 5), function(history_window) {
      data.frame(
        history_window = history_window,
        rbind(
          data_frame(smoth_fun = "mean"  , year = year_list),
          data_frame(smoth_fun = "median", year = year_list),
          data_frame(smoth_fun = "mode"  , year = year_list),
          data_frame(smoth_fun = "min"   , year = year_list),
          data_frame(smoth_fun = "max"   , year = year_list)
        )
      )
    }), fill = TRUE
  )
)

# If running as a SLURM array job, select only the row corresponding to the current task ID
if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  estimation_catalog$slurm_id <- rep(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MIN")):as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MAX")), 
                                 length=nrow(estimation_catalog))
  estimation_catalog <- estimation_catalog[estimation_catalog$slurm_id %in% as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

# Loop over each estimation scenario
lapply(1:nrow(estimation_catalog), function(est) {
  
  # Extract parameters for this run
  year           <- estimation_catalog$year[est]
  history_window <- estimation_catalog$history_window[est]
  smoth_fun      <- estimation_catalog$smoth_fun[est]
  
  # Subset data to the rolling window of years: [year - history_window + 1, ..., year]
  data <- sobscc[commodity_year %in% (year - history_window + 1):year]
  
  # Create a “trend” variable: 0 in the first year of the window, increasing by 1 each year
  data[, trend := commodity_year - min(commodity_year, na.rm = TRUE)]
  
  # Run the spatial smoothing estimator on log‐LCR,
  # modeling trend as the “treatment” over time,
  # using county_crop_id as the panel unit and county_fips for spatial neighbors
  data <- panel_based_spatial_smoothing_estimator(
    data       = data,
    output     = "lnlcr",
    treatment  = "trend",
    time       = "commodity_year",
    panel      = "county_crop_id",
    spatialvar = "county_fips"
  )
  
  # Split county_fips back into state_code and county_code columns
  data <- tidyr::separate(
    data, "county_fips",
    into   = c("state_code", "county_code"),
    sep    = 2,
    remove = FALSE
  )
  
  # Map the smoothing‐function name to the actual R function
  if (smoth_fun == "mean")   my_fun <- mean
  if (smoth_fun == "median") my_fun <- median
  if (smoth_fun == "mode")   my_fun <- calculate_mode
  if (smoth_fun == "min")    my_fun <- min
  if (smoth_fun == "max")    my_fun <- max
  
  # Apply iterative spatial smoothing to the raw estimates
  # - 5 iterations of smoothing
  # - grouping into 10 classes
  data <- smooth_county_estimates(
    data         = data,
    fip_col      = "county_fips",
    estimate_col = "estimate",
    iterations   = 5,
    fun          = my_fun,
    n_classes    = 10
  )
  
  # Keep only the key columns, plus restore separate state & county codes
  data <- as.data.frame(data)[
    c("county_fips", "estimate", "estimate_smooth", "estimate_cat")
  ]
  data <- tidyr::separate(
    data, "county_fips",
    into   = c("state_code", "county_code"),
    sep    = 2,
    remove = FALSE
  )
  data$state_code     <- as.numeric(as.character(data$state_code))
  data$county_code    <- as.numeric(as.character(data$county_code))
  data$commodity_year <- year
  
  # Save the results to an RDS file named by smoothing function, year, and window
  saveRDS(data,file = paste0(dir_estimations,smoth_fun, year, history_window, ".rds"))
  
})

# ggplot(data) +
#   geom_sf(aes(fill = estimate_cat), color = NA) +
#   scale_fill_viridis_d(direction = -1,na.value  = "grey90",name = "Smoothed\nEstimate") +
#   theme_void() +
#   theme(
#     legend.position    = "bottom",
#     legend.key.width   = unit(1.5, "cm"),
#     legend.title.align = 0.5)
