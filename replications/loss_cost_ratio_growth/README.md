Spatial Variation in Loss Cost Ratio Growth (FCIP)
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

The folder provides data and replication codes for the United States
Crop Insurance Program as described in

**Script:** Spatial Variation in Loss Cost Ratio Growth (FCIP)

**Purpose:** Estimate county-level trends and spatial variation in the
log loss–cost ratio (LCR) of the U.S. Federal Crop Insurance Program
(FCIP) using a rolling-window panel estimator with iterative spatial
smoothing.

**Inputs:** - “sobscc” data release (all years): county-crop-year
indemnity and liability records

**Outputs:** - RDS files of raw and smoothed trend estimates by county,
for each combination of:

               • history_window ∈ {5,10,15,20,25,30} years  
               • smoothing function ∈ {mean, median, mode, min, max}  
               • target year ∈ [min_year+30 … max_year]
               

**Key Steps:**

1.  Data prep: compute LCR, log-LCR, panel & spatial IDs  
2.  Build estimation catalog (all scenarios)  
3.  For each scenario: • Subset to the rolling window of years  
    • Define time trend within panel  
    • Run panel-based spatial smoothing estimator  
    • Iteratively smooth county estimates  
    • Save results to “output/estimations/”

## 📚 Citation

If you find this repository useful, please star this project and cite
our papers listed above.

See the [LICENSE](../LICENSE) file in the repository’s root for details.

*Maintained by [ftsiboe](https://github.com/ftsiboe)*
