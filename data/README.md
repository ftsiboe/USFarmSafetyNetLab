Datasets
================

-   <a href="#datasets" id="toc-datasets">Datasets</a>
    -   <a href="#federal-crop-insurance-program-fcip--summary-of-business-sob"
        id="toc-federal-crop-insurance-program-fcip--summary-of-business-sob">Federal
        Crop Insurance Program (FCIP) – Summary of Business (SOB)</a>
    -   <a href="#federal-crop-insurance-program-fcip--cause-of-loss-col"
        id="toc-federal-crop-insurance-program-fcip--cause-of-loss-col">Federal
        Crop Insurance Program (FCIP) – Cause of Loss (COL)</a>
    -   <a href="#federal-crop-insurance-program-fcip--reinsurance"
        id="toc-federal-crop-insurance-program-fcip--reinsurance">Federal Crop
        Insurance Program (FCIP) – Reinsurance</a>
    -   <a href="#examples-in-r" id="toc-examples-in-r">Examples in R</a>
    -   <a href="#-citation" id="toc--citation">📚 Citation</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Datasets

This directory provides guidance on accessing the various datasets used
in U.S. farm safety-net research. Below are descriptions of each dataset
and instructions on how to access them:

## Federal Crop Insurance Program (FCIP) – Summary of Business (SOB)

This dataset provides comprehensive participation information for the
FCIP. It can be accessed via two methods depending on the period of
analysis:

-   **Recent data (1988–present):**  
    Available through the R package
    [**rfcip**](https://github.com/dylan-turner25/rfcip), which offers
    tools to retrieve SOB, COL, and reinsurance data at various levels
    of aggregation.
-   **Historical data (pre-1988):**  
    Download from this repository’s release assets:
    -   [Historical summary of business by state, county, crop, and
        coverage
        (1989-present)](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/v0.1.0/historical_summary_of_business_by_state_county_crop_coverage.rds)
    -   [Historical summary of business by state, county, and crop
        (1948-2020)](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/v0.1.0/historical_summary_of_business_by_state_county_crop.rds)

## Federal Crop Insurance Program (FCIP) – Cause of Loss (COL)

This dataset offers summarized information on FCIP participation,
segmented by different causes of loss. It can be accessed as follows:

-   **Recent data (1989–present):**  
    Available through the R package
    [**rfcip**](https://github.com/dylan-turner25/rfcip).
-   **Historical data (pre-1989):**  
    Download from this repository’s release assets:
    -   [Historical cause-of-loss indemnities with
        month](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/v0.1.0/historical_cause_of_loss_indemnities_with_month.rds)
    -   [Historical cause-of-loss indemnities
        (aggregate)](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/v0.1.0/historical_cause_of_loss_indemnities_only.rds)
    -   [Historical cause-of-loss premiums and
        indemnities](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/v0.1.0/historical_cause_of_loss_premimums_and_indemnities.rds)

## Federal Crop Insurance Program (FCIP) – Reinsurance

This dataset provides reinsurance data for the FCIP Available through
the R package [**rfcip**](https://github.com/dylan-turner25/rfcip).

------------------------------------------------------------------------

For questions or additional information about these datasets, please
open an issue or contact us directly on GitHub.

## Examples in R

``` r
# rfcip can be installed directly from github using remotes::install_github("https://github.com/dylan-turner25/rfcip")

# Example 1: Accessing SOB data via rfcip
sob1 <- rfcip::get_sob_data(year = 2015:2020, crop = "corn")
```

    ## Downloading summary of business data for specified crop years ■■■■■■■■■■■
    ## …Downloading summary of business data for specified crop years ■■■■■■■■■■■■■■■■
    ## …Downloading summary of business data for specified crop years
    ## ■■■■■■■■■■■■■■■■■…Downloading summary of business data for specified crop years
    ## ■■■■■■■■■■■■■■■■■…

``` r
head(tibble(sob1))
```

    ## # A tibble: 6 × 23
    ##   commodity_year commodity_code commodity_name policies_sold
    ##            <dbl>          <int> <chr>                  <dbl>
    ## 1           2015             41 Corn                  569437
    ## 2           2016             41 Corn                  565226
    ## 3           2017             41 Corn                  560354
    ## 4           2018             41 Corn                  556212
    ## 5           2019             41 Corn                  560899
    ## 6           2020             41 Corn                  573839
    ## # ℹ 19 more variables: policies_earning_prem <dbl>, policies_indemnified <dbl>,
    ## #   units_earning_prem <dbl>, units_indemnified <dbl>, quantity <dbl>,
    ## #   quantity_type <chr>, companion_endorsed_acres <dbl>, liabilities <dbl>,
    ## #   total_prem <dbl>, subsidy <dbl>, indemnity <dbl>, efa_prem_discount <dbl>,
    ## #   addnl_subsidy <dbl>, state_subsidy <dbl>, pccp_state_matching_amount <dbl>,
    ## #   organic_certified_subsidy_amount <dbl>,
    ## #   organic_transitional_subsidy_amount <dbl>, earn_prem_rate <dbl>, …

``` r
# Example 2: Accessing SOB data via this repository’s release assets
base_url <- "https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download"
version  <- "v0.1.0"
file <- "historical_summary_of_business_by_state_county_crop.rds"
url <- paste(base_url, version, file, sep = "/")
sob2 <- tempfile(fileext = ".rds")
download.file(url, sob2, mode = "wb",quiet=TRUE)
sob2 <- readRDS(sob2)
head(tibble(sob2))
```

    ## # A tibble: 6 × 24
    ##   commodity_year state_code state_abbreviation county_code county_name
    ##            <dbl>      <dbl> <chr>                    <dbl> <chr>      
    ## 1           1948          1 AL                          21 CHILTON    
    ## 2           1948          1 AL                          49 DE KALB    
    ## 3           1948          1 AL                          69 HOUSTON    
    ## 4           1948          1 AL                          89 MADISON    
    ## 5           1948          1 AL                         109 PIKE       
    ## 6           1948          1 AL                         125 TUSCALOOSA 
    ## # ℹ 19 more variables: commodity_code <dbl>, commodity_name <chr>,
    ## #   policies_sold <dbl>, policies_earning_prem <dbl>,
    ## #   policies_indemnified <dbl>, units_earning_prem <dbl>,
    ## #   units_indemnified <dbl>, net_reporting_level_amount <dbl>,
    ## #   liability_amount <dbl>, total_premium_amount <dbl>, subsidy_amount <dbl>,
    ## #   indemnity_amount <dbl>, loss_ratio <dbl>, insurance_plan_code <dbl>,
    ## #   insurance_plan_abbreviation <chr>, coverage_type_code <chr>, …

``` r
# Example 3: Accessing COL data via rfcip
col1 <- rfcip::get_col_data(year = 2020)
```

    ## ℹ Locating cause of loss download links on RMA's website.
    ## ✔ Download links located.
    ## ℹ Merging cause of loss files for all specified crop years

``` r
head(tibble(col1))
```

    ## # A tibble: 6 × 30
    ##   commodity_year state_code state_abbrv county_code county_name commodity_code
    ##            <dbl>      <dbl> <chr>             <dbl> <chr>                <dbl>
    ## 1           2020          1 AL                    1 Autauga                 21
    ## 2           2020          1 AL                    1 Autauga                 21
    ## 3           2020          1 AL                    1 Autauga                 21
    ## 4           2020          1 AL                    1 Autauga                 41
    ## 5           2020          1 AL                    1 Autauga                 41
    ## 6           2020          1 AL                    1 Autauga                 41
    ## # ℹ 24 more variables: commodity_name <chr>, insurance_plan_code <dbl>,
    ## #   insurance_plan_abbrv <chr>, delivery_type <chr>, stage_code <chr>,
    ## #   col_code <dbl>, col_name <chr>, month_of_loss_code <dbl>,
    ## #   month_of_loss_name <chr>, year_of_loss <dbl>, policies_earning_prem <dbl>,
    ## #   policies_indemnified <dbl>, net_planted_qty <dbl>,
    ## #   net_endorsed_acres <dbl>, liability <dbl>, total_premium <dbl>,
    ## #   producer_paid_premium <dbl>, subsidy <dbl>, state_subsidy <dbl>, …

``` r
# Example 4: Accessing COL data via this repository’s release assets
file <- "historical_cause_of_loss_premimums_and_indemnities.rds"
url <- paste(base_url, version, file, sep = "/")
col2 <- tempfile(fileext = ".rds")
download.file(url, col2, mode = "wb",quiet=TRUE)
col2 <- readRDS(col2)
head(tibble(col2))
```

    ## # A tibble: 6 × 21
    ##   commodity_year state_code state_abbreviation county_code county_name
    ##            <dbl>      <dbl> <chr>                    <dbl> <chr>      
    ## 1           1948          1 AL                          21 CHILTON    
    ## 2           1948          1 AL                          21 CHILTON    
    ## 3           1948          1 AL                          21 CHILTON    
    ## 4           1948          1 AL                          49 DE KALB    
    ## 5           1948          1 AL                          49 DE KALB    
    ## 6           1948          1 AL                          49 DE KALB    
    ## # ℹ 16 more variables: commodity_code <dbl>, commodity_name <chr>,
    ## #   insurance_plan_code <dbl>, insurance_plan_abbreviation <chr>,
    ## #   coverage_type_code <chr>, col_code <dbl>, col_name <chr>,
    ## #   policies_earning_prem <dbl>, policies_indemnified <dbl>,
    ## #   net_planted_qty <dbl>, liability_amount <dbl>, total_premium_amount <dbl>,
    ## #   subsidy_amount <dbl>, indemnity_amount <dbl>, loss_ratio <chr>,
    ## #   damage_rc <chr>

``` r
# Example 5: Accessing Reinsurance data via rfcip
nationalSRA <- rfcip::nationalSRA
head(tibble(nationalSRA))
```

    ## # A tibble: 6 × 11
    ##   fund_abb reinsurance_year report_geography value_type            dollars
    ##   <chr>               <dbl> <chr>            <chr>                   <dbl>
    ## 1 AR                   1998 NationalFund     gross_liability    3286742595
    ## 2 AR                   1998 NationalFund     gross_premium       313517141
    ## 3 AR                   1998 NationalFund     gross_indemnity     463493075
    ## 4 AR                   1998 NationalFund     retained_liability  597494229
    ## 5 AR                   1998 NationalFund     retained_premium     58503706
    ## 6 AR                   1998 NationalFund     retained_indemnity   51840753
    ## # ℹ 6 more variables: data_release_month <dbl>, data_release_year <dbl>,
    ## #   data_release_day <dbl>, data_release_date <date>, fund_name <chr>,
    ## #   report_type <chr>

## 📚 Citation

If you find this repository useful, please star this project and cite
our papers listed above.

See the [LICENSE](../LICENSE) file in the repository’s root for details.

*Maintained by [ftsiboe](https://github.com/ftsiboe)*
