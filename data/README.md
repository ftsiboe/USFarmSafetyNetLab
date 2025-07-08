Datasets
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

This directory provides guidance on accessing the various datasets used
in U.S. farm safety-net research.

Please note that these datasets are based on gatherings from our past
and ongoing research. They do not constitute an endorsement of any
specific datasets, and we acknowledge that there are many other relevant
datasets that may not be included here.

The datasets available via the `USFarmSafetyNetLab` package include:

- [Contiguous
  county](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/blob/main/data/contiguous_county.rda)
  `USFarmSafetyNetLab::contiguous_county`
- [Index for price
  recived](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/blob/main/data/index_for_price_recived.rda)
  `USFarmSafetyNetLab::index_for_price_recived`
- [Marketing year average
  price](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/blob/main/data/marketing_year_avg_price.rda)
  `USFarmSafetyNetLab::marketing_year_avg_price`
- [RMA commodity type
  recodes](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/blob/main/data/rma_type_recodes.rda)
  `USFarmSafetyNetLab::rma_type_recodes`
- [Contiguous
  county](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/blob/main/data/contiguous_county.rda)
  `USFarmSafetyNetLab::contiguous_county`
- [State rental
  rates](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/blob/main/data/state_rental_rates.rda)
  `USFarmSafetyNetLab::state_rental_rates`

Below are descriptions of each dataset and instructions on how to access
them:

## Federal Crop Insurance Program (FCIP) – Summary of Business (SOB)

This dataset provides comprehensive participation information for the
FCIP. It can be accessed via two methods depending on the period of
analysis:

- **Available through the R package:**
  - [rfcip](https://github.com/dylan-turner25/rfcip), which offers tools
    to retrieve SOB, COL, and reinsurance data at various levels of
    aggregation.
- **Download from this repository’s release assets:**
  - [Summary of Business
    Releases](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/tag/sob)

## Federal Crop Insurance Program (FCIP) – Cause of Loss (COL)

This dataset offers summarized information on FCIP participation,
segmented by different causes of loss. It can be accessed as follows:

- **Available through the R package:**
  - [rfcip](https://github.com/dylan-turner25/rfcip).
- **Download from this repository’s release assets:**
  - [Cause of Loss
    Releases](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/tag/col)

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
sob1 <- rfcip::get_sob_data(year = 2020, crop = "corn")
head(tibble(sob1))
```

    ## # A tibble: 1 × 23
    ##   commodity_year commodity_code commodity_name policies_sold
    ##            <dbl>          <int> <chr>                  <dbl>
    ## 1           2020             41 Corn                  573839
    ## # ℹ 19 more variables: policies_earning_prem <dbl>, policies_indemnified <dbl>,
    ## #   units_earning_prem <dbl>, units_indemnified <dbl>, quantity <dbl>,
    ## #   quantity_type <chr>, companion_endorsed_acres <dbl>, liabilities <dbl>,
    ## #   total_prem <dbl>, subsidy <dbl>, indemnity <dbl>, efa_prem_discount <dbl>,
    ## #   addnl_subsidy <dbl>, state_subsidy <dbl>, pccp_state_matching_amount <dbl>,
    ## #   organic_certified_subsidy_amount <dbl>,
    ## #   organic_transitional_subsidy_amount <dbl>, earn_prem_rate <dbl>, …

``` r
# Example 2: Accessing SOB data via this repository’s release assets
sob2 <- get_data_release(name = "sobcov", year = 2020)
head(tibble(sob2))
```

    ## # A tibble: 6 × 28
    ##   commodity_year state_code state_abbreviation county_code county_name          
    ##            <dbl>      <dbl> <chr>                    <dbl> <chr>                
    ## 1           2020          2 AK                         999 "All Other Counties …
    ## 2           2020          2 AK                         240 "Southeast Fairbanks…
    ## 3           2020          2 AK                         240 "Southeast Fairbanks…
    ## 4           2020          2 AK                         240 "Southeast Fairbanks…
    ## 5           2020          2 AK                         240 "Southeast Fairbanks…
    ## 6           2020          2 AK                         240 "Southeast Fairbanks…
    ## # ℹ 23 more variables: commodity_code <dbl>, commodity_name <chr>,
    ## #   insurance_plan_code <dbl>, insurance_plan_name_abbreviation <chr>,
    ## #   coverage_category <chr>, delivery_type <chr>, coverage_level_percent <dbl>,
    ## #   policies_sold_count <int>, policies_earning_premium_count <int>,
    ## #   policies_indemnified_count <int>, units_earning_premium_count <int>,
    ## #   units_indemnified_count <int>, quantity_type <chr>,
    ## #   net_reported_quantity <int>, endorsed_companion_acres <int>, …

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
col2 <- get_data_release(name = "colsom", year = 2020)
head(tibble(col2))
```

    ## # A tibble: 6 × 30
    ##   commodity_year state_code state_abbreviation county_code county_name          
    ##            <dbl>      <dbl> <chr>                    <dbl> <chr>                
    ## 1           2020          1 AL                           1 "Autauga            …
    ## 2           2020          1 AL                           1 "Autauga            …
    ## 3           2020          1 AL                           1 "Autauga            …
    ## 4           2020          1 AL                           1 "Autauga            …
    ## 5           2020          1 AL                           1 "Autauga            …
    ## 6           2020          1 AL                           1 "Autauga            …
    ## # ℹ 25 more variables: commodity_code <dbl>, commodity_name <chr>,
    ## #   insurance_plan_code <dbl>, insurance_plan_name_abbreviation <chr>,
    ## #   coverage_category <chr>, stage_code <chr>, cause_of_loss_code <chr>,
    ## #   cause_of_loss_description <chr>, month_of_loss <int>,
    ## #   month_of_loss_name <chr>, year_of_loss <int>,
    ## #   policies_earning_premium <int>, policies_indemnified <int>,
    ## #   net_planted_quantity <chr>, net_endorsed_acres <chr>, liability <chr>, …

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
