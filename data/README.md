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

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Datasets

This directory provides guidance on accessing the various datasets used
in U.S. farm safety-net research. Below are descriptions of each dataset
and instructions on how to access them:

## Federal Crop Insurance Program (FCIP) – Summary of Business (SOB)

This dataset provides comprehensive participation information for the
FCIP. It can be accessed via two methods depending on the period of
analysis:

-   **Recent data (1989–present):**  
    Available through the R package
    [**rfcip**](https://github.com/dylan-turner25/rfcip), which offers
    tools to retrieve SOB, COL, and reinsurance data at various levels
    of aggregation.
-   **Historical data (pre-1989):**  
    Download from this repository’s release assets:
    -   [Historical summary of business by state, county, crop, and
        coverage](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/v0.1.0/historical_summary_of_business_by_state_county_crop_coverage.rds)
    -   [Historical summary of business by state, county, and
        crop](https://github.com/ftsiboe/US-FarmSafetyNet-Lab/releases/download/v0.1.0/historical_summary_of_business_by_state_county_crop.rds)

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
head(sob1)
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
head(sob2)
```

    ##   commodity_year state_code state_abbreviation county_code county_name
    ## 1           1948          1                 AL          21     CHILTON
    ## 2           1948          1                 AL          49     DE KALB
    ## 3           1948          1                 AL          69     HOUSTON
    ## 4           1948          1                 AL          89     MADISON
    ## 5           1948          1                 AL         109        PIKE
    ## 6           1948          1                 AL         125  TUSCALOOSA
    ##   commodity_code commodity_name policies_sold policies_earning_prem
    ## 1             21         COTTON           348                   232
    ## 2             21         COTTON          1038                   998
    ## 3             21         COTTON           356                   324
    ## 4             21         COTTON           328                   325
    ## 5             21         COTTON           227                   227
    ## 6             21         COTTON           264                   233
    ##   policies_indemnified units_earning_prem units_indemnified
    ## 1                    0                245                 6
    ## 2                    0               1240               170
    ## 3                    0                334                24
    ## 4                    0                435                22
    ## 5                    0                234                 8
    ## 6                    0                302                 4
    ##   net_reporting_level_amount liability_amount total_premium_amount
    ## 1                       1247            31179                 1495
    ## 2                       7567           496744                18392
    ## 3                       2552           119880                 9247
    ## 4                       8392           462757                17335
    ## 5                       1696            51123                 5116
    ## 6                       2528           128016                10143
    ##   subsidy_amount indemnity_amount loss_ratio insurance_plan_code
    ## 1           1495              154       0.10                  NA
    ## 2          18392            21418       1.16                  NA
    ## 3           9247              905       0.10                  NA
    ## 4          17335             4320       0.25                  NA
    ## 5           5116              344       0.07                  NA
    ## 6          10143              250       0.02                  NA
    ##   insurance_plan_abbreviation coverage_type_code delivery_id
    ## 1                        <NA>               <NA>        <NA>
    ## 2                        <NA>               <NA>        <NA>
    ## 3                        <NA>               <NA>        <NA>
    ## 4                        <NA>               <NA>        <NA>
    ## 5                        <NA>               <NA>        <NA>
    ## 6                        <NA>               <NA>        <NA>
    ##   reporting_level_type endorsed_commodity_reporting_level_amount
    ## 1                 <NA>                                        NA
    ## 2                 <NA>                                        NA
    ## 3                 <NA>                                        NA
    ## 4                 <NA>                                        NA
    ## 5                 <NA>                                        NA
    ## 6                 <NA>                                        NA

``` r
# Example 3: Accessing COL data via rfcip
# rfcip can be installed directly from github using remotes::install_github("https://github.com/dylan-turner25/rfcip")
col1 <- rfcip::get_col_data(year = 2020)
```

    ## ℹ Locating cause of loss download links on RMA's website.
    ## ✔ Download links located.
    ## ℹ Merging cause of loss files for all specified crop years

``` r
head(col1)
```

    ##   commodity_year state_code state_abbrv county_code county_name commodity_code
    ## 1           2020          1          AL           1     Autauga             21
    ## 2           2020          1          AL           1     Autauga             21
    ## 3           2020          1          AL           1     Autauga             21
    ## 4           2020          1          AL           1     Autauga             41
    ## 5           2020          1          AL           1     Autauga             41
    ## 6           2020          1          AL           1     Autauga             41
    ##   commodity_name insurance_plan_code insurance_plan_abbrv delivery_type
    ## 1         Cotton                   2                   RP             A
    ## 2         Cotton                   2                   RP             A
    ## 3         Cotton                   2                   RP             A
    ## 4           Corn                   2                   RP             A
    ## 5           Corn                   2                   RP             A
    ## 6           Corn                   2                   RP             A
    ##   stage_code col_code                           col_name month_of_loss_code
    ## 1          H       31 Excess Moisture/Precipitation/Rain                  9
    ## 2          H       92      Hurricane/Tropical Depression                 10
    ## 3          H       92      Hurricane/Tropical Depression                  9
    ## 4          H        1                   Decline in Price                  9
    ## 5          H       11                            Drought                  7
    ## 6          R       93                           Wildlife                  5
    ##   month_of_loss_name year_of_loss policies_earning_prem policies_indemnified
    ## 1                SEP         2020                     1                    1
    ## 2                OCT         2020                     3                    3
    ## 3                SEP         2020                     3                    3
    ## 4                SEP         2020                     1                    1
    ## 5                JUL         2020                     1                    1
    ## 6                MAY         2020                     1                    1
    ##   net_planted_qty net_endorsed_acres  liability total_premium
    ## 1          38.925                  0  11519.000      1389.000
    ## 2         992.425                  0 495420.000     38585.500
    ## 3         392.600                  0 220061.000     17815.500
    ## 4          12.900                  0   2543.665       437.095
    ## 5           8.100                  0   1597.185       274.455
    ## 6          30.000                  0   5915.500      1016.500
    ##   producer_paid_premium  subsidy state_subsidy addnl_subsidy efa_prem_discount
    ## 1               570.000   819.00             0             0                 0
    ## 2              8457.500 30128.00             0             0                 0
    ## 3              4157.500 13658.00             0             0                 0
    ## 4               196.725   240.37             0             0                 0
    ## 5               123.525   150.93             0             0                 0
    ## 6               457.500   559.00             0             0                 0
    ##   indemnified_quantity indem_amount loss_ratio
    ## 1               38.925      1938.00       1.40
    ## 2              992.425     95015.00       2.46
    ## 3              392.600     18962.00       1.06
    ## 4               25.800      1602.18       3.67
    ## 5               16.200      1006.02       3.67
    ## 6               14.000       439.00       0.43

``` r
# Example 4: Accessing COL data via this repository’s release assets
file <- "historical_cause_of_loss_premimums_and_indemnities.rds"
url <- paste(base_url, version, file, sep = "/")
col2 <- tempfile(fileext = ".rds")
download.file(url, col2, mode = "wb",quiet=TRUE)
col2 <- readRDS(col2)
head(col2)
```

    ##   commodity_year state_code state_abbreviation county_code county_name
    ## 1           1948          1                 AL          21     CHILTON
    ## 2           1948          1                 AL          21     CHILTON
    ## 3           1948          1                 AL          21     CHILTON
    ## 4           1948          1                 AL          49     DE KALB
    ## 5           1948          1                 AL          49     DE KALB
    ## 6           1948          1                 AL          49     DE KALB
    ##   commodity_code commodity_name insurance_plan_code insurance_plan_abbreviation
    ## 1             21         COTTON                  NA                        <NA>
    ## 2             21         COTTON                  NA                        <NA>
    ## 3             21         COTTON                  NA                        <NA>
    ## 4             21         COTTON                  NA                        <NA>
    ## 5             21         COTTON                  NA                        <NA>
    ## 6             21         COTTON                  NA                        <NA>
    ##   coverage_type_code col_code                    col_name policies_earning_prem
    ## 1                          11                     DROUGHT                     0
    ## 2                          71                     INSECTS                     0
    ## 3                          99 OTHER (SNOW-LIGHTNING-ETC.)                     0
    ## 4                          11                     DROUGHT                     0
    ## 5                          31 EXCESS MOISTURE/PRECIP/RAIN                     0
    ## 6                          41                       FROST                     0
    ##   policies_indemnified net_planted_qty liability_amount total_premium_amount
    ## 1                    0               0                0                    0
    ## 2                    0               0                0                    0
    ## 3                    0               0                0                    0
    ## 4                    0               0                0                    0
    ## 5                    0               0                0                    0
    ## 6                    0               0                0                    0
    ##   subsidy_amount indemnity_amount loss_ratio         damage_rc
    ## 1              0               31       0000 Drought/High Temp
    ## 2              0               45       0000            Biotic
    ## 3              0               78       0000             Other
    ## 4              0               44       0000 Drought/High Temp
    ## 5              0               44       0000             Other
    ## 6              0            11733       0000          Low Temp

``` r
# Example 5: Accessing Reinsurance data via rfcip
nationalSRA <- rfcip::nationalSRA
head(nationalSRA)
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
